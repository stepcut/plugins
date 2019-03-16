{-# LANGUAGE CPP #-}
--
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
--

module System.Plugins.Env (
        env,
        withModEnv,
        withDepEnv,
        withPkgEnvs,
        withMerged,
        modifyModEnv,
        modifyDepEnv,
        modifyPkgEnv,
        modifyMerged,
        addModule,
        rmModule,
        addModules,
        isLoaded,
        loaded,
        addModuleDeps,
        getModuleDeps,
        rmModuleDeps,
        isMerged,
        lookupMerged,
        addMerge,
        addPkgConf,
        defaultPkgConf,
        union,
        addStaticPkg,
        isStaticPkg,
        rmStaticPkg,
        grabDefaultPkgConf,
        readPackageConf,
        lookupPkg,
        pkgManglingPrefix

   ) where

#include "config.h"

import System.Plugins.LoadTypes (Module)
import System.Plugins.Consts           ( sysPkgSuffix )

import Control.Monad            ( liftM )

import Data.IORef               ( writeIORef, readIORef, newIORef, IORef() )
import Data.Maybe               ( isJust, isNothing, fromMaybe )
import Data.List                ( (\\), nub )

import System.IO.Unsafe         ( unsafePerformIO )
import System.Directory         ( doesFileExist )
#if defined(CYGWIN) || defined(__MINGW32__)
import Prelude hiding ( catch, ioError )
import System.IO.Error          ( catch, ioError, isDoesNotExistError )
#endif

import Control.Concurrent.MVar  ( MVar(), newMVar, withMVar )

import GHC.Paths (libdir)
import DynFlags (
#if MIN_VERSION_ghc(7,8,0)
  Way(WayDyn), dynamicGhc, ways,
#endif
  defaultDynFlags, initDynFlags)
import SysTools (initSysTools, initLlvmConfig)

import Distribution.Package hiding (
#if MIN_VERSION_ghc(7,6,0)
                                     Module,
#endif
                                     depends, packageName, PackageName(..)
                                   , installedUnitId
#if MIN_VERSION_ghc(7,10,0)
                                   , installedPackageId
#endif
  )
import Distribution.Text

import Distribution.InstalledPackageInfo
import Distribution.Simple.Compiler
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Verbosity

import System.Environment
import Data.List.Split

import qualified Data.Map as M
import qualified Data.Set as S
--
-- and map Data.Map terms to FiniteMap terms
--
type FiniteMap k e = M.Map k e

emptyFM :: FiniteMap key elt
emptyFM   = M.empty

addToFM :: (Ord key) => FiniteMap key elt -> key -> elt -> FiniteMap key elt
addToFM   = \m k e -> M.insert k e m

addWithFM :: (Ord key)
          => (elt -> elt -> elt) -> FiniteMap key elt -> key -> elt -> FiniteMap key elt
addWithFM   = \comb m k e -> M.insertWith comb k e m

delFromFM :: (Ord key) => FiniteMap key elt -> key -> FiniteMap key elt
delFromFM = flip M.delete

lookupFM :: (Ord key) => FiniteMap key elt -> key -> Maybe elt
lookupFM  = flip M.lookup

--
-- | We need to record what modules and packages we have loaded, so if
-- we read a .hi file that wants to load something already loaded, we
-- can safely ignore that request. We're in the IO monad anyway, so we
-- can add some extra state of our own.
--
-- The state is a FiniteMap String (Module,Int) (a hash of
-- package\/object names to Modules and how many times they've been
-- loaded).
--
-- It also contains the package.conf information, so that if there is a
-- package dependency we can find it correctly, even if it has a
-- non-standard path or name, and if it isn't an official package (but
-- rather one provided via -package-conf). This is stored as a FiniteMap
-- PackageName PackageConfig. The problem then is whether a user's
-- package.conf, that uses the same package name as an existing GHC
-- package, should be allowed, or should shadow a library package?  I
-- don't know, but I'm inclined to have the GHC package shadow the
-- user's package.
--
-- This idea is based on /Hampus Ram's dynamic loader/ dependency
-- tracking system. He uses state to record dependency trees to allow
-- clean unloading and other fun. This is quite cool. We're just using
-- state to make sure we don't load the same package twice. Implementing
-- the full dependency tree idea would be nice, though not fully
-- necessary as we have the dependency information store in .hi files,
-- unlike in hram's loader.
--

type ModEnv = FiniteMap String (Module,Int)

type DepEnv = FiniteMap Module [Module]

-- represents a package.conf file
type PkgEnv  = FiniteMap PackageName PackageConfig

type StaticPkgEnv = S.Set PackageName

-- record dependencies between (src,stub) -> merged modid
type MergeEnv = FiniteMap (FilePath,FilePath) FilePath

-- multiple package.conf's kept in separate namespaces
type PkgEnvs = [PkgEnv]

type Env = (MVar (),
            IORef ModEnv,
            IORef DepEnv,
            IORef PkgEnvs,
            IORef StaticPkgEnv,
            IORef MergeEnv)


--
-- our environment, contains a set of loaded objects, and a map of known
-- packages and their informations. Initially all we know is the default
-- package.conf information.
--
env = unsafePerformIO $ do
                mvar  <- newMVar ()
                ref1  <- newIORef emptyFM         -- loaded objects
                ref2  <- newIORef emptyFM
                p     <- grabDefaultPkgConf
                ref3  <- newIORef p               -- package.conf info
                ref4  <- newIORef (S.fromList ["base","Cabal","haskell-src", "containers",
                                               "arrays", "directory", "random", "process",
                                               "ghc", "ghc-prim"])
                ref5  <- newIORef emptyFM         -- merged files
                return (mvar, ref1, ref2, ref3, ref4, ref5)
{-# NOINLINE env #-}

-- -----------------------------------------------------------
--
-- | apply 'f' to the loaded objects Env, apply 'f' to the package.conf
-- FM /locks up the MVar/ so you can't recursively call a function
-- inside a with any -Env function. Nice and threadsafe
--
withModEnv  :: Env -> (ModEnv   -> IO a) -> IO a
withDepEnv  :: Env -> (DepEnv   -> IO a) -> IO a
withPkgEnvs :: Env -> (PkgEnvs  -> IO a) -> IO a
withStaticPkgEnv :: Env -> (StaticPkgEnv -> IO a) -> IO a
withMerged  :: Env -> (MergeEnv -> IO a) -> IO a

withModEnv  (mvar,ref,_,_,_,_) f = withMVar mvar (\_ -> readIORef ref >>= f)
withDepEnv  (mvar,_,ref,_,_,_) f = withMVar mvar (\_ -> readIORef ref >>= f)
withPkgEnvs (mvar,_,_,ref,_,_) f = withMVar mvar (\_ -> readIORef ref >>= f)
withStaticPkgEnv (mvar,_,_,_,ref,_) f = withMVar mvar (\_ -> readIORef ref >>= f)
withMerged  (mvar,_,_,_,_,ref) f = withMVar mvar (\_ -> readIORef ref >>= f)

-- -----------------------------------------------------------
--
-- write an object name
-- write a new PackageConfig
--
modifyModEnv :: Env -> (ModEnv   -> IO ModEnv)  -> IO ()
modifyDepEnv :: Env -> (DepEnv   -> IO DepEnv)  -> IO ()
modifyPkgEnv :: Env -> (PkgEnvs  -> IO PkgEnvs) -> IO ()
modifyStaticPkgEnv :: Env -> (StaticPkgEnv  -> IO StaticPkgEnv) -> IO ()
modifyMerged :: Env -> (MergeEnv -> IO MergeEnv)-> IO ()

modifyModEnv (mvar,ref,_,_,_,_) f = lockAndWrite mvar ref f
modifyDepEnv (mvar,_,ref,_,_,_) f = lockAndWrite mvar ref f
modifyPkgEnv (mvar,_,_,ref,_,_) f = lockAndWrite mvar ref f
modifyStaticPkgEnv (mvar,_,_,_,ref,_) f = lockAndWrite mvar ref f
modifyMerged (mvar,_,_,_,_,ref) f = lockAndWrite mvar ref f

-- private
lockAndWrite mvar ref f = withMVar mvar (\_->readIORef ref>>=f>>=writeIORef ref)

-- -----------------------------------------------------------
--
-- | insert a loaded module name into the environment
--
addModule :: String -> Module -> IO ()
addModule s m = modifyModEnv env $ \fm -> let c = maybe 0 snd (lookupFM fm s)
                                          in return $ addToFM fm s (m,c+1)

--getModule :: String -> IO (Maybe Module)
--getModule s = withModEnv env $ \fm -> return (lookupFM fm s)

--
-- | remove a module name from the environment. Returns True if the
-- module was actually removed.
--
rmModule :: String -> IO Bool
rmModule s = do modifyModEnv env $ \fm -> let c = maybe 1 snd (lookupFM fm s)
                                              fm' = delFromFM fm s
                                          in if c-1 <= 0
                                                then return fm'
                                                else return fm
                withModEnv env $ \fm -> return (isNothing  (lookupFM fm s))

--
-- | insert a list of module names all in one go
--
addModules :: [(String,Module)] -> IO ()
addModules ns = mapM_ (uncurry addModule) ns

--
-- | is a module\/package already loaded?
--
isLoaded :: String -> IO Bool
isLoaded s = withModEnv env $ \fm -> return $ isJust (lookupFM fm s)

--
-- confusing! only for filter.
--
loaded :: String -> IO Bool
loaded m = do t <- isLoaded m ; return (not t)

-- -----------------------------------------------------------
--
-- module dependency stuff
--

--
-- | Set the dependencies of a Module.
--
addModuleDeps :: Module -> [Module] -> IO ()
addModuleDeps m deps = modifyDepEnv env $ \fm -> return $ addToFM fm m deps

--
-- | Get module dependencies. Nothing if none have been recored.
--
getModuleDeps :: Module -> IO [Module]
getModuleDeps m = withDepEnv env $ \fm -> return $ fromMaybe [] (lookupFM fm m)


--
-- | Unrecord a module from the environment.
--
rmModuleDeps :: Module -> IO ()
rmModuleDeps m = modifyDepEnv env $ \fm -> return $ delFromFM fm m

-- -----------------------------------------------------------
-- Package management stuff

--
-- | Insert a single package.conf (containing multiple configs) means:
-- create a new FM. insert packages into FM. add FM to end of list of FM
-- stored in the environment.
--
addPkgConf :: FilePath -> IO ()
addPkgConf f = do
    ps <- readPackageConf f
    modifyPkgEnv env $ \ls -> return $ union ls ps

-- | This function is required when running with stack.
defaultPkgConf :: IO ()
defaultPkgConf = do
  paths <- lookupEnv "GHC_PACKAGE_PATH"
  unsetEnv "GHC_PACKAGE_PATH"
  case paths of
    Nothing -> return ()
    Just s -> mapM_ addPkgConf $ splitOn ":" s

--
-- | add a new FM for the package.conf to the list of existing ones; if a package occurs multiple
-- times, pick the one with the higher version number as the default (e.g., important for base in
-- GHC 6.12)
--
union :: PkgEnvs -> [PackageConfig] -> PkgEnvs
union ls ps' =
        let fm = emptyFM -- new FM for this package.conf
        in foldr addOnePkg fm ps' : ls
    where
      -- we add each package with and without it's version number and with the full installedPackageId
      addOnePkg p fm' = addToPkgEnvs (addToPkgEnvs (addToPkgEnvs fm' (display $ sourcePackageId p) p) (display $ installedPackageId p) p)
                                     (packageName p) p

      -- if no version number specified, pick the higher version
      addToPkgEnvs = addWithFM higherVersion

      higherVersion pkgconf1 pkgconf2
        | installedPackageId pkgconf1 >= installedPackageId pkgconf2 = pkgconf1
        | otherwise                                                  = pkgconf2

--
-- | generate a PkgEnv from the system package.conf
-- The path to the default package.conf was determined by /configure/
-- This imposes a constraint that you must build your plugins with the
-- same ghc you use to build hs-plugins. This is reasonable, we feel.
--

grabDefaultPkgConf :: IO PkgEnvs
grabDefaultPkgConf = do
        pc <- configureAllKnownPrograms silent defaultProgramConfiguration
#if MIN_VERSION_Cabal(1,24,0)
        (compiler, _platform, _programConfiguration)
           <- configure silent Nothing Nothing pc
        pkgIndex <- getInstalledPackages silent compiler
                        [GlobalPackageDB, UserPackageDB] pc
#else
        pkgIndex <- getInstalledPackages silent [GlobalPackageDB, UserPackageDB] pc
#endif
        return $ [] `union` allPackages pkgIndex

--
-- parse a source file, expanding any $libdir we see.
--
readPackageConf :: FilePath -> IO [PackageConfig]
readPackageConf f = do
    pc <- configureAllKnownPrograms silent defaultProgramConfiguration
#if MIN_VERSION_Cabal(1,24,0)
    (compiler, _platform, _programConfiguration)
       <- configure silent Nothing Nothing pc
    pkgIndex <- getInstalledPackages silent compiler [GlobalPackageDB, UserPackageDB, SpecificPackageDB f] pc
#else
    pkgIndex <- getInstalledPackages silent [GlobalPackageDB, UserPackageDB, SpecificPackageDB f] pc
#endif
    return $ allPackages pkgIndex

-- -----------------------------------------------------------
-- Static package management stuff. A static package is linked with the base
-- application and we should therefore not link with any of the DLLs it requires.

addStaticPkg :: PackageName -> IO ()
addStaticPkg pkg = modifyStaticPkgEnv env $ \set -> return $ S.insert pkg set

isStaticPkg :: PackageName -> IO Bool
isStaticPkg pkg = withStaticPkgEnv env $ \set -> return $ S.member pkg set

rmStaticPkg :: String -> IO Bool
rmStaticPkg pkg = do
  (willRemove, s) <- withStaticPkgEnv env $ \s -> return (S.member pkg s, s)
  if not willRemove then return False
    else do modifyStaticPkgEnv env $ \s' -> return $ S.delete pkg s'
            return True
--
-- Package path, given a package name, look it up in the environment and
-- return the path to all the libraries needed to load this package.
--
-- What do we need to load? With the library_dirs as prefix paths:
--      . anything in the hs_libraries fields, libdir expanded
--
--      . anything in the extra_libraries fields (i.e. cbits), expanded,
--
--      which includes system .so files.
--
--      . also load any dependencies now, because of that weird mtl
--      library that lang depends upon, but which doesn't show up in the
--      interfaces for some reason.
--
-- We return all the package paths that possibly exist, and the leave it
-- up to loadObject not to load the same ones twice...
--
lookupPkg :: PackageName -> IO ([FilePath],[FilePath])
lookupPkg pn = go [] pn
    where
      go :: [PackageName] -> PackageName -> IO ([FilePath],[FilePath])
      go seen p = do
        (ps, (f, g)) <- lookupPkg' p
        static <- if not (null f) && null g
                  then addStaticPkg p >> return True
                  else isStaticPkg p
        (f', g') <- liftM unzip $ mapM (go (nub $ seen ++ ps)) (ps \\ seen)
        return $ (nub $ (concat f') ++ f, if static then [] else nub $ (concat g') ++ g)

-- This is the prefix of mangled symbols that come from this package.
pkgManglingPrefix :: PackageName -> IO (Maybe String)
-- base seems to be mangled differently!
pkgManglingPrefix "base" = return $ Just "base"
pkgManglingPrefix p = withPkgEnvs env $ \fms -> return (go fms p)
    where
        go [] _       = Nothing
        go (fm:fms) q = case lookupFM fm q of
            Nothing -> go fms q     -- look in other pkgs
            Just pkg -> Just $ drop 2 $ getHSLibraryName $ installedUnitId pkg

data LibrarySpec
   = DLL String         -- -lLib
   | DLLPath FilePath   -- -Lpath

classifyLdInput :: FilePath -> IO (Maybe LibrarySpec)
classifyLdInput ('-':'l':lib) = return (Just (DLL lib))
classifyLdInput ('-':'L':path) = return (Just (DLLPath path))
classifyLdInput _ = return Nothing

-- TODO need to define a MAC\/DARWIN symbol
#if defined(MACOSX)
mkSOName root = "lib" ++ root ++ ".dylib"
#elif defined(CYGWIN) || defined(__MINGW32__)
-- Win32 DLLs have no .dll extension here, because addDLL tries
-- both foo.dll and foo.drv
mkSOName root = root
#else
mkSOName root = "lib" ++ root ++ ".so"
#endif

#if defined(MACOSX)
mkDynPkgName root = mkSOName (root ++ "_dyn")
#else
mkDynPkgName root = mkSOName root
#endif

data HSLib = Static FilePath | Dynamic FilePath

--
-- return any stuff to load for this package, plus the list of packages
-- this package depends on. which includes stuff we have to then load
-- too.
--
lookupPkg' :: PackageName -> IO ([PackageName],([FilePath],[FilePath]))
lookupPkg' p = withPkgEnvs env $ \fms -> go fms p
    where
        go [] _       = return ([],([],[]))
        go (fm:fms) q = case lookupFM fm q of
            Nothing -> go fms q     -- look in other pkgs

            Just pkg -> do
                let    hslibs  = hsLibraries pkg
                       extras' = extraLibraries pkg
                       cbits   = filter (\e -> reverse (take (length "_cbits") (reverse e)) == "_cbits") extras'
                       extras  = filter (flip notElem cbits) extras'
                       ldopts  = ldOptions pkg
                       deppkgs = packageDeps pkg
                ldInput <- mapM classifyLdInput ldopts
                let ldOptsLibs  = [ path | Just (DLL path) <- ldInput ]
                    ldOptsPaths = [ path | Just (DLLPath path) <- ldInput ]
                    dlls        = map mkSOName (extras ++ ldOptsLibs)
#if defined(CYGWIN) || defined(__MINGW32__)
                    libdirs = fix_topdir (libraryDirs pkg) ++ ldOptsPaths ++ fix_topdir (libraryDynDirs pkg)
#else
                    libdirs = libraryDirs pkg ++ ldOptsPaths ++ libraryDynDirs pkg
#endif
                -- If we're loading dynamic libs we need the cbits to appear before the
                -- real packages.
                settings <- initSysTools (Just libdir)
                llvmConfig <- initLlvmConfig (Just libdir)
                dflags <- initDynFlags $ defaultDynFlags settings llvmConfig
                libs <- mapM (findHSlib
#if MIN_VERSION_ghc(7,8,0)
                              (WayDyn `elem` ways dflags || dynamicGhc)
#else
                              False
#endif
                              libdirs)
                             (cbits ++ hslibs)
#if defined(CYGWIN) || defined(__MINGW32__)
                windowsos <- catch (getEnv "OS")
                           (\e -> if isDoesNotExistError e then return "Windows_98" else ioError e)
                windowsdir <-
                    if windowsos == "Windows_9X" -- I don't know Windows 9X has OS system variable
                      then return "C:/windows"
                      else return "C:/winnt"
                sysroot <- catch (getEnv "SYSTEMROOT")
                           (\e -> if isDoesNotExistError e then return windowsdir else ioError e) -- guess at a reasonable default
                let syslibdir = sysroot ++ (if windowsos == "Windows_9X" then "/SYSTEM" else "/SYSTEM32")
                libs' <- mapM (findDLL $ syslibdir : libdirs) dlls
#else
                libs' <- mapM (findDLL libdirs) dlls
#endif
                let slibs = [ lib | Right (Static lib)  <- libs ]
                    dlibs = [ lib | Right (Dynamic lib) <- libs ]
                return (deppkgs, (slibs,map (either id id) libs' ++ dlibs) )

#if defined(CYGWIN) || defined(__MINGW32__)
        -- replace $topdir
        fix_topdir []        = []
        fix_topdir (x:xs)    = replace_topdir x : fix_topdir xs

        replace_topdir []           = []
        replace_topdir ('$':xs)
            | take 6 xs == "topdir" = ghcLibraryPath ++ (drop 6 xs)
            | otherwise             = '$' : replace_topdir xs
        replace_topdir (x:xs)       = x : replace_topdir xs
#endif
        -- a list elimination form for the Maybe type
        --filterRight :: [Either left right] -> [right]
        --filterRight []           = []
        --filterRight (Right x:xs) = x:filterRight xs
        --filterRight (Left _:xs)  =   filterRight xs

        --
        -- Check that a path to a library actually reaches a library
        findHSlib' :: [FilePath] -> String -> IO (Maybe FilePath)
        findHSlib' [] _           = return Nothing
        findHSlib' (dir:dirs) lib = do
                  let l = dir </> lib
                  b <- doesFileExist l
                  if b then return $ Just l     -- found it!
                       else findHSlib' dirs lib

        findHSslib dirs lib = findHSlib' dirs $ "lib" ++ lib ++ sysPkgSuffix
        findHSdlib dirs lib = findHSlib' dirs $ mkDynPkgName lib

        findHSlib :: Bool -> [FilePath] -> String -> IO (Either String HSLib)
        findHSlib dynonly dirs lib = do
            -- Problem: sysPkgSuffix  is ".a", but system packages could be dynamic, and
            -- extra dynamic libraries could be needed even when using normal (static) linkage.
            -- Solution: look for dynamic libraries only if using -dynamic; otherwise, use static
            -- and add any other dynamic libraries found.
            dl <- findHSdlib dirs lib
            rdl <- case dl of
                  Just file -> return $ Right $ Dynamic file
                  Nothing   -> do
                      -- TODO Generate this suffix automatically. It's absurd we have to use the preprocessor.
                      dynamicSuffix <- findHSdlib dirs (lib ++ "-ghc" ++ (reverse $ takeWhile (/= '-') $ reverse GHC_LIB_PATH))
                      case dynamicSuffix of
                             Just file -> return $ Right $ Dynamic file
                             Nothing   -> return $ Left lib

            if dynonly then return rdl else do
              rsl <- findHSslib dirs lib
              return $ case rsl of
                Just file -> Right $ Static file
                Nothing   -> rdl

        findDLL :: [FilePath] -> String -> IO (Either String FilePath)
        findDLL [] lib         = return (Left lib)
        findDLL (dir:dirs) lib = do
                 let l = dir </> lib
                 b <- doesFileExist l
                 if b then return $ Right l
                      else findDLL dirs lib

------------------------------------------------------------------------
-- do we have a Module name for this merge?
--
isMerged :: FilePath -> FilePath -> IO Bool
isMerged a b = withMerged env $ \fm -> return $ isJust (lookupFM fm (a,b))

lookupMerged :: FilePath -> FilePath -> IO (Maybe FilePath)
lookupMerged a b = withMerged env $ \fm -> return $ lookupFM fm (a,b)

--
-- insert a new merge pair into env
--
addMerge :: FilePath -> FilePath -> FilePath -> IO ()
addMerge a b z = modifyMerged env $ \fm -> return $ addToFM fm (a,b) z

------------------------------------------------------------------------
-- break a module cycle
-- private:
--
(</>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b


------------------------------------------------------------------------

--
-- We export an abstract interface to package conf`s because we have
-- to handle either traditional or Cabal style package conf`s.
--



packageName    :: PackageConfig -> PackageName
packageDeps    :: PackageConfig -> [PackageName]
-- updImportDirs  :: ([FilePath] -> [FilePath]) -> PackageConfig -> PackageConfig
-- updLibraryDirs :: ([FilePath] -> [FilePath]) -> PackageConfig -> PackageConfig


type PackageName = String

type PackageConfig = InstalledPackageInfo

packageName = display . pkgName . sourcePackageId
-- packageName_ = pkgName . sourcePackageId
packageDeps = (map display) . depends

{-
updImportDirs f pk@(InstalledPackageInfo { importDirs = idirs }) =
        pk { importDirs = f idirs }
updLibraryDirs f pk@(InstalledPackageInfo { libraryDirs = ldirs }) =
        pk { libraryDirs = f ldirs }
-}
