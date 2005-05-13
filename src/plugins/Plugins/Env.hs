{-# OPTIONS -cpp #-}
--
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

module Plugins.Env (
        withModEnv,
        withPkgEnvs,
        withMerged,
        modifyModEnv,
        modifyPkgEnv,
        modifyMerged,
        addModule,
        rmModule,
        addModules,
        isLoaded,
        loaded,
        isMerged,
        lookupMerged,
        addMerge,
        addPkgConf,
        union,
        grabDefaultPkgConf,
        readPackageConf,
        lookupPkg

   ) where

#include "../../../config.h"

import Plugins.PackageAPI  {- everything -}
#if CABAL == 1 || __GLASGOW_HASKELL__ >= 604
import Plugins.ParsePkgConfCabal( parsePkgConf )
#else
import Plugins.ParsePkgConfLite ( parsePkgConf )
#endif
import Plugins.Consts           ( ghcLibraryPath, sysPkgConf, sysPkgSuffix, dllSuf )

import Data.IORef               ( writeIORef, readIORef, newIORef, IORef() )
import Data.Maybe               ( isJust )
import Data.List                ( isPrefixOf, nub )

import System.IO.Unsafe         ( unsafePerformIO )
import System.Directory         ( doesFileExist )
#if defined(CYGWIN) || defined(__MINGW32__)
import System.Environment       ( getEnv )

import Control.Monad            ( liftM )
#endif

import Control.Concurrent.MVar  ( MVar(), newMVar, withMVar )

#if __GLASGOW_HASKELL__ < 604
import Data.FiniteMap

#else
import qualified Data.Map as M

--
-- and map Data.Map terms to FiniteMap terms
--
type FiniteMap k e = M.Map k e

emptyFM :: FiniteMap key elt
emptyFM   = M.empty

addToFM :: (Ord key) => FiniteMap key elt -> key -> elt -> FiniteMap key elt
addToFM   = \m k e -> M.insert k e m

delFromFM :: (Ord key) => FiniteMap key elt -> key -> FiniteMap key elt
delFromFM = flip M.delete

lookupFM :: (Ord key) => FiniteMap key elt -> key -> Maybe elt
lookupFM  = flip M.lookup

#endif

--
-- We need to record what modules and packages we have loaded, so if we
-- read a .hi file that wants to load something already loaded, we can
-- safely ignore that request. We're in the IO monad anyway, so we can
-- add some extra state of our own.
--
-- The state is a FiniteMap String Bool (a hash of package/object names
-- to whether they have been loaded or not). 
--
-- It also contains the package.conf information, so that if there is a
-- package dependency we can find it correctly, even if it has a
-- non-standard path or name, and if it isn't an official package (but
-- rather one provided via -package-conf). This is stored as a 
-- FiniteMap PackageName PackageConfig. The problem then is whether a
-- user's package.conf, that uses the same package name as an existing
-- GHC package, should be allowed, or should shadow a library package?
-- I don't know, but I'm inclined to have the GHC package shadow the
-- user's package.
--
-- This idea is based on *Hampus Ram's dynamic loader* dependency
-- tracking system. He uses state to record dependency trees to allow
-- clean unloading and other fun. This is quite cool. We're just using
-- state to make sure we don't load the same package twice. Implementing
-- the full dependency tree idea would be nice, though not fully
-- necessary as we have the dependency information store in .hi files,
-- unlike in hram's loader.
--

type ModEnv = FiniteMap String Bool

-- represents a package.conf file
type PkgEnv  = FiniteMap PackageName PackageConfig

-- record dependencies between (src,stub) -> merged modid
type MergeEnv = FiniteMap (FilePath,FilePath) FilePath

-- multiple package.conf's kept in separate namespaces
type PkgEnvs = [PkgEnv]

type Env = (MVar (), 
            IORef ModEnv,  
            IORef PkgEnvs,
            IORef MergeEnv)

--
-- our environment, contains a set of loaded objects, and a map of known
-- packages and their informations. Initially all we know is the default
-- package.conf information.
--
env = unsafePerformIO $ do 
                mvar  <- newMVar ()
                ref1  <- newIORef emptyFM         -- loaded objects
                p     <- grabDefaultPkgConf
                ref2  <- newIORef p               -- package.conf info
                ref3  <- newIORef emptyFM         -- merged files
                return (mvar, ref1, ref2, ref3)
{-# NOINLINE env #-}

-- -----------------------------------------------------------
--
-- apply 'f' to the loaded objects Env
-- apply 'f' to the package.conf FM
-- *locks up the MVar* so you can't recursively call a function inside a
-- with*Env function. Nice and threadsafe
--
withModEnv  :: Env -> (ModEnv   -> IO a) -> IO a
withPkgEnvs :: Env -> (PkgEnvs  -> IO a) -> IO a
withMerged  :: Env -> (MergeEnv -> IO a) -> IO a

withModEnv  (mvar,ref,_,_) f = withMVar mvar (\_ -> readIORef ref >>= f)
withPkgEnvs (mvar,_,ref,_) f = withMVar mvar (\_ -> readIORef ref >>= f)
withMerged  (mvar,_,_,ref) f = withMVar mvar (\_ -> readIORef ref >>= f)

-- -----------------------------------------------------------
--
-- write an object name
-- write a new PackageConfig
--
modifyModEnv :: Env -> (ModEnv   -> IO ModEnv)  -> IO ()
modifyPkgEnv :: Env -> (PkgEnvs  -> IO PkgEnvs) -> IO ()
modifyMerged :: Env -> (MergeEnv -> IO MergeEnv)-> IO ()

modifyModEnv (mvar,ref,_,_) f = lockAndWrite mvar ref f 
modifyPkgEnv (mvar,_,ref,_) f = lockAndWrite mvar ref f 
modifyMerged (mvar,_,_,ref) f = lockAndWrite mvar ref f

-- private
lockAndWrite mvar ref f = withMVar mvar (\_->readIORef ref>>=f>>=writeIORef ref)

-- -----------------------------------------------------------
--
-- insert a loaded module name into the environment
--
addModule :: String -> IO ()
addModule s = modifyModEnv env $ \fm -> return $ addToFM fm s True

--
-- remove a module name from the environment
--
rmModule :: String -> IO ()
rmModule s = modifyModEnv env $ \fm -> return $ delFromFM fm s

--
-- insert a list of module names all in one go
--
addModules :: [String] -> IO ()
addModules ns = modifyModEnv env $ \fm -> return $ unionL fm ns 
    where
        unionL :: ModEnv -> [String] -> ModEnv 
        unionL fm ss = foldr (\s fm' -> addToFM fm' s True) fm ss

--
-- is a module/package already loaded?
--
isLoaded :: String -> IO Bool
isLoaded s = withModEnv env $ \fm -> return $ isJust (lookupFM fm s)

--
-- confusing! only for filter.
--
loaded :: String -> IO Bool
loaded m = do t <- isLoaded m ; return (not t)

-- -----------------------------------------------------------
-- Package management stuff
--
-- insert a single package.conf (containing multiple configs)
-- means: create a new FM. insert packages into FM. add FM to end of
-- list of FM stored in the environment.
--
addPkgConf :: FilePath -> IO ()
addPkgConf f = do 
    ps <- readPackageConf f
    modifyPkgEnv env $ \ls -> return $ union ls ps

--
-- add a new FM for the package.conf to the list of existing ones
--
union :: PkgEnvs -> [PackageConfig] -> PkgEnvs
union ls ps' = 
        let fm = emptyFM -- new FM for this package.conf
        in ls ++ [foldr (\p fm' -> addToFM fm' (packageName p) p) fm ps']

-- 
-- generate a PkgEnv from the system package.conf
-- * the path to the default package.conf was determined by ./configure *
-- This imposes a constraint that you must build your plugins with the
-- same ghc you use to build hs-plugins. This is reasonable, we feel.
--

grabDefaultPkgConf :: IO PkgEnvs
grabDefaultPkgConf = do
        pkgs <- readPackageConf $ ghcLibraryPath </> sysPkgConf
        return $ union [] pkgs

--
-- parse a source file, expanding any $libdir we see.
--
readPackageConf :: FilePath -> IO [PackageConfig]
readPackageConf f = do
        s <- readFile f
        let p = parsePkgConf s
        return $! map expand_libdir p

  where
      expand_libdir :: PackageConfig -> PackageConfig
      expand_libdir pk =
        let pk'   = updImportDirs  (\idirs -> map expand idirs) pk
            pk''  = updLibraryDirs (\ldirs -> map expand ldirs) pk'
        in  pk''

      expand :: FilePath -> FilePath
      expand s | "$libdir" `isPrefixOf` s = ghcLibraryPath ++ drop 7 s
      expand s = s


--
-- Package path, given a package name, look it up in the environment and
-- return the path to all the libraries needed to load this package.
--
-- What do we need to load? With the library_dirs as prefix paths:
--      * anything in the hs_libraries fields, $libdir expanded
--      * anything in the extra_libraries fields (i.e. cbits), expanded,
--      which includes system .so files. Ignore these for now
--      * also load any dependencies now, because of that weird mtl
--      library that lang depends upon, but which doesn't show up in the
--      interfaces for some reason.
--
-- ToDo At present this does not handle extra_libraries correctly.  It
-- only find those extra libraries that live in the directory specfied
-- by the library_dirs field of the package.conf entry. But
-- extra_libraries can contain any libraries supported by the system's
-- linker. For this library they must be, of course, be dynamic.  The
-- extensions for such libraries are different on various platforms.
-- This would need to be checked for by configure.ac.  (Scary - dons)
--
-- We return all the package paths that possibly exist, and the leave it
-- up to loadObject not to load the same ones twice...
--
lookupPkg :: PackageName -> IO ([FilePath],[FilePath])
lookupPkg p = do 
        t <- lookupPkg' p
        case t of ([],(f,g)) -> return (f,g)
                  (ps,(f,g)) -> do gss <- mapM lookupPkg ps
				   let (f',g') = unzip gss
				   return $ (nub $ (concat f') ++ f,nub $ (concat g') ++ g)

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

            Just package -> do
#if defined(CYGWIN) || defined(__MINGW32__)
                let    libdirs = fix_topdir $ libraryDirs package
#else
                let    libdirs = libraryDirs package
#endif
                       hslibs  = hsLibraries package
                       extras  = extraLibraries package
                       deppkgs = packageDeps package
                libs <- mapM (findHSlib libdirs) (hslibs ++ extras)
#if defined(CYGWIN) || defined(__MINGW32__)
		syslibdir <- liftM ( \x -> x ++ "/SYSTEM") (getEnv "SYSTEMROOT")
		libs' <- mapM (findDLL $ syslibdir : libdirs) extras
#else
		libs' <- mapM (findDLL libdirs) extras
#endif
                -- don't care if there are 'Nothings', that usually
                -- means that they refer to system libraries. Can't do
                -- anything about that.
                return (deppkgs, (filterJust libs,filterJust libs') )

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
        filterJust :: [Maybe a] -> [a]
        filterJust []           = []
        filterJust (Just x:xs)  = x:filterJust xs
        filterJust (Nothing:xs) =   filterJust xs

        --
        -- Check that a path to a library actually reaches a library
        -- Problem: sysPkgSuffix  is ".o", but extra libraries could be
        -- ".so" -- what to do?
        --
        findHSlib :: [FilePath] -> String -> IO (Maybe FilePath)
        findHSlib [] _  = return Nothing
        findHSlib (dir:dirs) lib = do
                  let l = dir </> lib ++ sysPkgSuffix
                  b <- doesFileExist l
                  if b then return $ Just l     -- found it!
                       else findHSlib dirs lib

        findDLL :: [FilePath] -> String -> IO (Maybe FilePath)
	findDLL [] _           = return Nothing
	findDLL (dir:dirs) lib = do
		 let l = dir </> lib ++ dllSuf
		 b <- doesFileExist l
		 if b then return $ Just l
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
