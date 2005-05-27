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

module System.Plugins.Make ( 

        hasChanged,
        hasChanged',
        recompileAll,
        recompileAll',

        make, 
        makeAll,
        makeWith, 
        MakeStatus(..),
        MakeCode(..),

        merge, 
        mergeTo,
	    mergeToDir,
        MergeStatus(..),
        MergeCode,

        makeClean,
        makeCleaner,

        build, {- internal -}

  ) where

import System.Plugins.Utils
import System.Plugins.Parser
import System.Plugins.LoadTypes        ( Module (Module, path) )
import System.Plugins.Consts           ( ghc, hiSuf, objSuf, hsSuf )
import System.Plugins.Env              ( lookupMerged, addMerge
                                       , getModuleDeps)

#if DEBUG
import System.IO (hFlush, stdout, openFile, IOMode(..),hClose, hPutStr)
#else
import System.IO (openFile, IOMode(..),hClose,hPutStr)
#endif

import System.Directory         ( doesFileExist, removeFile
                                , getModificationTime )

import Control.Exception        ( handleJust )
import GHC.IOBase               ( Exception(IOException) )

#if __GLASGOW_HASKELL__ >= 604
import System.IO.Error          ( isDoesNotExistError )
#endif


------------------------------------------------------------------------
--
-- A better compiler status.
--
data MakeStatus 
        = MakeSuccess MakeCode FilePath 
        | MakeFailure Errors
        deriving (Eq,Show)

data MakeCode = ReComp | NotReq
        deriving (Eq,Show)

------------------------------------------------------------------------
--
-- An equivalent status for the preprocessor (merge)
--
data MergeStatus 
        = MergeSuccess MergeCode Args FilePath 
        | MergeFailure Errors
        deriving (Eq,Show)

type MergeCode = MakeCode

type Args   = [Arg]
type Errors = [String]

--
-- |Returns @True@ if the module or any of its dependencies have older object files than source files.
--  Defaults to @True@ if some files couldn't be located.
--
hasChanged :: Module -> IO Bool
hasChanged = hasChanged' ["hs","lhs"]

hasChanged' :: [String] -> Module -> IO Bool
hasChanged' suffices m@(Module {path = p})
    = do modFile <- doesFileExist p
         mbFile <- findFile suffices p
         case mbFile of
           Just f | modFile
             -> do srcT <- getModificationTime f
                   objT <- getModificationTime p
                   if srcT > objT
                      then return True
                      else do deps <- getModuleDeps m
                              depsStatus <- mapM (hasChanged' suffices) deps
                              return (or depsStatus)
           _ -> return True

--
-- |Same as 'makeAll' but with better recompilation checks since module dependencies are known.
--
recompileAll :: Module -> [Arg] -> IO MakeStatus
recompileAll = recompileAll' ["hs","lhs"]

recompileAll' :: [String] -> Module -> [Arg] -> IO MakeStatus
recompileAll' suffices m args
    = do changed <- hasChanged m
         if changed
            then do mbSource <- findFile suffices (path m)
                    case mbSource of
                      Nothing
                          -> error $ "Couldn't find source for object file: " ++ path m
                      Just source
                          -> makeAll source args
            else return (MakeSuccess NotReq (path m))

-- ---------------------------------------------------------------------
-- | Standard make. Compile a single module, unconditionally. 
-- Behaves like ghc -c
--
make :: FilePath -> [Arg] -> IO MakeStatus
make src args = rawMake src ("-c":args)  True

-- | Recursive make. Compile a module, and its dependencies if we can
-- find them. Takes the top-level file as the first argument.
-- Behaves like ghc --make
--
makeAll :: FilePath -> [Arg] -> IO MakeStatus
makeAll src args = 
    rawMake src ( "--make":"-no-hs-main":"-no-link":"-v0":args ) False

-- | merge two files; then make them. will leave a .o and .hi file in tmpDir.
--      
makeWith :: FilePath                           -- ^ a src file
         -> FilePath                           -- ^ a syntax stub file
         -> [Arg]                              -- ^ any required args
         -> IO MakeStatus                      -- ^ path to an object file

makeWith src stub args = do
    status <- merge src stub
    case status of
        MergeFailure errs -> return $ MakeFailure ("merge failed:\n":errs)
        MergeSuccess _ args' tmpf -> do
                 status' <- rawMake tmpf ("-c": args' ++ args) True
                 return status'

-- ---------------------------------------------------------------------
-- rawMake : really do the compilation
-- Conditional on file modification times, compile a .hs file
-- When using 'make', the name of the src file must be the name of the
-- .o file you are expecting back
--
-- Problem: we use GHC producing stdout to indicate compilation failure.
-- We should instead check the error conditions. I.e. --make will
-- produce output, but of course compiles correctly. TODO
-- So, e.g. --make requires -v0 to stop spurious output confusing
-- rawMake
--
-- Problem :: makeAll incorrectly refuses to recompile if the top level
-- src isn't new.
--

rawMake :: FilePath        -- ^ src
        -> [Arg]           -- ^ any compiler args
        -> Bool            -- ^ do our own recompilation checking
        -> IO MakeStatus

rawMake src args docheck = do
        src_exists <- doesFileExist src
        if not src_exists
                then return $ MakeFailure ["Source file does not exist: "++src]
                else do {
        ; let (obj,_) = outFilePath src args
        ; src_changed <- if docheck then src `newer` obj else return True
        ; if not src_changed
          then return $ MakeSuccess NotReq obj
          else do 
#if DEBUG    
                putStr "Compiling object ... " >> hFlush stdout
#endif
                err <- build src obj args
#if DEBUG    
                putStrLn "done"
#endif
                return $ if null err 
                         then MakeSuccess ReComp obj 
                         else MakeFailure err
        }

--
-- compile a .hs file to a .o file
--
-- If the plugin needs to import an api (which should be almost
-- everyone) then the ghc flags to find the api need to be provided as
-- arguments
--
build :: FilePath          -- path to .hs source
      -> FilePath          -- path to object file
      -> [String]          -- any extra cmd line flags
      -> IO [String]

build src obj extra_opts = do

    let odir = dirname obj -- *always* put the .hi file next to the .o file

    let ghc_opts = [ "-Onot" ]
        output   = [ "-o", obj, "-odir", odir, 
                     "-hidir", odir, "-i" ++ odir ]

    let flags = ghc_opts ++ output ++ extra_opts ++ [src]

#if DEBUG
    -- env.
    putStr $ show $ ghc : flags
#endif
    (_,err) <- exec ghc flags       -- this is a fork()

    obj_exists <- doesFileExist obj -- sanity
    return $ if not obj_exists && null err -- no errors, but no object?
             then ["Compiled, but didn't create object file `"++obj++"'!"]
             else err

-- ---------------------------------------------------------------------
-- | Merge to source files into a temporary file. If we've tried to
-- merge these two stub files before, then reuse the module name (helps
-- recompilation checking)
--
merge :: FilePath -> FilePath -> IO MergeStatus
merge src stb = do 
    m_mod <- lookupMerged src stb
    (out,domerge) <- case m_mod of
                Nothing -> do out <- mkUnique
                              addMerge src stb (dropSuffix out)
                              return (out, True) -- definitely out of date
                Just nm -> return $ (nm <> hsSuf, False)
    rawMerge src stb out domerge

-- | Merge to source files and store them in the specified output file,
-- instead of a temp file as merge does.
--
mergeTo :: FilePath -> FilePath -> FilePath -> IO MergeStatus
mergeTo src stb out = rawMerge src stb out False

mergeToDir :: FilePath -> FilePath -> FilePath -> IO MergeStatus
mergeToDir src stb dir = do
	out <- mkUniqueIn dir
	rawMerge src stb out True

-- ---------------------------------------------------------------------
-- Conditional on file modification times, merge a src file with a
-- syntax stub file into a result file.
--
-- Merge should only occur if the srcs has changed since last time.
-- Parser errors result in MergeFailure, and are reported to the client
--
-- Also returns a list of cmdline flags found in pragmas in the src of
-- the files. This last feature exists as OPTION pragmas aren't handled
-- (for obvious reasons, relating to the implementation of OPTIONS
-- parsing in GHC) by the library parser, and, also, we want a way for
-- the user to introduce *dynamic* cmd line flags in the .conf file.
-- This is achieved via the GLOBALOPTIONS pragma : an extension to ghc
-- pragma syntax
--
rawMerge :: FilePath -> FilePath -> FilePath -> Bool -> IO MergeStatus
rawMerge src stb out always_merge = do
    src_exists <- doesFileExist src
    stb_exists <- doesFileExist stb
    case () of {_
        | not src_exists  -> return $ 
                MergeFailure ["Source file does not exist : "++src]
        | not stb_exists -> return $ 
                MergeFailure ["Source file does not exist : "++stb]
        | otherwise -> do {
    
    ;do_merge <- do src_changed <- src `newer` out
                    stb_changed <- stb `newer` out
                    return $ src_changed || stb_changed

    ;if not do_merge && not always_merge
     then return $ MergeSuccess NotReq [] out
     else do
        src_str <- readFile src
        stb_str <- readFile stb

        let (a,a') = parsePragmas src_str
            (b,b') = parsePragmas stb_str
            opts = a ++ a' ++ b ++ b'

        let e_src_syn = parse src src_str
            e_stb_syn = parse stb stb_str
    
        -- check if there were parser errors
        case (e_src_syn,e_stb_syn) of
                (Left e,  _)       -> return $ MergeFailure [e]
                (_ , Left e)       -> return $ MergeFailure [e]
                (Right src_syn, Right stb_syn) -> do {

        ;let mrg_syn = mergeModules src_syn stb_syn
             mrg_syn'= replaceModName mrg_syn (mkModid $ basename out)
             mrg_str = pretty mrg_syn'

        ;hdl <- openFile out WriteMode  -- overwrite!
        ;hPutStr hdl mrg_str ; hClose hdl
        ;return $ MergeSuccess ReComp opts out -- must have recreated file
    }}}

-- ---------------------------------------------------------------------
-- | makeClean : assuming we some element of [f.hs,f.hi,f.o], remove the
-- .hi and .o components. Silently ignore any missing components. *Does
-- not remove .hs files*. To do that use makeCleaner. This would be
-- useful for merged files, for example.
--
makeClean :: FilePath -> IO ()
makeClean f = let f_hi = dropSuffix  f <> hiSuf
                  f_o  = dropSuffix  f <> objSuf
              in mapM_ rm_f [f_hi, f_o]

makeCleaner :: FilePath -> IO ()
makeCleaner f = makeClean f >> rm_f (dropSuffix f <> hsSuf)
           
-- internal:
--      try to remove a file, ignoring if it didn't exist in the first place
-- Doesn't seem to be able to remove all files in all circumstances, why?
--
rm_f f = handleJust doesntExist (\_->return ()) (removeFile f)
    where
        doesntExist (IOException ioe)
                | isDoesNotExistError ioe = Just ()
                | otherwise               = Nothing
        doesntExist _ = Nothing

