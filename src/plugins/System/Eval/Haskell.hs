{-# OPTIONS -fglasgow-exts -fffi #-}
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

--
-- compile and run haskell strings at runtime.
--

module System.Eval.Haskell ( 
        eval,
        eval_,
        unsafeEval,
        unsafeEval_,
        typeOf,

        hs_eval_b,      -- return a Bool
        hs_eval_c,      -- return a CChar
        hs_eval_i,      -- return a CInt
        hs_eval_s,      -- return a CString

        module System.Eval.Utils, 

    ) where

import System.Eval.Utils
import System.Plugins.Make
import System.Plugins.Load

import AltData.Dynamic             ( Dynamic )
import AltData.Typeable            ( Typeable )

import Data.Either

import System.IO
import System.Directory

import Foreign.C
import Foreign

--
-- ok. the idea is: the have either installed the library, in which case
-- is is registered, and the path to altdata is known to ghc, so just
-- saying "-package altdata" will work. if not, we search in the build
-- dir just in case. this should work for inplace work.
--
-- TODO could have a few extra package.conf search paths in here,
-- including PREFIX.
--

-- ---------------------------------------------------------------------
-- return a compiled value, and type check it first
--
-- TODO make this faster.
--
eval :: Typeable a => String -> [Import] -> IO (Maybe a)
eval src mods = do
    pwd                <- getCurrentDirectory
    (cmdline,loadpath) <- getPaths
    tmpf               <- mkUniqueWith dynwrap src mods
    status             <- make tmpf cmdline
    m_rsrc <- case status of
        MakeSuccess _ obj -> do 
           m_v <- dynload obj [pwd] loadpath symbol
           case m_v of LoadFailure _      -> return Nothing
                       LoadSuccess _ rsrc -> return $ Just rsrc
        MakeFailure err -> mapM_ putStrLn err >> return Nothing
    makeCleaner tmpf
    return m_rsrc

-- ---------------------------------------------------------------------
-- Version of eval with all the buttons available.
eval_ :: Typeable a =>
         String           -- ^ code to compile
      -> [Import]         -- ^ any imports
      -> [String]         -- ^ extra make flags
      -> [FilePath]       -- ^ (package.confs) for load
      -> [FilePath]       -- ^ include paths load is to search in
      -> IO (Either [String] (Maybe a)) -- ^ either errors, or maybe a well typed value

eval_ src mods args ldflags incs = do 
    pwd                <- getCurrentDirectory
    (cmdline,loadpath) <- getPaths      -- find path to altdata
    tmpf               <- mkUniqueWith dynwrap src mods
    status             <- make tmpf $ ["-Onot"] ++ cmdline ++ args
    m_rsrc <- case status of
        MakeSuccess _ obj -> do 
           m_v <- dynload obj (pwd:incs) (loadpath++ldflags) symbol
           return $ case m_v of LoadFailure e      -> Left e
                                LoadSuccess _ rsrc -> Right (Just rsrc)
        MakeFailure err -> return $ Left err
    makeCleaner tmpf
    return m_rsrc

-- ---------------------------------------------------------------------
-- unsafe because it doesn't use Dynamic types
-- useful for not having to provide type constraints to values, or when
-- you want to easily deal with polymorphic values.
--
unsafeEval :: String -> [Import] -> IO (Maybe a)
unsafeEval src mods = do
    pwd    <- getCurrentDirectory
    tmpf   <- mkUniqueWith wrap src mods
    status <- make tmpf []
    m_rsrc <- case status of
        MakeSuccess _ obj  -> do 
           m_v <- load obj [pwd] [] symbol
           case m_v of LoadFailure _      -> return Nothing
                       LoadSuccess _ rsrc -> return $ Just rsrc
        MakeFailure err -> mapM_ putStrLn err >> return Nothing
    makeCleaner tmpf
    return m_rsrc

--
-- like unsafeEval, except you can supply extra args to make and load,
-- and the error messages are returned too.
--
-- Need  to be able to specify a search path to look in.
--
unsafeEval_ :: String           -- ^ code to compile
            -> [Import]         -- ^ any imports
            -> [String]         -- ^ make flags
            -> [FilePath]       -- ^ (package.confs) for load
            -> [FilePath]       -- ^ include paths load is to search in
            -> IO (Either [String] a)

unsafeEval_ src mods args ldflags incs = do 
    pwd  <- getCurrentDirectory
    tmpf <- mkUniqueWith wrap src mods
    status <- make tmpf args
    e_rsrc <- case status of
        MakeSuccess _ obj  -> do 
           m_v <- load obj (pwd:incs) ldflags symbol
           case m_v of LoadFailure e      -> return $ Left e
                       LoadSuccess _ rsrc -> return $ Right rsrc
        MakeFailure err -> return $ Left err
    makeCleaner tmpf
    return e_rsrc

------------------------------------------------------------------------
--
-- return a compiled value's type, by using Dynamic to get a
-- representation of the inferred type.
--
typeOf :: String -> [Import] -> IO String
typeOf src mods = do
    pwd                <- getCurrentDirectory
    (cmdline,loadpath) <- getPaths
    tmpf               <- mkUniqueWith dynwrap src mods
    status             <- make tmpf cmdline
    ty <- case status of
        MakeSuccess _ obj -> do 
            m_v <- load obj [pwd] loadpath symbol
            case m_v of 
                LoadFailure _              -> return "<failure>"
                LoadSuccess _ (v::Dynamic) -> return $ (init . tail) $ show v

        MakeFailure err -> mapM_ putStrLn err >> return []
    makeCleaner tmpf
    return ty

--
-- note that the wrapper uses our altdata library for dynamic typing.
-- hence it needs to see the path to the altdata package. grr. is it
-- installed or not? what path does it have?
--
dynwrap :: String -> String -> [Import] -> String
dynwrap expr nm mods =
        "module "++nm++ "( resource ) where\n" ++ 
         concatMap (\m-> "import "++m++"\n") mods ++
        "import AltData.Dynamic\n" ++
        "resource = let { v = \n" ++
        "{-# LINE 1 \"<eval>\" #-}\n" ++ expr ++ ";} in toDyn v"

-- ---------------------------------------------------------------------
-- unsafe wrapper
--
wrap :: String -> String -> [Import] -> String
wrap expr nm mods =
        "module "++nm++ "( resource ) where\n" ++ 
        concatMap (\m-> "import "++m++"\n") mods ++
        "resource = let { v = \n" ++
        "{-# LINE 1 \"<Plugins.Eval>\" #-}\n" ++ expr ++ ";} in v"

------------------------------------------------------------------------
--
-- And for our friends in foreign parts
--
-- TODO needs to accept char** to import list
--

--
-- return NULL pointer if an error occured.
--
  
foreign export ccall hs_eval_b  :: CString -> IO (Ptr CInt)
foreign export ccall hs_eval_c  :: CString -> IO (Ptr CChar)
foreign export ccall hs_eval_i  :: CString -> IO (Ptr CInt)
foreign export ccall hs_eval_s  :: CString -> IO CString

------------------------------------------------------------------------
--
-- TODO implement a marshalling for Dynamics, so that we can pass that
-- over to the C side for checking.
--

hs_eval_b :: CString -> IO (Ptr CInt)
hs_eval_b s = do m_v <- eval_cstring s
                 case m_v of Nothing -> return nullPtr 
                             Just v  -> new (fromBool v)

hs_eval_c :: CString -> IO (Ptr CChar)
hs_eval_c s = do m_v <- eval_cstring s
                 case m_v of Nothing -> return nullPtr 
                             Just v  -> new (castCharToCChar v)

-- should be Integral
hs_eval_i :: CString -> IO (Ptr CInt)
hs_eval_i s = do m_v <- eval_cstring s :: IO (Maybe Int)
                 case m_v of Nothing -> return nullPtr 
                             Just v  -> new (fromIntegral v :: CInt)

hs_eval_s :: CString -> IO CString
hs_eval_s s = do m_v <- eval_cstring s
                 case m_v of Nothing -> return nullPtr 
                             Just v  -> newCString v
 
--
-- convenience
--
eval_cstring :: Typeable a => CString -> IO (Maybe a)
eval_cstring cs = do s <- peekCString cs
                     eval s []            -- TODO use eval()

