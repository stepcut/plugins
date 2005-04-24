{-# OPTIONS -fglasgow-exts -fffi -cpp #-}
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

--
-- compile and run haskell strings at runtime.
--

module Eval.Utils ( 

        Import,
        symbol,
        escape,
        getPaths,
        find_altdata_pkgconf,

        mkUniqueWith,
        cleanup,

        module Data.Maybe,
        module Control.Monad,

    ) where

import Plugins.Load                ( Symbol )
import Plugins.Utils
import Plugins.Consts              ( top {- :{ -} )

import System.IO
import System.Directory

import Data.Char

-- 
-- we export these so that eval() users have a nice time
--
import Data.Maybe 
import Control.Monad

--
-- imports Foo's
--
type Import = String

--
-- distinguished symbol name
--
symbol :: Symbol
symbol = "resource"

--
-- turn a Haskell string into a printable version of the same string
--
escape s = concatMap (\c -> showLitChar c $ "") s

--
-- For Dynamic eval's, work out the compile and load command lines
--
getPaths :: IO ([String],[String])
getPaths = do
        m_pkg <- find_altdata_pkgconf
        let load_path = if isJust m_pkg then fromJust m_pkg else []
        let make_line = 
                let compulsory = ["-Onot","-fglasgow-exts","-package","altdata"]
                in if not $ null load_path 
                          then "-package-conf":load_path:compulsory
                          else compulsory
        let load_path' = if null load_path then [] else [load_path]
        return (make_line,load_path')

-- ---------------------------------------------------------------------
-- if we are in-tree eval() needs to use the inplace package.conf to
-- find altdata, otherwise we need it to be in the ghc package system.
--
-- fixing Typeable/Dynamic in ghc obsoletes this code. as would adding
-- an extra param to eval, which I don't want to do.
--
find_altdata_pkgconf :: IO (Maybe String)
find_altdata_pkgconf = do
        let f = top </> "plugins.conf.inplace"
        b <- doesFileExist f
        return $ if b 
                 then Just f 
                 else Nothing

-- ---------------------------------------------------------------------
-- create the tmp file, and write source into it, using wrapper to
-- create extra .hs src.
--
mkUniqueWith :: (String -> String -> [Import] -> String) 
             -> String 
             -> [Import] -> IO FilePath

mkUniqueWith wrapper src mods = do
        (tmpf,hdl) <- hMkUnique
        let nm   = mkModid (basename tmpf)       -- used as a module name
            src' = wrapper src nm mods
        hPutStr hdl src' >> hFlush hdl >> hClose hdl >> return tmpf

--
-- remove all the tmp files
--
cleanup :: String -> String -> IO ()
cleanup a b = mapM_ removeFile [a, b, replaceSuffix b ".hi"]

