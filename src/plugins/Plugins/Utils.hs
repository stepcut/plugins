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

#include "../../../config.h"

module Plugins.Utils ( 
    Arg,

    hWrite,

    mkUnique,
    hMkUnique,
    mkUniqueIn,
    hMkUniqueIn,

    mkTemp, mkTempIn, {- internal -}

    replaceSuffix,
    outFilePath,
    dropSuffix,
    mkModid,
 
    isSublistOf,                -- :: Eq a => [a] -> [a] -> Bool

    dirname,
    basename,

    (</>), (<.>), (<+>), (<>),

    newer,

    encode,
    decode,
    EncodedString,

    exec,
    panic

  ) where

import Plugins.Env              ( isLoaded )
import Plugins.Consts           ( objSuf, hiSuf, tmpDir )
import qualified Plugins.MkTemp ( mkstemps )

import Data.Char
import Data.List

import System.IO
import System.Environment       ( getEnv )
import System.Directory

--
-- The fork library
--
#if CABAL == 0 && __GLASGOW_HASKELL__ < 604
import POpen                    ( popen )
import System.Posix.Process     ( getProcessStatus )
#else
import System.Process
#endif

-- ---------------------------------------------------------------------
-- some misc types we use

type Arg = String

-- ---------------------------------------------------------------------
-- | useful
--
panic s = ioError ( userError s )

-- ---------------------------------------------------------------------
-- | writeFile for Handles
--
hWrite :: Handle -> String -> IO ()
hWrite hdl src = hPutStr hdl src >> hClose hdl >> return ()


-- ---------------------------------------------------------------------
-- | mkstemps.
--
-- We use the Haskell version now... it is faster than calling into
-- mkstemps(3).
--

mkstemps :: String -> Int -> IO (String,Handle)
mkstemps path slen = do
        m_v <- Plugins.MkTemp.mkstemps path slen
        case m_v of Nothing -> error "mkstemps : couldn't create temp file"
                    Just v' -> return v'

{-

mkstemps path slen = do 
    withCString path $ \ ptr -> do
        let c_slen = fromIntegral $ slen+1
        fd   <- throwErrnoIfMinus1 "mkstemps" $ c_mkstemps ptr c_slen
        name <- peekCString ptr
        hdl  <- fdToHandle fd
        return (name, hdl)

foreign import ccall unsafe "mkstemps" c_mkstemps :: CString -> CInt -> IO Fd

-}

-- ---------------------------------------------------------------------
-- | create a new temp file, returning name and handle.
-- bit like the mktemp shell utility
--
mkTemp :: IO (String,Handle)
mkTemp = do tmpd  <- catch (getEnv "TMPDIR") (\_ -> return tmpDir)
            mkTempIn tmpd

mkTempIn :: String -> IO (String, Handle)
mkTempIn tmpd = do
        (tmpf,hdl)  <- mkstemps (tmpd++"/MXXXXXXXXX.hs") 3
        let modname = mkModid $ dropSuffix tmpf
        if and $ map (\c -> isAlphaNum c && c /= '_') modname
                then return (tmpf,hdl)
                else panic $ "Illegal characters in temp file: `"++tmpf++"'"

-- ---------------------------------------------------------------------
-- | Get a new temp file, unique from those in /tmp, and from those
-- modules already loaded. Very nice for merge/eval uses.
--
-- Will run for a long time if we can't create a temp file, luckily
-- mkstemps gives us a pretty big search space
--
mkUnique :: IO FilePath
mkUnique = do (t,h) <- hMkUnique
              hClose h >> return t

hMkUnique :: IO (FilePath,Handle)
hMkUnique = do (t,h) <- mkTemp
               alreadyLoaded <- isLoaded t -- not unique!
               if alreadyLoaded 
                        then hClose h >> removeFile t >> hMkUnique
                        else return (t,h)

mkUniqueIn :: FilePath -> IO FilePath
mkUniqueIn dir = do (t,h) <- hMkUniqueIn dir
		    hClose h >> return t

hMkUniqueIn :: FilePath -> IO (FilePath,Handle)
hMkUniqueIn dir = do (t,h) <- mkTempIn dir
                     alreadyLoaded <- isLoaded t -- not unique!
                     if alreadyLoaded 
                        then hClose h >> removeFile t >> hMkUniqueIn dir
                        else return (t,h)

-- ---------------------------------------------------------------------
--
-- | execute a command and it's arguments, returning the
-- (stdout,stderr), waiting for it to exit, too.
--

exec :: String -> [String] -> IO ([String],[String])

#if CABAL == 1 || __GLASGOW_HASKELL__ >= 604
--
-- Use the forkProcess library
--
exec prog args = do
    (_,outh,errh,proc_hdl) <- runInteractiveProcess prog args Nothing Nothing   
    b <- waitForProcess proc_hdl    -- wait
    out <- hGetContents outh
    err <- hGetContents errh
    case b of
        _exit_status -> return ( lines $ out, lines $ err )

#else 
--
-- 6.2.2 Posix version.
--
exec prog args = do
    (out,err,pid) <- popen prog args Nothing
    b <- getProcessStatus True False pid  -- wait
    case b of    
        Nothing -> return ([], ["process `"++prog++"' has disappeared"])
        _       -> return ( lines $! out, lines $! err )
#endif

-- ---------------------------------------------------------------------
-- some filename manipulation stuff

--
-- | </>, <.> : join two path components
--
infixr 6 </>
infixr 6 <.>

(</>), (<.>), (<+>), (<>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

[] <.> b = b
a  <.> b = a ++ "." ++ b

[] <+> b = b
a  <+> b = a ++ " " ++ b

[] <> b = b
a  <> b = a ++ b

--
-- | dirname : return the directory portion of a file path
-- if null, return "."
--
dirname :: FilePath -> FilePath
dirname p  = 
    case reverse $ dropWhile (/= '/') $ reverse p of
        [] -> "."
        p' -> p'

--
-- | basename : return the filename portion of a path
--
basename :: FilePath -> FilePath
basename p = reverse $ takeWhile (/= '/') $ reverse p

--
-- drop suffix
--
dropSuffix :: FilePath -> FilePath
dropSuffix f = reverse . tail . dropWhile (/= '.') $ reverse f

--
-- | work out the mod name from a filepath
mkModid :: String -> String
mkModid = (takeWhile (/= '.')) . reverse . (takeWhile (/= '/')) . reverse

-- | return the object file, given the .conf file
-- i.e. /home/dons/foo.rc -> /home/dons/foo.o
--
-- we depend on the suffix we are given having a lead '.'
--
replaceSuffix :: FilePath -> String -> FilePath
replaceSuffix [] _ = [] -- ?
replaceSuffix f suf = 
    case reverse $ dropWhile (/= '.') $ reverse f of
        [] -> f  ++ suf                 -- no '.' in file name
        f' -> f' ++ tail suf

--
-- Normally we create the .hi and .o files next to the .hs files.
-- For some uses this is annoying (i.e. true EDSL users don't actually
-- want to know that their code is compiled at all), and for hmake-like
-- applications. 
--
-- This code checks if "-o foo" or "-odir foodir" are supplied as args
-- to make(), and if so returns a modified file path, otherwise it
-- uses the source file to determing the path to where the object and
-- .hi file will be put.
--
outFilePath :: FilePath -> [Arg] -> (FilePath,FilePath)
outFilePath src args =
    let objs  = find_o args -- user sets explicit object path
        paths = find_p args -- user sets a directory to put stuff in
    in case () of { _
        | not (null objs)
        -> let obj = last objs in (obj, mk_hi obj)

        | not (null paths)
        -> let obj = last paths </> mk_o (basename src) in (obj, mk_hi obj)

        | otherwise
        -> (mk_o src, mk_hi src)
    }
    where 
          outpath = "-o"
          outdir  = "-odir"

          mk_hi s = replaceSuffix s hiSuf
          mk_o  s = replaceSuffix s objSuf

          find_o [] = []
          find_o (f:f':fs) | f == outpath = [f']
                           | otherwise    = find_o $! f':fs
          find_o _ = []

          find_p [] = []
          find_p (f:f':fs) | f == outdir  = [f']
                           | otherwise    = find_p $! f':fs
          find_p _ = []

------------------------------------------------------------------------

--
-- | is file1 newer than file2?
--
-- needs some fixing to work with 6.0.x series. (is this true?)
--
-- fileExist still seems to throw exceptions on some platforms: ia64 in
-- particular.
--
-- invarient : we already assume the first file, 'a', exists
--
newer :: FilePath -> FilePath -> IO Bool
newer a b = do
    a_t      <- getModificationTime a
    b_exists <- doesFileExist b
    if not b_exists
        then return True                -- needs compiling
        else do b_t <- getModificationTime b
                return ( a_t > b_t )    -- maybe need recompiling

------------------------------------------------------------------------
--
-- | return the Z-Encoding of the string.
--
-- Stolen from GHC. Use -package ghc as soon as possible
--
type EncodedString = String

encode :: String -> EncodedString
encode []     = []
encode (c:cs) = encode_ch c ++ encode cs

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'

--
-- Decode is used for user printing.
--
decode :: EncodedString -> String
decode [] = []
decode ('Z' : d : rest) | isDigit d = decode_tuple   d rest
                        | otherwise = decode_upper   d : decode rest
decode ('z' : d : rest) | isDigit d = decode_num_esc d rest
                        | otherwise = decode_lower   d : decode rest
decode (c  : rest) = c : decode rest

decode_upper, decode_lower :: Char -> Char

decode_upper 'L' = '('
decode_upper 'R' = ')'
decode_upper 'M' = '['
decode_upper 'N' = ']'
decode_upper 'C' = ':'
decode_upper 'Z' = 'Z'
decode_upper ch  = error $ "decode_upper can't handle this char `"++[ch]++"'"
            
decode_lower 'z' = 'z'
decode_lower 'a' = '&'
decode_lower 'b' = '|'
decode_lower 'c' = '^'
decode_lower 'd' = '$'
decode_lower 'e' = '='
decode_lower 'g' = '>'
decode_lower 'h' = '#'
decode_lower 'i' = '.'
decode_lower 'l' = '<'
decode_lower 'm' = '-'
decode_lower 'n' = '!'
decode_lower 'p' = '+'
decode_lower 'q' = '\''
decode_lower 'r' = '\\'
decode_lower 's' = '/'
decode_lower 't' = '*'
decode_lower 'u' = '_'
decode_lower 'v' = '%'
decode_lower ch  = error $ "decode_lower can't handle this char `"++[ch]++"'"

-- Characters not having a specific code are coded as z224U
decode_num_esc :: Char -> [Char] -> String
decode_num_esc d cs
  = go (digitToInt d) cs
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go n ('U' : rest)           = chr n : decode rest
    go _ other = error $
        "decode_num_esc can't handle this: \""++other++"\""


encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = [c]     -- Common case first

-- Constructors
encode_ch '('  = "ZL"   -- Needed for things like (,), and (->)
encode_ch ')'  = "ZR"   -- For symmetry with (
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"
encode_ch 'Z'  = "ZZ"

-- Variables
encode_ch 'z'  = "zz"
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '_'  = "zu"
encode_ch '%'  = "zv"
encode_ch c    = 'z' : shows (ord c) "U"

decode_tuple :: Char -> EncodedString -> String
decode_tuple d cs
  = go (digitToInt d) cs
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ['T']          = "()"
    go n ['T']          = '(' : replicate (n-1) ',' ++ ")"
    go 1 ['H']          = "(# #)"
    go n ['H']          = '(' : '#' : replicate (n-1) ',' ++ "#)"
    go _ other = error $ "decode_tuple \'"++other++"'"

-- ---------------------------------------------------------------------

--
-- 'isSublistOf' takes two arguments and returns 'True' iff the first
-- list is a sublist of the second list. This means that the first list
-- is wholly contained within the second list. Both lists must be
-- finite.

isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf [] _ = True
isSublistOf _ [] = False
isSublistOf x y@(_:ys)
    | isPrefixOf x y = True
    | otherwise      = isSublistOf x ys

