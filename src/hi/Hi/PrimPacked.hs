{-# OPTIONS -cpp -fglasgow-exts #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-matches #-}

{-# OPTIONS -#include "hschooks.h" #-}

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
-- Based on $fptools/ghc/compiler/utils/PrimPacked.lhs
--
-- (c) The GRASP/AQUA Project, Glasgow University, 1997-1998
--
--
-- Basic ops on packed representations
--
-- Some basic operations for working on packed representations of series
-- of bytes (character strings). Used by the interface lexer input
-- subsystem, mostly.

{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module Hi.PrimPacked (
	Ptr(..), nullPtr, plusAddr#,
	BA(..),
	packString, 	   -- :: String -> (Int, BA)
	unpackNBytesBA,    -- :: BA -> Int -> [Char]
        strLength,	   -- :: Ptr CChar -> Int
        copyPrefixStr,	   -- :: Addr# -> Int -> BA
        copySubStrBA,	   -- :: BA -> Int -> Int -> BA
        eqStrPrefix,	   -- :: Addr# -> ByteArray# -> Int# -> Bool
        eqStrPrefixBA,	   -- :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
 ) where

import Foreign
import GHC.Exts
import GHC.ST

-- Wrapper types for bytearrays

data BA    = BA  ByteArray#
data MBA s = MBA (MutableByteArray# s)

packString :: String -> (Int, BA)
packString str = (l, arr)
 where
  l@(I# length#) = length str

  arr = runST (do
    ch_array <- new_ps_array length#
      -- fill in packed string from "str"
    fill_in ch_array 0# str
      -- freeze the puppy:
    freeze_ps_array ch_array length#
   )

  fill_in :: MBA s -> Int# -> [Char] -> ST s ()
  fill_in arr_in# idx [] =
   return ()
  fill_in arr_in# idx (C# c : cs) =
   write_ps_array arr_in# idx c	 >>
   fill_in arr_in# (idx +# 1#) cs

-- Unpacking a string

unpackNBytesBA :: BA -> Int -> [Char]
unpackNBytesBA (BA bytes) (I# len)
 = unpack 0#
 where
    unpack nh
      | nh >=# len  = []
      | otherwise   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharArray# bytes nh

-- Copying a char string prefix into a byte array.

copyPrefixStr :: Addr# -> Int -> BA
copyPrefixStr a# len@(I# length#) = copy' length#
 where
   copy' length# = runST (do
     {- allocate an array that will hold the string
     -}
     ch_array <- new_ps_array length#
     {- Revert back to Haskell-only solution for the moment.
     	_ccall_ memcpy ch_array (A# a) len        >>=  \ () ->
     	write_ps_array ch_array length# (chr# 0#) >>
     -}
     -- fill in packed string from "addr"
     fill_in ch_array 0#
     -- freeze the puppy:
     freeze_ps_array ch_array length#
    )

   fill_in :: MBA s -> Int# -> ST s ()
   fill_in arr_in# idx
      | idx ==# length#
      = return ()
      | otherwise
      = case (indexCharOffAddr# a# idx) of { ch ->
	write_ps_array arr_in# idx ch >>
	fill_in arr_in# (idx +# 1#) }

-- Copying out a substring, assume a 0-indexed string:
-- (and positive lengths, thank you).

copySubStrBA :: BA -> Int -> Int -> BA
copySubStrBA (BA barr#) (I# start#) len@(I# length#) = ba
 where
  ba = runST (do
     -- allocate an array that will hold the string
    ch_array <- new_ps_array length#
     -- fill in packed string from "addr"
    fill_in ch_array 0#
     -- freeze the puppy:
    freeze_ps_array ch_array length#
   )

  fill_in :: MBA s -> Int# -> ST s ()
  fill_in arr_in# idx
      | idx ==# length#
      = return ()
      | otherwise
      = case (indexCharArray# barr# (start# +# idx)) of { ch ->
	write_ps_array arr_in# idx ch >>
	fill_in arr_in# (idx +# 1#) }

-- (Very :-) ``Specialised'' versions of some CharArray things...
-- [Copied from PackBase; no real reason -- UGH]

new_ps_array	:: Int# -> ST s (MBA s)
write_ps_array	:: MBA s -> Int# -> Char# -> ST s () 
freeze_ps_array :: MBA s -> Int# -> ST s BA

#if __GLASGOW_HASKELL__ < 411
#define NEW_BYTE_ARRAY newCharArray#
#else 
#define NEW_BYTE_ARRAY newByteArray#
#endif

new_ps_array size = ST $ \ s ->
    case (NEW_BYTE_ARRAY size s)  of { (# s2#, barr# #) ->
    (# s2#, MBA barr# #) }

write_ps_array (MBA barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    (# s2#, () #) }

-- same as unsafeFreezeByteArray
freeze_ps_array (MBA arr#) len# = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, BA frozen# #) }

-- Compare two equal-length strings for equality:

eqStrPrefix :: Addr# -> ByteArray# -> Int# -> Bool
eqStrPrefix a# barr# len# = 
  unsafePerformIO $ do
   x <- memcmp_ba a# barr# (I# len#)
   return (x == 0)

eqStrPrefixBA :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
eqStrPrefixBA b1# b2# start# len# = 
  unsafePerformIO $ do
    x <- memcmp_baoff_ba b2# (I# start#) b1# (I# len#)
    return (x == 0)

------------------------------------------------------------------------
-- in hschooks
--

foreign import ccall unsafe "plugin_strlen"
        strLength :: Ptr () -> Int

foreign import ccall unsafe "plugin_memcmp"
        memcmp_ba :: Addr# -> ByteArray# -> Int -> IO Int

foreign import ccall unsafe "plugin_memcmp_off"
        memcmp_baoff_ba :: ByteArray# -> Int -> ByteArray# -> Int -> IO Int

