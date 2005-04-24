{-# OPTIONS -cpp -fglasgow-exts #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
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
-- Based on code from $fptools/ghc/compiler/utils/FastMutInt.lhs
--
-- (c) Copyright 2002, The University Court of the University of Glasgow. 

--
-- Unboxed mutable Ints
--

module Hi.FastMutInt (
        FastMutInt, 
        newFastMutInt,
        readFastMutInt, 
        writeFastMutInt,
        incFastMutInt, 
        incFastMutIntBy
    ) where

#include "MachDeps.h"

#if __GLASGOW_HASKELL__ < 503
import GlaExts
import PrelIOBase
#else
import GHC.Base
import GHC.IOBase
#endif

#if __GLASGOW_HASKELL__ < 411
newByteArray# = newCharArray#
#endif

data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt :: IO FastMutInt
newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where I# size = SIZEOF_HSINT

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }

incFastMutInt :: FastMutInt -> IO Int	-- Returns original value
incFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  case writeIntArray# arr 0# (i +# 1#) s of { s ->
  (# s, I# i #) } }

incFastMutIntBy :: FastMutInt -> Int -> IO Int	-- Returns original value
incFastMutIntBy (FastMutInt arr) (I# n) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  case writeIntArray# arr 0# (i +# n) s of { s ->
  (# s, I# i #) } }

