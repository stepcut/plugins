{-# OPTIONS -cpp -fglasgow-exts #-}
{-# OPTIONS -fno-warn-unused-imports -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-unused-matches -fno-warn-unused-binds   #-}
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

-- Based on $fptools/ghc/compiler/utils/Binary.hs:
-- (c) The University of Glasgow 2002
--
-- Binary I/O library, with special tweaks for GHC
--
-- Based on the nhc98 Binary library, which is copyright
-- (c) Malcolm Wallace and Colin Runciman, University of York, 1998.
-- Under the terms of the license for that software, we must tell you
-- where you can obtain the original version of the Binary library, namely
--     http://www.cs.york.ac.uk/fp/nhc98/
--
-- We never have to write stuff, so I've scrubbed all the put* code.
--

module Hi.Binary ( 
    {-type-}  Bin,
    {-class-} Binary(..),
    {-type-}  BinHandle,

   openBinIO, openBinIO_,
   openBinMem,
--   closeBin,

   seekBin,
   tellBin,
   castBin,

   readBinMem,

   isEOFBin,

   -- for writing instances:
   getByte,

   -- lazy Bin I/O
   lazyGet,

   -- GHC only:
   ByteArray(..),
   getByteArray,

   getBinFileWithDict,	-- :: Binary a => FilePath -> IO a

  ) where

-- The *host* architecture version:
#include "MachDeps.h"

-- import Hi.Utils -- ?

import Hi.FastMutInt
import Hi.FastString

#if __GLASGOW_HASKELL__ < 604
import Data.FiniteMap
#else
import qualified Data.Map as M
#endif 

import Data.Unique

import Data.Array.IO
import Data.Array
import Data.Bits
import Data.Int
import Data.Word
import Data.IORef
import Data.Char		( ord, chr )
import Data.Array.Base  	( unsafeRead, unsafeWrite )
import Control.Monad		( when )
import System.IO
import System.IO.Unsafe		( unsafeInterleaveIO )
import System.IO.Error		( mkIOError, eofErrorType )
import GHC.Real			( Ratio(..) )
import GHC.Exts
import GHC.IOBase	 	( IO(..) )
import GHC.Word			( Word8(..) )
#if __GLASGOW_HASKELL__ < 601
import GHC.Handle		( openFileEx, IOModeEx(..) )
#endif

#if __GLASGOW_HASKELL__ < 601
openBinaryFile f mode = openFileEx f (BinaryMode mode)
#endif

type BinArray = IOUArray Int Word8

---------------------------------------------------------------
--		BinHandle
---------------------------------------------------------------

data BinHandle
  = BinMem {		-- binary data stored in an unboxed array
     bh_usr :: UserData,	-- sigh, need parameterized modules :-)
     off_r :: !FastMutInt,		-- the current offset
     sz_r  :: !FastMutInt,		-- size of the array (cached)
     arr_r :: !(IORef BinArray) 	-- the array (bounds: (0,size-1))
    }
	-- XXX: should really store a "high water mark" for dumping out
	-- the binary data to a file.

  | BinIO {		-- binary data stored in a file
     bh_usr :: UserData,
     off_r :: !FastMutInt,		-- the current offset (cached)
     hdl   :: !Handle		-- the file handle (must be seekable)
   }
	-- cache the file ptr in BinIO; using hTell is too expensive
	-- to call repeatedly.  If anyone else is modifying this Handle
	-- at the same time, we'll be screwed.

getUserData :: BinHandle -> UserData
getUserData bh = bh_usr bh

setUserData :: BinHandle -> UserData -> BinHandle
setUserData bh us = bh { bh_usr = us }


---------------------------------------------------------------
--		Bin
---------------------------------------------------------------

newtype Bin a = BinPtr Int 
  deriving (Eq, Ord, Show, Bounded)

castBin :: Bin a -> Bin b
castBin (BinPtr i) = BinPtr i

---------------------------------------------------------------
--		class Binary
---------------------------------------------------------------

class Binary a where
    get    :: BinHandle -> IO a

getAt  :: Binary a => BinHandle -> Bin a -> IO a
getAt bh p = do seekBin bh p; get bh

openBinIO_ :: Handle -> IO BinHandle
openBinIO_ h = openBinIO h 

openBinIO :: Handle -> IO BinHandle
openBinIO h = do
  r <- newFastMutInt
  writeFastMutInt r 0
  return (BinIO noUserData r h)

openBinMem :: Int -> IO BinHandle
openBinMem size
 | size <= 0 = error "Hi.Binary.openBinMem: size must be >= 0"
 | otherwise = do
   arr <- newArray_ (0,size-1)
   arr_r <- newIORef arr
   ix_r <- newFastMutInt
   writeFastMutInt ix_r 0
   sz_r <- newFastMutInt
   writeFastMutInt sz_r size
   return (BinMem noUserData ix_r sz_r arr_r)

tellBin :: BinHandle -> IO (Bin a)
tellBin (BinIO  _ r _)   = do ix <- readFastMutInt r; return (BinPtr ix)
tellBin (BinMem _ r _ _) = do ix <- readFastMutInt r; return (BinPtr ix)

seekBin :: BinHandle -> Bin a -> IO ()
seekBin (BinIO _ ix_r h) (BinPtr p) = do 
  writeFastMutInt ix_r p
  hSeek h AbsoluteSeek (fromIntegral p)
seekBin h@(BinMem _ ix_r sz_r a) (BinPtr p) = do
  sz <- readFastMutInt sz_r
  if (p >= sz)
	then do expandBin h p; writeFastMutInt ix_r p
	else writeFastMutInt ix_r p

isEOFBin :: BinHandle -> IO Bool
isEOFBin (BinMem _ ix_r sz_r a) = do
  ix <- readFastMutInt ix_r
  sz <- readFastMutInt sz_r
  return (ix >= sz)
isEOFBin (BinIO _ ix_r h) = hIsEOF h

readBinMem :: FilePath -> IO BinHandle
-- Return a BinHandle with a totally undefined State
readBinMem filename = do
  h <- openBinaryFile filename ReadMode
  filesize' <- hFileSize h
  let filesize = fromIntegral filesize'
  arr <- newArray_ (0,filesize-1)
  count <- hGetArray h arr filesize
  when (count /= filesize)
	(error ("Hi.Binary.readBinMem: only read " ++ show count ++ " bytes"))
  hClose h
  arr_r <- newIORef arr
  ix_r <- newFastMutInt
  writeFastMutInt ix_r 0
  sz_r <- newFastMutInt
  writeFastMutInt sz_r filesize
  return (BinMem noUserData ix_r sz_r arr_r)

-- expand the size of the array to include a specified offset
expandBin :: BinHandle -> Int -> IO ()
expandBin (BinMem _ ix_r sz_r arr_r) off = do
   sz <- readFastMutInt sz_r
   let sz' = head (dropWhile (<= off) (iterate (* 2) sz))
   arr <- readIORef arr_r
   arr' <- newArray_ (0,sz'-1)
   sequence_ [ unsafeRead arr i >>= unsafeWrite arr' i
 	     | i <- [ 0 .. sz-1 ] ]
   writeFastMutInt sz_r sz'
   writeIORef arr_r arr'
#ifdef DEBUG
   hPutStrLn stderr ("Binary: expanding to size: " ++ show sz')
#endif
   return ()
expandBin (BinIO _ _ _) _ = return ()
	-- no need to expand a file, we'll assume they expand by themselves.

-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

getWord8 :: BinHandle -> IO Word8
getWord8 (BinMem _ ix_r sz_r arr_r) = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
    when (ix >= sz)  $
#if __GLASGOW_HASKELL__ <= 408
	throw (mkIOError eofErrorType "Hi.Binary.getWord8" Nothing Nothing)
#else
	ioError (mkIOError eofErrorType "Hi.Binary.getWord8" Nothing Nothing)
#endif
    arr <- readIORef arr_r
    w <- unsafeRead arr ix
    writeFastMutInt ix_r (ix+1)
    return w
getWord8 (BinIO _ ix_r h) = do
    ix <- readFastMutInt ix_r
    c <- hGetChar h
    writeFastMutInt ix_r (ix+1)
    return $! (fromIntegral (ord c))	-- XXX not really correct

getByte :: BinHandle -> IO Word8
getByte = getWord8

-- -----------------------------------------------------------------------------
-- Primitve Word writes

instance Binary Word8 where
  get  = getWord8

instance Binary Word16 where
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 8) .|. fromIntegral w2)

instance Binary Word32 where
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 24) .|. 
	       (fromIntegral w2 `shiftL` 16) .|. 
	       (fromIntegral w3 `shiftL`  8) .|. 
	       (fromIntegral w4))

instance Binary Word64 where
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    w5 <- getWord8 h
    w6 <- getWord8 h
    w7 <- getWord8 h
    w8 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 56) .|. 
	       (fromIntegral w2 `shiftL` 48) .|. 
	       (fromIntegral w3 `shiftL` 40) .|. 
	       (fromIntegral w4 `shiftL` 32) .|. 
	       (fromIntegral w5 `shiftL` 24) .|. 
	       (fromIntegral w6 `shiftL` 16) .|. 
	       (fromIntegral w7 `shiftL`  8) .|. 
	       (fromIntegral w8))

-- -----------------------------------------------------------------------------
-- Primitve Int writes

instance Binary Int8 where
  get h    = do w <- get h; return $! (fromIntegral (w::Word8))

instance Binary Int16 where
  get h    = do w <- get h; return $! (fromIntegral (w::Word16))

instance Binary Int32 where
  get h    = do w <- get h; return $! (fromIntegral (w::Word32))

instance Binary Int64 where
  get h    = do w <- get h; return $! (fromIntegral (w::Word64))

-- -----------------------------------------------------------------------------
-- Instances for standard types

instance Binary () where
    get  _     = return ()

instance Binary Bool where
    get  bh   = do x <- getWord8 bh; return $! (toEnum (fromIntegral x))

instance Binary Char where
    get  bh   = do x <- get bh; return $! (chr (fromIntegral (x :: Word32)))

instance Binary Int where
#if SIZEOF_HSINT == 4
    get  bh = do
	x <- get bh
	return $! (fromIntegral (x :: Int32))
#elif SIZEOF_HSINT == 8
    get  bh = do
	x <- get bh
	return $! (fromIntegral (x :: Int64))
#else
#error "unsupported sizeof(HsInt)"
#endif

instance Binary a => Binary [a] where
    get bh         = do h <- getWord8 bh
                        case h of
                          0 -> return []
                          _ -> do x  <- get bh
                                  xs <- get bh
                                  return (x:xs)

instance (Binary a, Binary b) => Binary (a,b) where
    get bh        = do a <- get bh
                       b <- get bh
                       return (a,b)

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         return (a,b,c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         return (a,b,c,d)

instance Binary a => Binary (Maybe a) where
    get bh           = do h <- getWord8 bh
                          case h of
                            0 -> return Nothing
                            _ -> do x <- get bh; return (Just x)

instance (Binary a, Binary b) => Binary (Either a b) where
    get bh            = do h <- getWord8 bh
                           case h of
                             0 -> do a <- get bh ; return (Left a)
                             _ -> do b <- get bh ; return (Right b)

#ifdef __GLASGOW_HASKELL__
instance Binary Integer where
    get bh = do 
	b <- getByte bh
	case b of
	  0 -> do (I# i#) <- get bh
		  return (S# i#)
	  _ -> do (I# s#) <- get bh
		  sz <- get bh
		  (BA a#) <- getByteArray bh sz
		  return (J# s# a#)

getByteArray :: BinHandle -> Int -> IO ByteArray
getByteArray bh (I# sz) = do
  (MBA arr) <- newByteArray sz 
  let loop n
	   | n ==# sz = return ()
	   | otherwise = do
		w <- getByte bh 
		writeByteArray arr n w
		loop (n +# 1#)
  loop 0#
  freezeByteArray arr


data ByteArray = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray# sz s of { (# s, arr #) ->
  (# s, MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s, arr #) ->
  (# s, BA arr #) }

#if __GLASGOW_HASKELL__ < 503
writeByteArray arr i w8 = IO $ \s ->
  case word8ToWord w8 of { W# w# -> 
  case writeCharArray# arr i (chr# (word2Int# w#)) s  of { s ->
  (# s , () #) }}
#else
writeByteArray arr i (W8# w) = IO $ \s ->
  case writeWord8Array# arr i w s of { s ->
  (# s, () #) }
#endif

#if __GLASGOW_HASKELL__ < 503
indexByteArray a# n# = fromIntegral (I# (ord# (indexCharArray# a# n#)))
#else
indexByteArray a# n# = W8# (indexWord8Array# a# n#)
#endif

instance (Integral a, Binary a) => Binary (Ratio a) where
    get bh = do a <- get bh; b <- get bh; return (a :% b)
#endif

instance Binary (Bin a) where
  get bh = do i <- get bh; return (BinPtr i)

-- -----------------------------------------------------------------------------
-- Lazy reading/writing

lazyGet :: Binary a => BinHandle -> IO a
lazyGet bh = do
    p <- get bh		-- a BinPtr
    p_a <- tellBin bh
    a <- unsafeInterleaveIO (getAt bh p_a)
    seekBin bh p -- skip over the object for now
    return a

-- --------------------------------------------------------------
-- 	Main wrappers: getBinFileWithDict, putBinFileWithDict
--
--	This layer is built on top of the stuff above, 
--	and should not know anything about BinHandles
-- --------------------------------------------------------------

initBinMemSize       = (1024*1024) :: Int
binaryInterfaceMagic = 0x1face :: Word32

getBinFileWithDict :: Binary a => FilePath -> IO a
getBinFileWithDict file_path = do
        bh <- Hi.Binary.readBinMem file_path

	-- Read the magic number to check that this really is a GHC .hi file
	-- (This magic number does not change when we change 
	--  GHC interface file format)
        magic <- get bh

        when (magic /= binaryInterfaceMagic) $
                error "magic number mismatch: old/corrupt interface file?"

	-- Read the dictionary
	-- The next word in the file is a pointer to where the dictionary is
	-- (probably at the end of the file)
        dict_p <- Hi.Binary.get bh	-- Get the dictionary ptr
        data_p <- tellBin bh		-- Remember where we are now
        seekBin bh dict_p
        dict <- getDictionary bh

        seekBin bh data_p		-- Back to where we were before

	-- Initialise the user-data field of bh
        let bh' = setUserData bh (initReadState dict)

	-- At last, get the thing 
        get bh'

-- -----------------------------------------------------------------------------
-- UserData
-- -----------------------------------------------------------------------------

data UserData = 
   UserData { 	-- This field is used only when reading
	      ud_dict :: Dictionary,

		-- The next two fields are only used when writing
	      ud_next :: IORef Int,	-- The next index to use
#if __GLASGOW_HASKELL__ < 604
	      ud_map  :: IORef (FiniteMap Unique (Int,FastString))
#else
	      ud_map  :: IORef (M.Map Unique (Int,FastString))
#endif
	}

noUserData = error "Hi.Binary.UserData: no user data"

initReadState :: Dictionary -> UserData
initReadState dict = UserData{ ud_dict = dict,
			       ud_next = undef "next",
			       ud_map  = undef "map" }

newWriteState :: IO UserData
newWriteState = do
        j_r <- newIORef 0
#if __GLASGOW_HASKELL__ < 604
        out_r <- newIORef emptyFM
#else
        out_r <- newIORef M.empty
#endif
        return (UserData { ud_dict = error "dict",
                           ud_next = j_r,
                           ud_map  = out_r })


undef s = error ("Hi.Binary.UserData: no " ++ s)

---------------------------------------------------------
--		The Dictionary 
---------------------------------------------------------

type Dictionary = Array Int FastString	-- The dictionary
					-- Should be 0-indexed

getDictionary :: BinHandle -> IO Dictionary
getDictionary bh = do 
  sz <- get bh
  elems <- sequence (take sz (repeat (getFS bh)))
  return (listArray (0,sz-1) elems)

#if __GLASGOW_HASKELL__ < 604
constructDictionary :: Int -> FiniteMap Unique (Int,FastString) -> Dictionary
constructDictionary j fm = array (0,j-1) (eltsFM fm)
#else
constructDictionary :: Int -> M.Map Unique (Int,FastString) -> Dictionary
constructDictionary j fm = array (0,j-1) (M.elems fm)
#endif

---------------------------------------------------------
--		Reading and writing FastStrings
---------------------------------------------------------
  
getFS bh = do
        (I# l)  <- get bh
        (BA ba) <- getByteArray bh (I# l)
        return $! (mkFastSubStringBA# ba 0# l)

instance Binary FastString where
        get bh = do j <- get bh -- Int
	            return $! (ud_dict (getUserData bh) ! j)

