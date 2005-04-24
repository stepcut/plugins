{-# OPTIONS -cpp -fglasgow-exts #-}
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
-- reimplement the Data.Dynamic library to use equality over the
-- canonical name of a type, rather than on integer keys. The later is
-- how the Haskell library works, and is broken for our situation:
-- static versus dynamic instances of the same type seem to generate
-- different keys, meaning equal types are not detected as such.
--

module AltData.Dynamic ( 

        Dynamic,                -- must be abstract
        toDyn,                  -- :: Typeable a => a -> Dynamic
        fromDyn,                -- :: Typeable a => Dynamic -> Maybe a
        fromDynamic,
        dynApp,
        dynApply,
        dynAppHList,

        typecase,
        (-->),

        _Int,
        _Char,
        _Bool,
        _String,
        _IntToInt,

   ) where

import AltData.Typeable
import Data.Maybe
import System.IO.Unsafe         ( unsafePerformIO )
import GHC.Base                 ( unsafeCoerce# )
import Data.List

data Dynamic = Dynamic TypeRep Obj

type Obj = forall a . a

instance Show Dynamic where
   -- the instance just prints the type representation.
   showsPrec _ (Dynamic t _) = 
          showString "<" . 
	  showsPrec 0 t   . 
	  showString ">"

instance Typeable Dynamic where
#if __GLASGOW_HASKELL__ >= 603
    typeOf _ = mkTyConApp (mkTyCon "AltData.Dynamic") []
#else
    typeOf _ = mkAppTy    (mkTyCon "AltData.Dyanmic") []
#endif

--
-- must be monomophic, see Data.Dynamic
--
toDyn :: Typeable a => a -> Dynamic
toDyn v = Dynamic (typeOf v) (unsafeCoerce# v)

--
-- Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type. (this is the same as fromDynamic)
--
-- Uses string comparison of the name of the type, rather than the
-- hashed key of the type, which doesn't work for plugins, which mix
-- static and dynamic loaded code.
--
-- TypeRep is abstract, unfortunately.
--
fromDyn :: Typeable a => Dynamic -> Maybe a

fromDyn (Dynamic t v) =
  case unsafeCoerce# v of 
    r | t == typeOf r -> Just r
      | otherwise     -> unsafePerformIO (putStrLn $
                                "Couldn't match `"    ++show(typeOf r) ++
                                        "' against `" ++show t         ++"'"++
                                "\n\tExpected type: " ++show(typeOf r) ++
                                "\n\tInferred type: " ++show t
                        ) `seq` Nothing

fromDynamic d = case fromDyn d of
                Just v  -> v
                Nothing -> error ("\nType error in dynamic unwrapping.\n" ++
                                  "In value `" ++ show d ++ "'")

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x = case dynApply f x of 
             Just r -> r
             Nothing -> error ("Type error in dynamic application.\n" ++
                               "Can't apply function " ++ show f ++
                               " to argument " ++ show x)

--
-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
--
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic t1 f) (Dynamic t2 x) =
#if __GLASGOW_HASKELL__ >= 603
  case funResultTy t1 t2 of
#else
  case applyTy t1 t2 of
#endif
    Just t3 -> Just (Dynamic t3 ((unsafeCoerce# f) x))
    Nothing -> Nothing


--
-- hmm
--
dynAppHList :: Dynamic -> [Dynamic] -> Dynamic
dynAppHList fn []     = fn      -- partial applicaiton
dynAppHList fn (x:xs) = (fn `dynApp` x) `dynAppHList` xs

-- ---------------------------------------------------------------------
--
-- Implementation of typecase, without patterns, based on "Dynamic
-- typing in a statically typed language". Mart\'in Abadi, Luca
-- Cardelli, Benjamin Pierce and Gordon Plotkin. ACM Trans. Prog. Lang.
-- and Syst. 13(2):237-268, 1991.
--
-- Doesn't provide the behaviour that if the value is not a Dynamic,
-- then typecase returns a error. Need low-level ops for that.
--

-- typecase :: Typeable u => Dynamic -> [(TypeRep, Dynamic)] -> u -> u

typecase :: Typeable u
         => Dynamic               -- selector
         -> [(Dynamic, Dynamic)]  -- branches
         -> u                     -- else arm
         -> u                     -- return type

typecase dv@(Dynamic ty _) alts dflt =
        case find (hasType ty) alts of
                Nothing -> dflt
                Just v  -> fromDynamic $ snd v `dynApp` dv

        where hasType t ((Dynamic u _),_) = t == u

infixl 6 -->
(-->) :: Typeable b => a -> b -> (a,Dynamic)
a --> b = (a,toDyn b)

--
-- need a way to generate a Dynamic prelude
--
_Int      = toDyn ( undefined :: Int        )
_Char     = toDyn ( undefined :: Char       )
_Bool     = toDyn ( undefined :: Bool       )
_String   = toDyn ( undefined :: [Char]     )
_IntToInt = toDyn ( undefined :: Int -> Int )

------------------------------------------------------------------------
