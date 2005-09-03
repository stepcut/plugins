{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dynamic
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Dynamic interface provides basic support for dynamic types.
-- 
-- Operations for injecting values of arbitrary type into
-- a dynamically typed value, Dynamic, are provided, together
-- with operations for converting dynamic values into a concrete
-- (monomorphic) type.
-- 
-----------------------------------------------------------------------------

module AltData.Dynamic (

	-- Module Data.Typeable re-exported for convenience
	module AltData.Typeable,

	-- * The @Dynamic@ type
	Dynamic,	-- abstract, instance of: Show, Typeable

	-- * Converting to and from @Dynamic@
	toDyn,		-- :: Typeable a => a -> Dynamic
	fromDyn,	-- :: Typeable a => Dynamic -> a -> a
	fromDynamic,	-- :: Typeable a => Dynamic -> Maybe a
	
#if __GLASGOW_HASKELL__ >= 603
	-- * Applying functions of dynamic type
	dynApply,
	dynApp,
        dynTypeRep

#endif
  ) where

import AltData.Typeable
import Data.Maybe

import System.IO.Unsafe    (unsafePerformIO)

import GHC.Base
import GHC.Show
#if __GLASGOW_HASKELL__ >= 603
import GHC.Err
#endif

unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#

-------------------------------------------------------------
--
--		The type Dynamic
--
-------------------------------------------------------------

{-|
  A value of type 'Dynamic' is an object encapsulated together with its type.

  A 'Dynamic' may only represent a monomorphic value; an attempt to
  create a value of type 'Dynamic' from a polymorphically-typed
  expression will result in an ambiguity error (see 'toDyn').

  'Show'ing a value of type 'Dynamic' returns a pretty-printed representation
  of the object\'s type; useful for debugging.
-}
data Dynamic = Dynamic TypeRep Obj

instance Typeable Dynamic where
#if __GLASGOW_HASKELL__ >= 603
    typeOf _ = mkTyConApp (mkTyCon "AltData.Dynamic") []
#else
    typeOf _ = mkAppTy    (mkTyCon "AltData.Dynamic") []
#endif

instance Show Dynamic where
   -- the instance just prints the type representation.
   showsPrec _ (Dynamic t _) = 
          showString "<<" . 
	  showsPrec 0 t   . 
	  showString ">>"

type Obj = forall a . a
 -- Dummy type to hold the dynamically typed value.
 --
 -- In GHC's new eval/apply execution model this type must
 -- be polymorphic.  It can't be a constructor, because then
 -- GHC will use the constructor convention when evaluating it,
 -- and this will go wrong if the object is really a function.  On
 -- the other hand, if we use a polymorphic type, GHC will use
 -- a fallback convention for evaluating it that works for all types.
 -- (using a function type here would also work).

-- | Converts an arbitrary value into an object of type 'Dynamic'.  
--
-- The type of the object must be an instance of 'Typeable', which
-- ensures that only monomorphically-typed objects may be converted to
-- 'Dynamic'.  To convert a polymorphic object into 'Dynamic', give it
-- a monomorphic type signature.  For example:
--
-- >    toDyn (id :: Int -> Int)
--
toDyn :: Typeable a => a -> Dynamic
toDyn v = Dynamic (typeOf v) (unsafeCoerce v)

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDynamic'.
fromDyn :: Typeable a
 	=> Dynamic 	-- ^ the dynamically-typed object
	-> a		-- ^ a default value 
	-> a		-- ^ returns: the value of the first argument, if
			-- it has the correct type, otherwise the value of
			-- the second argument.
fromDyn (Dynamic t v) def
  | typeOf def == t = unsafeCoerce v
  | otherwise       = def

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDyn'.
fromDynamic
	:: Typeable a
	=> Dynamic	-- ^ the dynamically-typed object
	-> Maybe a	-- ^ returns: @'Just' a@, if the dynamically-typed
			-- object has the correct type (and @a@ is its value), 
			-- or 'Nothing' otherwise.
fromDynamic (Dynamic t v) =
  case unsafeCoerce v of 
    r | t == typeOf r -> Just r
      | otherwise     -> unsafePerformIO (putStrLn $
                                "Couldn't match `"    ++show(typeOf r) ++
                                        "' against `" ++show t         ++"'"++
                                "\n\tExpected type: " ++show(typeOf r) ++
                                "\n\tInferred type: " ++show t
                        ) `seq` Nothing

#if __GLASGOW_HASKELL__ >= 603

-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic t1 f) (Dynamic t2 x) =
  case funResultTy t1 t2 of
    Just t3 -> Just (Dynamic t3 ((unsafeCoerce f) x))
    Nothing -> Nothing


dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x = case dynApply f x of 
             Just r -> r
             Nothing -> error ("Type error in dynamic application.\n" ++
                               "Can't apply function " ++ show f ++
                               " to argument " ++ show x)

dynTypeRep :: Dynamic -> TypeRep
dynTypeRep (Dynamic tr _) = tr 

#endif
