module API where

import Data.Typeable

data TestIO = TestIO { 
                field :: IO String 
#if __GLASGOW_HASKELL__ >= 800
        } deriving Typeable
#else
        }
instance Typeable TestIO where
#if __GLASGOW_HASKELL__ >= 603
    typeOf i = mkTyConApp (mkTyCon "API.TestIO") []
#else
    typeOf i = mkAppTy (mkTyCon "API.TestIO") []
#endif
#endif

testio :: TestIO
testio = TestIO { field = return "default value" }
