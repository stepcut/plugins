module Plugin where

import Data.Typeable
import Data.Generics.Aliases
import Data.Generics.Schemes

import API

resource = rsrc { 
     field = id listify :: Typeable r => (r -> Bool) -> GenericQ [r]
}
