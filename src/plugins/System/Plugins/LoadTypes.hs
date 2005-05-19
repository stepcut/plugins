module System.Plugins.LoadTypes
    ( Key (..)
    , Symbol
    , Type
    , Errors
    , PackageConf
    , Module (..)
    , ObjType (..)
    ) where

import Language.Hi.Parser

data Key = Object String | Package String
type Symbol      = String
type Type        = String
type Errors      = [String]
type PackageConf = FilePath

data Module = Module { path  :: !FilePath 
                     , mname :: !String 
                     , kind  :: !ObjType 
                     , iface :: Iface    -- cache the iface
                     , key   :: Key
                     }
instance Ord Module where
    compare m1 m2 = mname m1 `compare` mname m2

instance Eq Module where
    m1 == m2 = mname m1 == mname m2

data ObjType = Vanilla | Shared deriving Eq