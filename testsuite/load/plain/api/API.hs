module API(CLIInterface(..), testio) where

import Data.Typeable

data CLIInterface = CLIInterface {
    repl :: IO ()
} deriving Typeable

testio :: CLIInterface
testio = CLIInterface { repl = return () }
