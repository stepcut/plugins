module TestIO (resource) where

import Control.Monad (forever)

import API

resource :: CLIInterface
resource = testio { repl = loop }

loop :: IO ()
loop = forever $ getLine >>= putStrLn
