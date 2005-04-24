{-# GLOBALOPTIONS -package posix #-}

module M ( resource ) where

import System.IO.Unsafe
import API
import System.Process
import System.IO

resource = tiny { field = date }

date :: String
date = unsafePerformIO $ do
            (_,outh,_,proc) <- runInteractiveProcess "echo" ["hello"] Nothing Nothing		
    	    waitForProcess proc
            s <- hGetContents outh
            return s
