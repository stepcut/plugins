{-# LANGUAGE CPP #-}
--
-- | A Posix.popen compatibility mapping.
--
-- If we use this, we should build -threaded
--
module System.Plugins.Process (exec, popen) where

import System.Exit
import System.IO
import System.Process
import Control.Concurrent       (forkIO)

import qualified Control.Exception as E

--
-- slight wrapper over popen for calls that don't care about stdin to the program
--
exec :: String -> [String] -> IO ([String],[String],Bool)
exec f as = do
        (a,b,c,_) <- popen f as (Just [])
        return (lines a, lines b,c)

type ProcessID = ProcessHandle

--
-- Ignoring exit status for now.
--
-- XXX there are still issues. Large amounts of output can cause what
-- seems to be a dead lock on the pipe write from runplugs, for example.
-- Posix.popen doesn't have this problem, so maybe we can reproduce its
-- pipe handling somehow.
--
popen :: FilePath -> [String] -> Maybe String -> IO (String,String,Bool,ProcessID)
popen file args minput =
    E.handle (\e -> return ([],show (e::E.IOException), False, error (show e))) $ do

    (inp,out,err,pid) <- runInteractiveProcess file args Nothing Nothing

    case minput of
        Just input -> hPutStr inp input >> hClose inp -- importante!
        Nothing    -> return ()

    -- Now, grab the input
    output <- hGetContents out
    errput <- hGetContents err

    -- SimonM sez:
    -- ... avoids blocking the main thread, but ensures that all the
    -- data gets pulled as it becomes available. you have to force the
    -- output strings before waiting for the process to terminate.
    --
    _ <- forkIO (E.evaluate (length output) >> return ())
    _ <- forkIO (E.evaluate (length errput) >> return ())

    -- And now we wait. We must wait after we read, unsurprisingly.
    exitCode <- waitForProcess pid -- blocks without -threaded, you're warned.
    case exitCode of
      ExitFailure code
          | null errput -> let errMsg = file ++ ": failed with error code " ++ show code
                           in return ([],errMsg,False,error errMsg)
          | otherwise -> return ([],errput,False,error errput)
      _ -> return (output,errput,True,pid)
