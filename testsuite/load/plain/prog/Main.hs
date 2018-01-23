module Main (main) where

import Control.Exception (handle)

import System.Plugins

import API

fexn :: IOError -> IO ()
fexn = print

main :: IO ()
main = handle fexn $ do
    mf <- load "../TestIO.o" ["../api"] [] "resource"
    case mf of
      LoadFailure _ -> error "nope"
      LoadSuccess _ v -> do
        putStrLn "success"
        engage v

engage :: CLIInterface -> IO ()
engage plugin = repl plugin
