module Load where

import API
import System.Plugins

testload = do

  s  <- make "../Plugin1.hs"     ["-i../api"]
  o1 <- case s of
    MakeSuccess _ o -> return o
    MakeFailure e   -> mapM_ putStrLn e >> fail "o1"
    
  s  <- make "../Sub/Plugin2.hs" ["-i../api"]
  o2 <- case s of
    MakeSuccess _ o -> return o
    MakeFailure e   -> mapM_ putStrLn e >> fail "o2"

  fc <- pdynload o1 ["..","../api"] [] "API.PluginAPI" "action"

  case fc of
    LoadFailure msg -> mapM_ putStrLn msg 
    LoadSuccess modul proc -> do
      let ac :: API.PluginAPI; ac = proc
      let s = proc 42
      print s

  print o2
  fc <- pdynload o2 ["..","../api"] [] "API.PluginAPI" "action"
  case fc of
    LoadFailure msg -> mapM_ putStrLn msg 
    LoadSuccess modul proc -> do
      let ac :: API.PluginAPI; ac = proc
      let s = proc 42
      print s
