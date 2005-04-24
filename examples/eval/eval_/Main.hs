
import Eval.Haskell

main = do i <- eval_ "Just (7 :: Int)" 
                     ["Maybe"] 
                     ["-fglasgow-exts"] 
                     []
                     [] :: IO (Either [String] (Maybe (Maybe Int)))
          print i
