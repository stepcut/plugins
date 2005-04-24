--
-- test the popen function
--

import Plugins.Utils
import System.IO

main = do
        (sout,serr) <- exec "date" [] 
        mapM_ putStrLn serr
