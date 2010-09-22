module Plugin where

import API
import Data.Dynamic

my_fun = plugin { 
                equals = \x y -> (x /= y)  -- a strange equals function :)
         }

resource_dyn :: Dynamic
resource_dyn = toDyn my_fun

