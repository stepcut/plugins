
import Plugins

main = do
        m <- loadRawObject "t.o"
        print (path m)
        resolveObjs
--      loadFunction m "sym"
