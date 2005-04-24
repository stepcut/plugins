
import Plugins

main = do loadPackage "posix"
          unloadPackage  "posix"
          loadPackage "posix"
