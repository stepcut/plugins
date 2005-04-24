import Plugins
import API
import System.Directory

main = do
        status <- make "../Plugin.hs" [ "-i../api", "-odir", "/tmp" ]
        o <- case status of
                MakeSuccess _ o -> return o
                MakeFailure e -> mapM_ putStrLn e >> error "didn't compile"
        m_v     <- load o ["../api"] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            _               -> error "load failed"
        putStrLn $ field v 
        mapM_ removeFile ["/tmp/Plugin.hi", "/tmp/Plugin.o" ]

