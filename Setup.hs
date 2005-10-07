import Distribution.Simple
import Distribution.Setup   ( ConfigFlags (..) )
import System.Directory     ( findExecutable )

main = defaultMainWithHooks (defaultUserHooks { postConf = defaultPostConf })
    where defaultPostConf args flags lbi
              = do args' <- fmap (args++) (configToArgs flags)
                   (postConf defaultUserHooks) args' flags lbi

configToArgs :: ConfigFlags -> IO [String]
configToArgs (ConfigFlags { configHcPath = Just hcPath })
    = do exec <- findExecutable hcPath
         case exec of
           Just realPath -> return ["--with-ghc="++realPath]
           Nothing -> return ["--with-ghc="++hcPath]
configToArgs _ = return []
