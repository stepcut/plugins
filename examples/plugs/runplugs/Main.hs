{-# OPTIONS -cpp #-}
--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Runplugs: use hs-plugins to run a Haskell expression under
-- controlled conditions.
--
import System.Eval.Haskell      (unsafeEval_)

import Data.Maybe               (isJust, fromJust)
import Control.Monad            (when) 
import Control.Exception        (evaluate) 


import System.Exit              (exitWith, ExitCode(ExitSuccess))
import System.IO                (hGetContents, hPutStrLn, putStrLn, hClose, stdin, stdout, stderr)
#if !defined(CYGWIN) && !defined(__MINGW32__)
import System.Posix.Resource    (setResourceLimit,
			         Resource(ResourceCPUTime), 
                                 ResourceLimits(ResourceLimits),
			         ResourceLimit(ResourceLimit))
import Control.Concurrent           ( forkIO )
import qualified Control.Exception  ( evaluate )

rlimit = ResourceLimit 3
#endif

context = prehier ++ datas ++ controls

prehier = ["Char", "List", "Maybe", "Numeric", "Random" ]

datas   = map ("Data." ++) [
                "Bits", "Bool", "Char", "Dynamic", "Either", 
                "Graph", "Int", "Ix", "List", "Maybe", 
#if __GLASGOW_HASKELL__ >= 604
                "Map",
#else
                "FiniteMap",
#endif
                "Ratio", "Set", "Tree", "Tuple", "Typeable", "Word" 
              ]

controls = map ("Control." ++) ["Monad", "Arrow"]

--
-- with ghc 6.4, ghc doesn't seem to be able to call gcc, setNoFDBlocking fails.
--
-- *** Assembler
-- gcc -I/tmp -c /tmp/ghc11596.s -o /tmp/MySzQ14137.o
-- 
-- Failed: gcc -I/tmp -c /tmp/ghc11596.s -o /tmp/MySzQ14137.o
-- gcc: setNonBlockingFD: invalid argument (Bad file descriptor)
--
main = do
#if !defined(CYGWIN) && defined(__MINGW32__)
        setResourceLimit ResourceCPUTime (ResourceLimits rlimit rlimit)
#endif
        s <- hGetContents stdin
        when (not . null $ s) $ do
                s <- unsafeEval_ ("(take 2048 (show ("++s++")))") context ["-v"] [] []
                case s of
                        Left errs -> mapM_ putStrLn errs
                        Right s   -> putStrLn s
        exitWith ExitSuccess

