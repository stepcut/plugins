{-# OPTIONS -cpp #-}
--
-- Should evaluate to '3', unless something goes wrong.
--
-- Not so bad to use AltData, as it is already derived for all the basic
-- types. Then, just replace deriving Typeable, with hand-derived
-- instance of Typeable (see hs-plugins/examples/eval/eval_fn1/Poly.hs
--
--

#include "../../../config.h"

import Eval.Haskell
import AltData.Dynamic

-- import Data.Dynamic

pkgconf = TOP ++ "/plugins.conf.inplace"

main = do
    a <- return $ toDyn (3::Int)

    m_b <- unsafeEval_ "\\dyn -> fromMaybe (7 :: Int) (fromDyn dyn)"
                ["AltData.Dynamic","Data.Maybe"]  -- imports

                [ "-package-conf "++pkgconf , "-package altdata" ]

                [ pkgconf ]
                []
                

{-
-- should work, but doesn't. type check fails 
-- (due to static vs dynamic typing issue)

     m_b <- unsafeEval_ "\\dyn -> fromMaybe (7 :: Int) (fromDynamic dyn)"
                                ["Data.Dynamic","Data.Maybe"] [] []
-}

    case m_b of 
        Left s   -> mapM_ putStrLn s
        Right b  -> putStrLn $ show (b a :: Int)
