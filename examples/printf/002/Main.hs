import Printf
import Control.Exception        ( evaluate )

main = do
        fn <- evaluate $! printf "%10.4f\n"
        fn $> (10.0 :: Double)          ! []
        fn $> (-10.0 :: Double)         ! []
        fn $> (10.1010 :: Double)       ! []
        fn $> (0.0 :: Double)           ! []
        fn $> (0.987654321 :: Double)   ! []
        fn $> (987654321 :: Double)     ! []
        fn $> (-987654321 :: Double)     ! []
