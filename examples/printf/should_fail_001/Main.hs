import Printf

main = do
        printf "%d\n"        $> (42 :: Int) ! []
        printf "0x%X\n"      $> (42 :: Int) ! []
        printf "%f\n"        $> (42.1234 :: Double) ! []
        printf "%c:%c:%c\n"  $> 'a' ! 'b' ! 'c' ! []
        printf "%s\n"        $> "haskell" ! []
        printf "%-010.4d\n"  $> (42 :: Int) ! []
        printf "%010.4f\n"   $> (42.1234 :: Double) ! []
        printf "%10.4s\n"    $> (7 :: Int)! []
        printf "%-10.4s\n"   $> "haskell" ! []

