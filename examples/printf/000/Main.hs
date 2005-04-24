import Printf

main = do printf "%d\n"         $> (42::Int) ! []
          printf "%u\n"         $> (42::Int) ! []
          printf "0%o\n"        $> (42::Int) ! []
          printf "0x%x\n"       $> (42::Int) ! []
          printf "0x%X\n"       $> (42::Int) ! []

          printf "%e\n"         $> (42.1234 :: Double) ! []
          printf "%E\n"         $> (42.1234 :: Double) ! []
          printf "%g\n"         $> (42.1234 :: Double) ! []
          printf "%G\n"         $> (42.1234 :: Double) ! []
          printf "%f\n"         $> (42.1234 :: Double) ! []

          printf "%c:%c:%c\n"   $> 'a' ! 'b' ! 'c' ! []
          printf "%s\n"         $> "printf" ! []

          printf "%+d\n"        $> (42::Int) ! []
          printf "%+0d\n"       $> (42::Int) ! []
          printf "%0+d\n"       $> (42::Int) ! []
          printf "%10d\n"       $> (42::Int) ! []
          printf "%-010d\n"     $> (42::Int) ! []
          printf "%-010.2d\n"   $> (42::Int) ! []

          printf "%+f\n"        $> (42.1234 :: Double) ! []
          printf "%+0f\n"       $> (42.1234 :: Double) ! []
          printf "%0+f\n"       $> (42.1234 :: Double) ! []
          printf "%10f\n"       $> (42.1234 :: Double) ! []
          printf "%-010f\n"     $> (42.1234 :: Double) ! []
          printf "%-010.2f\n"   $> (42.1234 :: Double) ! []

          printf "%10s\n"    $> "printf" ! []
          printf "%-10s\n"   $> "printf" ! []
          printf "%10.2s\n"  $> "printf" ! []
          printf "%2.10s\n"  $> "printf" ! []
          printf "%-2.10s\n" $> "printf" ! []
          printf "%-10.2s\n" $> "printf" ! []
