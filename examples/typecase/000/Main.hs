import AltData.Dynamic
import Data.Char

main = putStrLn f

f = let v = toDyn (7 :: Int)
    in typecase (v) [ 
            _Bool     --> \(b::Bool)     -> show (not b)++" :: Bool", 
            _Char     --> \(c::Char)     -> show (toUpper c)++" :: Char", 
            _Int      --> \(i::Int)      -> show (-i)++" :: Int", 
            _String   --> \(s::String)   -> show (reverse s)++" :: [Char]", 
            _IntToInt --> \(f::Int->Int) -> show (f 7) ++":: Int -> Int"
       ] ("couldn't find a typing")

