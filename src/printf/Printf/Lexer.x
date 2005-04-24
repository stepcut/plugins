-- 
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

--
-- Lexer for printf format strings
-- Based on B1.2 Formatted Output, from Kernighan and Ritchie.
--

{

{-# OPTIONS -w #-}
-- ^ don't want to see all the warns alex templates produce

module Printf.Lexer ( scan, Token(..) ) where

}

%wrapper "monad"

$digit = 0-9
$conv  = [dioxXucsfeEgGpn\%]
$len   = [hlL]
$flag  = [\-\+\ 0\#]
$str   = [. # \%]

printf :-

<0> $str+                   { mkstr      }
<0> \%                      { begin flag }

<flag> $flag*               { mkflags `andBegin` fmt }

<fmt>  $digit+              { mkint     }
<fmt> \.                    { mkdot     }
<fmt> $len                  { mklength  }
<fmt> $conv                 { mkconv `andBegin` 0 }

{


mkflags, mkconv, mklength, mkint, mkstr, mkdot :: AlexInput -> Int -> Alex Token

mkflags  (_,_,input) len = return (FlagT (take len input))
mkconv   (_,_,(c:_)) _   = return (ConvT c)
mklength (_,_,(c:_)) _   = return (LengthT c)
mkint    (_,_,input) len = return (IntT (read (take len input)))
mkstr    (_,_,input) len = return (StrT (take len input))
mkdot    _ _             = return DotT

alexEOF = return EOFT

data Token 
        = FlagT   [Char]
        | ConvT   Char 
        | LengthT Char
        | IntT    Int  
        | StrT    String
        | DotT    
        | EOFT
    deriving (Eq, Show)

scan :: String -> Either String [Token]
scan str = runAlex str $ do
        let loop tks = do 
                tok <- alexMonadScan; 
                if tok == EOFT then do return $! reverse tks 
                               else loop $! (tok:tks)
        loop []

}
