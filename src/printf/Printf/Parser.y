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
-- Parser for printf format strings
-- Based on B1.2 Formatted Output, from Kernighan and Ritchie.
--

{

{-# OPTIONS -fno-warn-name-shadowing -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-incomplete-patterns    #-}
-- ^ grr. happy needs them all on one line

module Printf.Parser where

import Printf.Lexer

}

%name parse
%tokentype { Token }
%token

       'h'  { LengthT 'h' }
       'l'  { LengthT 'l' }
       'L'  { LengthT 'L' }

       'd'  { ConvT 'd' }
       'i'  { ConvT 'i' }
       'o'  { ConvT 'o' }
       'x'  { ConvT 'x' }
       'X'  { ConvT 'X' }
       'u'  { ConvT 'u' }
       'c'  { ConvT 'c' }
       's'  { ConvT 's' }
       'f'  { ConvT 'f' }
       'e'  { ConvT 'e' }
       'E'  { ConvT 'E' }
       'g'  { ConvT 'g' }
       'G'  { ConvT 'G' }
       '%'  { ConvT '%' }

       '.'  { DotT }

        INT { IntT   $$ }
     STRING { StrT   $$ }
      FLAGS { FlagT $$ }

%%

printf :: { [Format] }
        : {- epsilon -}  { []      }
        | format0 printf { $1 : $2 }
        
format0 :: { Format }
        : string        { $1 }
        | format        { $1 }

string :: { Format }
        : STRING        { StrLit $1 }

format :: { Format }
        : flags width '.' precision length conv { ConvSp $1 $2 $4 $5 $6 }
        | flags width               length conv { ConvSp $1 $2 Nothing $3 $4 }

flags  :: { [Flag] }
        : FLAGS         { mkFlags $1 }

precision :: { Maybe Prec }
        : INT           { Just $1 }
        | {- epsilon -} { Nothing  }

width  :: { Maybe Width }
        : INT           { Just $1 }
        | {- epsilon -} { Nothing }

length :: { Length }
        : 'h'           { Short  }
        | 'l'           { Long   }
        | 'L'           { Double }
        | {- epsilon -} { Default}

conv   :: { Conv }
        : 'd'           { D } 
        | 'i'           { D } -- n.b
        | 'o'           { O } 
        | 'x'           { Xx } 
        | 'X'           { XX }
        | 'u'           { U }
        | 'c'           { C }
        | 's'           { S }
        | 'f'           { F }
        | 'e'           { Ee } 
        | 'E'           { EE }
        | 'g'           { Gg } 
        | 'G'           { GG }
        | '%'           { Percent }

{

------------------------------------------------------------------------
--
-- abstract syntax for printf format strings
--
data Format 
        = StrLit  String
        | ConvSp { flags     :: [Flag],
                   width     :: (Maybe Width),
                   precision :: (Maybe Prec ),
                   lenght    :: Length, 
                   conv      :: Conv }
        deriving (Show, Eq)

type Width  = Int
type Prec   = Int

data Flag 
        = LeftAdjust    -- -
        | Signed        -- +
        | Space         -- ' '
        | LeadZero      -- 0
        | Alt           -- #
        deriving (Show, Eq)

data Length 
        = Short         -- h
        | Long          -- l
        | Double        -- L
        | Default 
        deriving (Show, Eq)

data Conv 
        = D
        | O 
        | Xx | XX 
        | U
        | C
        | S
        | F
        | Ee | EE
        | Gg | GG
        | Percent
        deriving (Show, Eq)

mkFlags :: [Char] -> [Flag]
mkFlags [] = []
mkFlags (c:cs) = (case c of
        '-' -> LeftAdjust
        '+' -> Signed
        ' ' -> Space
        '0' -> LeadZero
        '#' -> Alt) : mkFlags cs

happyError :: [Token] -> a
happyError []  = error "Parser" "parse error"
happyError tks = error $ "Parser: " ++ show tks

}
