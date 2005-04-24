{-# OPTIONS -fglasgow-exts #-}
-- 
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
-- 

--
-- compile and run haskell strings at runtime.
--
-- Some of the backend code is based on Ian Lynagh's TH version of
-- Printf.
--
-- The specification of this implementation is taken from 
-- the OpenBSD 3.5 man page for printf(3)
--

module Printf.Compile ( 
        printf, 
        (!), 
        ($>), ($<),
   ) where

import Printf.Lexer
import Printf.Parser

import Eval.Haskell             ( eval )
import Eval.Utils               ( escape )
import Plugins.Utils            ( (<>), (<+>) )

import AltData.Dynamic 
import AltData.Typeable hiding  ( typeOf )

import Data.List
import Data.Maybe               ( isNothing, isJust )

import System.IO.Unsafe         ( unsafePerformIO )

type Type = String
type Code = String

-- ---------------------------------------------------------------------
--
-- Generate a new Haskell function, as compiled native-code, from a
-- printf format string. It isn't applied to its arguments yet.
-- The function will return a String, but we won't typecheck this till
-- application.
--
printf :: String -> Dynamic     -- ([Dynamic] -> String)
printf fmt = run src ["Data.Char","Numeric"]
    where 
          src     = compile . parse . scan' . escape $ fmt 
          scan' s = either (error "lexer failed") (id) (scan s)

          run e i = case unsafePerformIO (eval e i) of
                        Nothing -> error "source failed to compile"
                        Just a  -> a

--
-- application shortcuts. these expect all arguments to be supplied, and
-- if this is so, we can then give the result a type.
-- partial application means type annotations, or retaining everything
-- as a Dynamic
--

--
-- sprintf
-- Apply a new fn to a arg list, returning a String
--
infixr 0 $<
($<) :: Dynamic -> [Dynamic] -> String
f $< as = fromDynamic $! f `dynAppHList` as

--
-- printf
-- Apply a new fn to a arg list, printing out the result
--
infixr 0 $>
($>) :: Dynamic -> [Dynamic] -> IO ()
f $> as = putStr (fromDynamic $! f `dynAppHList` as)

-- ---------------------------------------------------------------------
-- a printf code generator
--
-- ToDo handle all the different specifiers
--
-- Compile a printf format syntax tree into a Haskell string
-- representing a Haskell function to implement this printf.
--
compile :: [Format] -> String
compile fmt = 
        let (tys,src) = compile' fmt 0
        in "toDyn $ \\" <> 
           spacify (map (\(ty,i) -> parens('x':show i <+> "::" <+> ty)) 
                        (zip tys [0..length src])) <+> "->" <+> consify src

     where spacify s = concat (intersperse " "  s)
           consify s = concat (intersperse "++" s)

-- ---------------------------------------------------------------------
--
-- Compile an individual format or string literal

compile' :: [Format] -> Int -> ([String],[String])
compile' [] _ = ([],[])

compile' ((StrLit s):xs) i = ( ts, ( '"':s++"\"" ):ss )
    where (ts,ss) = compile' xs i

compile' ((ConvSp _ _ _ _ Percent):xs) i = (ts, "\"%\"":ss)
    where (ts,ss) = compile' xs $! i+1

compile' (c@(ConvSp _ _ _ _ t):xs) i = 
        (typeOf t:ts, parens(
           (snd.plus.pad.alt.trunc.codeOf) c  -- apply transformations
        <+> ident i) : ss)

    where (ts, ss) = compile' xs $! i+1

-- ---------------------------------------------------------------------
--
-- What argument type does a conversion specifier generate?
-- should be a FM
--
typeOf :: Conv -> Type
typeOf x = case x of
        D       -> "Int"
        O       -> "Int"
        Xx      -> "Int"
        XX      -> "Int"
        U       -> "Int"
        C       -> "Char"
        S       -> "String"
        F       -> "Double"
        Ee      -> "Double"
        EE      -> "Double"
        Gg      -> "Double"
        GG      -> "Double"
        Percent -> error "typeOf %: conversion specifier has no argument type"

-- ---------------------------------------------------------------------
--
-- Generate Haskell code for each particular format
--
codeOf :: Format -> (Format,Code)
codeOf c@(ConvSp _ _ p _ f) = case f of

--  diouxX  The int (or appropriate variant) argument is converted to signed
--          decimal (d and i), unsigned octal (o), unsigned decimal (u), or
--          unsigned hexadecimal (x and X) notation.  The letters abcdef are
--          used for x conversions; the letters ABCDEF are used for X conver-
--          sions.  The precision, if any, gives the minimum number of digits
--          that must appear; if the converted value requires fewer digits,
--          it is padded on the left with zeros.

        D  -> (c,"(show)")
        U  -> (c,"(show)")
        O  -> (c,"(\\v -> showOct v [])")
        Xx -> (c,"(\\v -> showHex v [])")
        XX -> (c,"(\\v -> map toUpper (showHex v []))")

--  eE      The double argument is rounded and converted in the style
--          [-]d.ddde+-dd where there is one digit before the decimal-point
--          character and the number of digits after it is equal to the pre-
--          cision; if the precision is missing, it is taken as 6; if the
--          precision is zero, no decimal-point character appears.  An E con-
--          version uses the letter E (rather than e) to introduce the expo-
--          nent.  The exponent always contains at least two digits; if the
--          value is zero, the exponent is 00.

-- TODO prints exponent differently to printf(3)

        Ee -> let prec = if isNothing p then "Just 6" else show p
              in (c,"(\\v->(showEFloat("++prec++")v)[])")

        EE -> let prec = if isNothing p then "Just 6" else show p
              in (c,"(\\v->map toUpper((showEFloat ("++prec++")v)[]))")

--   gG      The double argument is converted in style f or e (or E for G con-
--           versions).  The precision specifies the number of significant
--           digits.  If the precision is missing, 6 digits are given; if the
--           precision is zero, it is treated as 1.  Style e is used if the
--           exponent from its conversion is less than -4 or greater than or
--           equal to the precision.  Trailing zeros are removed from the
--           fractional part of the result; a decimal point appears only if it
--           is followed by at least one digit.

-- TODO unimplemented

        Gg -> let prec = if isNothing p then "Just 6" else show p
              in (c,"(\\v->(showGFloat("++prec++")v)[])")

        GG -> let prec = if isNothing p then "Just 6" else show p
              in (c,"(\\v->map toUpper((showGFloat ("++prec++")v)[]))")

--  f       The double argument is rounded and converted to decimal notation
--          in the style [-]ddd.ddd, where the number of digits after the
--          decimal-point character is equal to the precision specification.
--          If the precision is missing, it is taken as 6; if the precision
--          is explicitly zero, no decimal-point character appears.  If a
--          decimal point appears, at least one digit appears before it.

        F  -> let prec = if isNothing p then "Just 6" else show p
              in (c, "(\\v -> (showFFloat ("++prec++") v) [])")

--  c       The int argument is converted to an unsigned char, and the re-
--          sulting character is written.

        C  -> (c,"(\\c -> (showLitChar c) [])")

--  s       The char * argument is expected to be a pointer to an array of
--          character type (pointer to a string).  Characters from the array
--          are written up to (but not including) a terminating NUL charac-
--          ter; if a precision is specified, no more than the number speci-
--          fied are written.  If a precision is given, no null character
--          need be present; if the precision is not specified, or is greater
--          than the size of the array, the array must contain a terminating
--          NUL character.

        S  -> (c,"(id)")

--  %       A `%' is written.  No argument is converted.  The complete con-
--          version specification is `%%'.

        Percent -> (c,"%")

codeOf _ = error "codeOf: unknown conversion specifier"

-- ---------------------------------------------------------------------
--
-- Do we need a leading + ?
--
--   A `+' character specifying that a sign always be placed before a
--   number produced by a signed conversion.  A `+' overrides a space
--   if both are used.
--
plus :: (Format, Code) -> (Format, Code)
plus p@(StrLit _,_) = p
plus a@(c@(ConvSp fs _w _ _ x), code) = case x of
        D -> prefix
        Ee-> prefix
        EE-> prefix
        Gg-> prefix
        GG-> prefix
        F -> prefix
        _ -> a

    where prefix = let pref | Signed `elem` fs   = "\"+\""
                          | Space `elem` fs    = "\" \""
                          | otherwise          = "[]"
                   in (c,parens("\\v ->"<+>pref<+>"++ v") <$> code)

                {- munge = case w of
                                Just w' | w' > 0 -> "tail"
                                _ -> "" -}

-- ---------------------------------------------------------------------
-- Work out padding.
--
--   A negative field width flag `-' indicates the converted value is
--   to be left adjusted on the field boundary.  Except for n conver-
--   sions, the converted value is padded on the right with blanks,
--   rather than on the left with blanks or zeros.  A `-' overrides a
--   `0' if both are given.
--
--   A zero `0' character specifying zero padding.  For all conver-
--   sions except n, the converted value is padded on the left with
--   zeros rather than blanks.  If a precision is given with a numeric
--   conversion (d, i, o, u, x, and X), the `0' flag is ignored.
--
pad :: (Format,Code) -> (Format,Code)
pad (c@(ConvSp fs (Just w) p _ x),code)

        | LeftAdjust `elem` fs 
        = (c, parens(parens("\\i c s -> if length s < i"<+>
                        "then s ++ take (i-length s) (repeat c) else s")
              <+>show w<+>"' '")<$>code )

        | otherwise            
        = (c, parens(parens("\\i c s -> if length s < i"<+>
                        "then take (i-length s) (repeat c) ++ s else s")
              <+>show w<+>pad_chr)<$>code)

   where pad_chr | isNumeric x && isJust p        = "' '"
                 | LeadZero `elem` fs             = "'0'"
                 | otherwise                      = "' '"

pad (c@(ConvSp _ Nothing _ _ _),code) = (c,code)

pad ((StrLit _),_) = error "pad: can't pad str lit"

isNumeric :: Conv -> Bool
isNumeric x = case x of
        D  -> True
        O  -> True
        U  -> True
        Xx -> True
        XX -> True
        _  -> False

-- ---------------------------------------------------------------------
--
-- Check the 'alternate' modifier
--
--   A hash `#' character specifying that the value should be convert-
--   ed to an ``alternate form''.  For c, d, i, n, p, s, and u conver-
--   sions, this option has no effect.  For o conversions, the preci-
--   sion of the number is increased to force the first character of
--   the output string to a zero (except if a zero value is printed
--   with an explicit precision of zero).  For x and X conversions, a
--   non-zero result has the string `0x' (or `0X' for X conversions)
--   prepended to it.  For e, E, f, g, and G conversions, the result
--   will always contain a decimal point, even if no digits follow it
--   (normally, a decimal point appears in the results of those con-
--   versions only if a digit follows).  For g and G conversions,
--   trailing zeros are not removed from the result as they would oth-
--   erwise be.
--

alt :: (Format,Code) -> (Format,Code)
alt a@(c@(ConvSp fs _ _ _ x), code) | Alt `elem` fs = case x of

        Xx -> (c,parens("\\v->if fst (head (readHex v)) /= 0"<+>
                      "then \"0x\"++v else v")<$>code)

        XX -> (c,parens("\\v->if fst (head (readHex v)) /= 0"<+> 
                      "then \"0X\"++v else v")<$>code)

        O  -> (c,parens("\\v->if fst(head(readOct v)) /= 0"<+>
                      "then \"0\"++v else v")<$>code)
        _  -> a

alt a = a

-- ---------------------------------------------------------------------
--
-- Handle precision. Involves truncating strings and decimal points
--
--     An optional precision, in the form of a period `.' followed by an op-
--     tional digit string.  If the digit string is omitted, the precision
--     is taken as zero.  This gives the minimum number of digits to appear
--     for d, i, o, u, x, and X conversions, the number of digits to appear
--     after the decimal-point for e, E, and f conversions, the maximum num-
--     ber of significant digits for g and G conversions, or the maximum
--     number of characters to be printed from a string for s conversions.
--
trunc :: (Format,Code) -> (Format,Code)
trunc (c@(ConvSp _ _ (Just i) _ x), code) = case x of
        S -> (c, parens("(\\i s -> if length s > i"<+>
                                "then take i s else s)"<+>show i)<$>code)

        _ | isNumeric x -> {-TODO-} (c, code)
          | otherwise   -> (c, code)

trunc c = c

-- ---------------------------------------------------------------------
-- make a new variable
ident i = 'x':show i

-- wrap in parens
parens p = "("++p++")"

-- lazy operator
infixr 6 <$>
(<$>) :: String -> String -> String
[] <$> a = a
a  <$> b = a ++ " $ " ++ b

-- ---------------------------------------------------------------------
--
-- This bit of syntax constructs a [Dynamic].
--
infixr 6 !
(!) :: Typeable a => a -> [Dynamic] -> [Dynamic]
a ! xs = toDyn a : xs

