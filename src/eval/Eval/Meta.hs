{-# OPTIONS -cpp -fth #-}
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
-- an implementation of the staged compilation primitives from 
--      "Dynamic Typing as Staged Type Inference"
--      Shields, Sheard and Jones, 1998
--      http://doi.acm.org/10.1145/268946.268970
--

module Eval.Meta ( 

        run,
        defer,
        splice,

    ) where

import Eval.Haskell             ( eval )
import AltData.Typeable         ( Typeable )

#if __GLASGOW_HASKELL__ > 602
import Language.Haskell.TH       ( ExpQ, pprint, runQ )
#else
import Language.Haskell.THSyntax ( ExpQ, pprExp, runQ )
import Text.PrettyPrint          ( render )
#endif

import System.IO.Unsafe         ( unsafePerformIO )

type ExpR = String -- hack for splicing

--
-- defer the evaluation of an expression by one stage.
-- uses [| |] just for the nice syntax.
--
-- defer [| 1 + 1 |] --> (1 + 1)
--
defer :: ExpQ -> ExpR
#if __GLASGOW_HASKELL__ > 602
defer e = pprint (unsafePerformIO (runQ e))
#else
defer e = render $ pprExp (unsafePerformIO (runQ e))
#endif

--
-- evaluate 'e' to a deferred expression, and evaluate the result.
--
-- run( defer [|1+1|] ) --> 2
--
run :: (Show t, Typeable t) => ExpR -> t
run e = case unsafePerformIO (eval e imports) of
                Nothing -> error "source failed to compile"
                Just a  -> a

--
-- evaluate 'e' to a deferred expression. then splice the result back in
-- to the surrounding deferred expression. splice() is only legal within
-- deferred expressions.
--
-- let code = defer [| 1 + 1 |] in defer [| splice(code) + 2 |]
--      -->
-- defer [| 1 + 1 + 2 |]
--
-- defer( "\x -> " ++ splice (v) )
--
splice :: Show t => t -> ExpR
splice e = show e

--
-- libraries needed
--
imports = 
        [
        "GHC.Base",
        "GHC.Num",
        "GHC.List"
        ]

