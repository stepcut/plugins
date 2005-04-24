{-# OPTIONS -fglasgow-exts -cpp  -fno-warn-name-shadowing -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-incomplete-patterns    #-}
-- parser produced by Happy Version 1.14


-- ^ grr. happy needs them all on one line

module Printf.Parser where

import Printf.Lexer
import Array
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
happyIn4 :: ([Format]) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ([Format])
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Format) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Format)
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Format) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Format)
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Format) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Format)
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ([Flag]) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ([Flag])
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Maybe Prec) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Maybe Prec)
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Maybe Width) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Maybe Width)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Length) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Length)
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Conv) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Conv)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x0f\x00\x00\x00\x14\x00\x0f\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x15\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x0a\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfe\xff\x00\x00\x00\x00\xfe\xff\xfc\xff\xfb\xff\xf3\xff\xfa\xff\xf7\xff\xef\xff\xf4\xff\xfd\xff\x00\x00\xf2\xff\xf1\xff\xf0\xff\xf5\xff\xef\xff\xf6\xff\xf8\xff\xee\xff\xed\xff\xec\xff\xeb\xff\xea\xff\xe9\xff\xe8\xff\xe7\xff\xe6\xff\xe5\xff\xe4\xff\xe3\xff\xe2\xff\xe1\xff\x00\x00\xf9\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x08\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x02\x00\x03\x00\x07\x00\x12\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x05\x00\x14\x00\x15\x00\x06\x00\x08\x00\x07\x00\x13\x00\x13\x00\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x23\x00\x0e\x00\x0f\x00\x10\x00\x0b\x00\x03\x00\x04\x00\x05\x00\x06\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x0e\x00\x0f\x00\x10\x00\x22\x00\x11\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x11\x00\x08\x00\x09\x00\x09\x00\x13\x00\x0c\x00\x13\x00\x0b\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 30) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30)
	]

happy_n_terms = 23 :: Int
happy_n_nonterms = 9 :: Int

happyReduce_1 = happySpecReduce_0 0# happyReduction_1
happyReduction_1  =  happyIn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2 0# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_3 = happySpecReduce_1 1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_1 1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_5 = happySpecReduce_1 2# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { (StrT   happy_var_1) -> 
	happyIn6
		 (StrLit happy_var_1
	)}

happyReduce_6 = happyReduce 6# 3# happyReduction_6
happyReduction_6 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	case happyOut11 happy_x_5 of { happy_var_5 -> 
	case happyOut12 happy_x_6 of { happy_var_6 -> 
	happyIn7
		 (ConvSp happy_var_1 happy_var_2 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}

happyReduce_7 = happyReduce 4# 3# happyReduction_7
happyReduction_7 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	happyIn7
		 (ConvSp happy_var_1 happy_var_2 Nothing happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_8 = happySpecReduce_1 4# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOutTok happy_x_1 of { (FlagT happy_var_1) -> 
	happyIn8
		 (mkFlags happy_var_1
	)}

happyReduce_9 = happySpecReduce_1 5# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { (IntT   happy_var_1) -> 
	happyIn9
		 (Just happy_var_1
	)}

happyReduce_10 = happySpecReduce_0 5# happyReduction_10
happyReduction_10  =  happyIn9
		 (Nothing
	)

happyReduce_11 = happySpecReduce_1 6# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { (IntT   happy_var_1) -> 
	happyIn10
		 (Just happy_var_1
	)}

happyReduce_12 = happySpecReduce_0 6# happyReduction_12
happyReduction_12  =  happyIn10
		 (Nothing
	)

happyReduce_13 = happySpecReduce_1 7# happyReduction_13
happyReduction_13 happy_x_1
	 =  happyIn11
		 (Short
	)

happyReduce_14 = happySpecReduce_1 7# happyReduction_14
happyReduction_14 happy_x_1
	 =  happyIn11
		 (Long
	)

happyReduce_15 = happySpecReduce_1 7# happyReduction_15
happyReduction_15 happy_x_1
	 =  happyIn11
		 (Double
	)

happyReduce_16 = happySpecReduce_0 7# happyReduction_16
happyReduction_16  =  happyIn11
		 (Default
	)

happyReduce_17 = happySpecReduce_1 8# happyReduction_17
happyReduction_17 happy_x_1
	 =  happyIn12
		 (D
	)

happyReduce_18 = happySpecReduce_1 8# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn12
		 (D
	)

happyReduce_19 = happySpecReduce_1 8# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn12
		 (O
	)

happyReduce_20 = happySpecReduce_1 8# happyReduction_20
happyReduction_20 happy_x_1
	 =  happyIn12
		 (Xx
	)

happyReduce_21 = happySpecReduce_1 8# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn12
		 (XX
	)

happyReduce_22 = happySpecReduce_1 8# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn12
		 (U
	)

happyReduce_23 = happySpecReduce_1 8# happyReduction_23
happyReduction_23 happy_x_1
	 =  happyIn12
		 (C
	)

happyReduce_24 = happySpecReduce_1 8# happyReduction_24
happyReduction_24 happy_x_1
	 =  happyIn12
		 (S
	)

happyReduce_25 = happySpecReduce_1 8# happyReduction_25
happyReduction_25 happy_x_1
	 =  happyIn12
		 (F
	)

happyReduce_26 = happySpecReduce_1 8# happyReduction_26
happyReduction_26 happy_x_1
	 =  happyIn12
		 (Ee
	)

happyReduce_27 = happySpecReduce_1 8# happyReduction_27
happyReduction_27 happy_x_1
	 =  happyIn12
		 (EE
	)

happyReduce_28 = happySpecReduce_1 8# happyReduction_28
happyReduction_28 happy_x_1
	 =  happyIn12
		 (Gg
	)

happyReduce_29 = happySpecReduce_1 8# happyReduction_29
happyReduction_29 happy_x_1
	 =  happyIn12
		 (GG
	)

happyReduce_30 = happySpecReduce_1 8# happyReduction_30
happyReduction_30 happy_x_1
	 =  happyIn12
		 (Percent
	)

happyNewToken action sts stk [] =
	happyDoAction 22# (error "reading EOF!") action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	LengthT 'h' -> cont 1#;
	LengthT 'l' -> cont 2#;
	LengthT 'L' -> cont 3#;
	ConvT 'd' -> cont 4#;
	ConvT 'i' -> cont 5#;
	ConvT 'o' -> cont 6#;
	ConvT 'x' -> cont 7#;
	ConvT 'X' -> cont 8#;
	ConvT 'u' -> cont 9#;
	ConvT 'c' -> cont 10#;
	ConvT 's' -> cont 11#;
	ConvT 'f' -> cont 12#;
	ConvT 'e' -> cont 13#;
	ConvT 'E' -> cont 14#;
	ConvT 'g' -> cont 15#;
	ConvT 'G' -> cont 16#;
	ConvT '%' -> cont 17#;
	DotT -> cont 18#;
	IntT   happy_dollar_dollar -> cont 19#;
	StrT   happy_dollar_dollar -> cont 20#;
	FlagT happy_dollar_dollar -> cont 21#;
	_ -> happyError tks
	}

happyThen = \m k -> k m
happyReturn = \a -> a
happyThen1 = happyThen
happyReturn1 = \a tks -> a

parse tks = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq

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
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id: Parser.hs,v 1.1 2004/06/28 03:56:01 dons Exp $













{-# LINE 27 "GenericTemplate.hs" #-}



data Happy_IntList = HappyCons Int# Happy_IntList






































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = (happyTcHack j 
				                  (happyTcHack st))
					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st











indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 165 "GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
