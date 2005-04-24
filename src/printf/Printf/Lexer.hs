{-# OPTIONS -fglasgow-exts -cpp #-}
{-# LINE 25 "Printf/Lexer.x" #-}

{-# OPTIONS -w #-}
-- ^ don't want to see all the warns alex templates produce

module Printf.Lexer ( scan, Token(..) ) where


#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif
alex_base :: AlexAddr
alex_base = AlexA# "\xf7\xff\xe2\xff\xef\xff\xf9\xff\x04\x00\x00\x00\xe6\xff\xfa\xff\x00\x00\x00\x00\x00\x00"#

alex_table :: AlexAddr
alex_table = AlexA# "\x00\x00\xff\xff\x06\x00\xff\xff\x00\x00\x06\x00\x06\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x06\x00\xff\xff\x06\x00\x00\x00\x06\x00\x06\x00\x06\x00\x0a\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x08\x00\xff\xff\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\xff\xff\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x0a\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x0a\x00\x0a\x00\x0a\x00\x0a\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x0a\x00\x0a\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

alex_check :: AlexAddr
alex_check = AlexA# "\xff\xff\x0a\x00\x20\x00\x0a\x00\xff\xff\x23\x00\x20\x00\xff\xff\xff\xff\x23\x00\xff\xff\xff\xff\xff\xff\x2b\x00\x0a\x00\x2d\x00\xff\xff\x2b\x00\x30\x00\x2d\x00\x25\x00\xff\xff\x30\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x2e\x00\x25\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x25\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x45\x00\xff\xff\x47\x00\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x58\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\xff\xff\xff\xff\x6c\x00\xff\xff\x6e\x00\x6f\x00\x70\x00\xff\xff\xff\xff\x73\x00\xff\xff\x75\x00\xff\xff\xff\xff\x78\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_deflt :: AlexAddr
alex_deflt = AlexA# "\x04\x00\xff\xff\xff\xff\x04\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_accept = listArray (0::Int,10) [[],[(AlexAcc (alex_action_2))],[],[],[(AlexAcc (alex_action_0))],[(AlexAcc (alex_action_1))],[(AlexAcc (alex_action_2))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_6))]]
{-# LINE 54 "Printf/Lexer.x" #-}


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



flag,fmt :: Int
flag = 1
fmt = 2
alex_action_0 = mkstr      
alex_action_1 = begin flag 
alex_action_2 = mkflags `andBegin` fmt 
alex_action_3 = mkint     
alex_action_4 = mkdot     
alex_action_5 = mklength  
alex_action_6 = mkconv `andBegin` 0 
{-# LINE 1 "GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine













{-# LINE 34 "GenericTemplate.hs" #-}












data AlexAddr = AlexA# Addr#

{-# INLINE alexIndexShortOffAddr #-}
alexIndexShortOffAddr (AlexA# arr) off =
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




-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> Maybe (AlexInput,Int,act)
alexScan input (I# (sc))
  = alexScanUser undefined input (I# (sc))

alexScanUser user input (I# (sc))
  = case alex_scan_tkn user input 0# input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  case s of 
    -1# -> (last_acc, input)
    _ -> alex_scan_tkn' user orig_input len input s last_acc

alex_scan_tkn' user orig_input len input s last_acc =
  let 
	new_acc = check_accs (alex_accept `unsafeAt` (I# (s)))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		base   = alexIndexShortOffAddr alex_base s
		(I# (ord_c)) = ord c
		offset = (base +# ord_c)
		check  = alexIndexShortOffAddr alex_check offset
		
		new_s = if (offset >=# 0#) && (check ==# ord_c)
			  then alexIndexShortOffAddr alex_table offset
			  else alexIndexShortOffAddr alex_deflt s
	in
	alex_scan_tkn user orig_input (len +# 1#) new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (I# (len))
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (I# (len)) input
	   = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (I# (len)) input
	   = AlexLastSkip input (I# (len))
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (I# (sc)) user _ _ input = 
     case alex_scan_tkn user input 0# input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (I# (i)) = i
{-# LINE 1 "wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn, 	-- current position,
		  Char,		-- previous char
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
				Just (c, (p', c, s))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
	deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad


data AlexState = AlexState {
	alex_pos :: !AlexPosn,	-- position at current input location
	alex_inp :: String,	-- the current input
	alex_chr :: !Char,	-- the character before the input
	alex_scd :: !Int 	-- the current startcode
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either String a
runAlex input (Alex f) 
   = case f (AlexState {alex_pos = alexStartPos,
 			alex_inp = input,	
			alex_chr = '\n',
			alex_scd = 0}) of Left msg -> Left msg
					  Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
				Left msg -> Left msg
				Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} -> 
	Right (s, (pos,c,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
		  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError inp' -> alexError "lexical error"
    AlexSkip  inp' len -> do
	alexSetInput inp'
	alexMonadScan
    AlexToken inp' len action -> do
	alexSetInput inp'
	action inp len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
-- andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

-- token :: (String -> Int -> token) -> AlexAction token
token t input len = return (t input len)


-- -----------------------------------------------------------------------------
-- Basic wrapper

{-# LINE 146 "wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

{-# LINE 162 "wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

{-# LINE 180 "wrappers.hs" #-}

