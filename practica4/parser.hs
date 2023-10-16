{-# OPTIONS_GHC -w #-}
module Main where

import Data.Char
import Data.List (nub)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,67) ([19008,4,128,0,512,0,0,1359,40448,10,0,9504,2,512,2048,0,1024,0,0,0,0,0,0,0,608,61440,84,43488,512,0,9728,0,0,35144,0,5436,0,0,8192,32768,9,4393,20992,34,32768,0,0,0,0,608,0,0,2,10496,17,16384,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","C","B","E","if","then","else","while","do","skip","true","false","L","int","'='","';'","'('","')'","'-'","'+'","'&'","\":=\"","%eof"]
        bit_start = st * 25
        bit_end = (st + 1) * 25
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..24]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (7) = happyShift action_4
action_0 (10) = happyShift action_5
action_0 (12) = happyShift action_6
action_0 (15) = happyShift action_2
action_0 (19) = happyShift action_7
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (15) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (24) = happyShift action_19
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (25) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (13) = happyShift action_11
action_4 (14) = happyShift action_12
action_4 (15) = happyShift action_13
action_4 (16) = happyShift action_14
action_4 (19) = happyShift action_15
action_4 (21) = happyShift action_16
action_4 (23) = happyShift action_17
action_4 (5) = happyGoto action_18
action_4 (6) = happyGoto action_10
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (13) = happyShift action_11
action_5 (14) = happyShift action_12
action_5 (15) = happyShift action_13
action_5 (16) = happyShift action_14
action_5 (19) = happyShift action_15
action_5 (21) = happyShift action_16
action_5 (23) = happyShift action_17
action_5 (5) = happyGoto action_9
action_5 (6) = happyGoto action_10
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 (7) = happyShift action_4
action_7 (10) = happyShift action_5
action_7 (12) = happyShift action_6
action_7 (15) = happyShift action_2
action_7 (19) = happyShift action_7
action_7 (4) = happyGoto action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (18) = happyShift action_27
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (11) = happyShift action_26
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (17) = happyShift action_25
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_6

action_12 _ = happyReduce_7

action_13 _ = happyReduce_11

action_14 _ = happyReduce_12

action_15 (15) = happyShift action_13
action_15 (16) = happyShift action_14
action_15 (19) = happyShift action_15
action_15 (6) = happyGoto action_24
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (13) = happyShift action_11
action_16 (14) = happyShift action_12
action_16 (15) = happyShift action_13
action_16 (16) = happyShift action_14
action_16 (19) = happyShift action_15
action_16 (21) = happyShift action_16
action_16 (23) = happyShift action_17
action_16 (5) = happyGoto action_23
action_16 (6) = happyGoto action_10
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (13) = happyShift action_11
action_17 (14) = happyShift action_12
action_17 (15) = happyShift action_13
action_17 (16) = happyShift action_14
action_17 (19) = happyShift action_15
action_17 (21) = happyShift action_16
action_17 (23) = happyShift action_17
action_17 (5) = happyGoto action_22
action_17 (6) = happyGoto action_10
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (8) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (15) = happyShift action_13
action_19 (16) = happyShift action_14
action_19 (19) = happyShift action_15
action_19 (6) = happyGoto action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_1

action_21 (7) = happyShift action_4
action_21 (10) = happyShift action_5
action_21 (12) = happyShift action_6
action_21 (15) = happyShift action_2
action_21 (19) = happyShift action_7
action_21 (4) = happyGoto action_33
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (13) = happyShift action_11
action_22 (14) = happyShift action_12
action_22 (15) = happyShift action_13
action_22 (16) = happyShift action_14
action_22 (19) = happyShift action_15
action_22 (21) = happyShift action_16
action_22 (23) = happyShift action_17
action_22 (5) = happyGoto action_32
action_22 (6) = happyGoto action_10
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_10

action_24 (22) = happyShift action_31
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (15) = happyShift action_13
action_25 (16) = happyShift action_14
action_25 (19) = happyShift action_15
action_25 (6) = happyGoto action_30
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (7) = happyShift action_4
action_26 (10) = happyShift action_5
action_26 (12) = happyShift action_6
action_26 (15) = happyShift action_2
action_26 (19) = happyShift action_7
action_26 (4) = happyGoto action_29
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (7) = happyShift action_4
action_27 (10) = happyShift action_5
action_27 (12) = happyShift action_6
action_27 (15) = happyShift action_2
action_27 (19) = happyShift action_7
action_27 (4) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (20) = happyShift action_36
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_4

action_30 _ = happyReduce_8

action_31 (15) = happyShift action_13
action_31 (16) = happyShift action_14
action_31 (19) = happyShift action_15
action_31 (6) = happyGoto action_35
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_9

action_33 (9) = happyShift action_34
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (7) = happyShift action_4
action_34 (10) = happyShift action_5
action_34 (12) = happyShift action_6
action_34 (15) = happyShift action_2
action_34 (19) = happyShift action_7
action_34 (4) = happyGoto action_38
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (20) = happyShift action_37
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_3

action_37 _ = happyReduce_13

action_38 _ = happyReduce_2

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (Loc happy_var_1))
	 =  HappyAbsSyn4
		 (AssignASA (LocASA happy_var_1) happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 5 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (SeqASA happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 4 happyReduction_4
happyReduction_4 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (WhileDo happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn4
		 (SkipASA
	)

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn5
		 (BoolASA True
	)

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn5
		 (BoolASA False
	)

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (EqualASA happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (AndASA happy_var_2 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (NotASA happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 (HappyTerminal (Loc happy_var_1))
	 =  HappyAbsSyn6
		 (LocASA happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyTerminal (Number happy_var_1))
	 =  HappyAbsSyn6
		 (NumberASA happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 6 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SumASA happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 25 25 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	If -> cont 7;
	Then -> cont 8;
	Else -> cont 9;
	While -> cont 10;
	Do -> cont 11;
	Skip -> cont 12;
	Boolean True -> cont 13;
	Boolean False -> cont 14;
	Loc happy_dollar_dollar -> cont 15;
	Number happy_dollar_dollar -> cont 16;
	Equal -> cont 17;
	Seq -> cont 18;
	LP -> cont 19;
	RP -> cont 20;
	Not -> cont 21;
	Sum -> cont 22;
	And -> cont 23;
	Assign -> cont 24;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 25 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data C = AssignASA E E | IfThenElse B C C | SeqASA C C | WhileDo B C | SkipASA deriving (Eq, Show)

data B = BoolASA Bool | EqualASA E E | AndASA B B | NotASA B deriving (Eq, Show)

data E = LocASA Int | NumberASA Int | SumASA E E deriving (Eq, Show)

data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP | Sum deriving (Eq, Show)

lexer :: String -> [Token]
lexer [] = []
lexer (' ':xs) = lexer xs
lexer ('\n':xs) = lexer xs
lexer ('\t':xs) = lexer xs
lexer (';':xs) = Seq : lexer xs
lexer ('&':xs) = And : lexer xs
lexer ('-':xs) = Not : lexer xs
lexer ('&':xs) = Equal : lexer xs
lexer (':':'=':xs) = Assign : lexer xs
lexer ('s':'k':'i':'p':xs) = Skip : lexer xs
lexer ('i':'f':xs) = If : lexer xs
lexer ('t':'h':'e':'n':xs) = Then : lexer xs
lexer ('e':'l':'s':'e':xs) = Else : lexer xs
lexer ('w':'h':'i':'l':'e':xs) = While : lexer xs
lexer ('d':'o':xs) = Do : lexer xs
lexer ('(':xs) = LP : lexer xs
lexer (')':xs) = RP : lexer xs
lexer ('+':xs) = Sum : lexer xs
lexer ('t':'r':'u':'e':xs) = Boolean True : lexer xs
lexer ('f':'a':'l':'s':'e':xs) = Boolean False : lexer xs
lexer (x:xs)
    | isDigit x = Number (read (x : takeWhile isDigit xs)) : lexer (dropWhile isDigit xs)
    | x == 'L' = Loc (read (takeWhile isDigit xs)) : lexer (dropWhile isDigit xs)
    | otherwise = error ("lexer: unexpected character " ++ [x])

main = getContents >>= print . parser . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
