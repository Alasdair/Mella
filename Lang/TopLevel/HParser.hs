{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.TopLevel.HParser where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T

import Lang.TopLevel.Tokenizer

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 ([Command])
	| HappyAbsSyn7 (Command)
	| HappyAbsSyn8 ([Expr])
	| HappyAbsSyn9 (Expr)
	| HappyAbsSyn10 ([(Text, [Expr])])
	| HappyAbsSyn11 ((Text, [Expr]))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39 :: () => Int -> ({-HappyReduction (Maybe) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Maybe) HappyAbsSyn)

happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19 :: () => ({-HappyReduction (Maybe) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Maybe) HappyAbsSyn)

action_0 (12) = happyShift action_4
action_0 (13) = happyShift action_5
action_0 (14) = happyShift action_6
action_0 (18) = happyShift action_7
action_0 (19) = happyShift action_8
action_0 (7) = happyGoto action_12
action_0 _ = happyFail

action_1 (12) = happyShift action_4
action_1 (13) = happyShift action_5
action_1 (14) = happyShift action_6
action_1 (18) = happyShift action_7
action_1 (19) = happyShift action_8
action_1 (5) = happyGoto action_9
action_1 (6) = happyGoto action_10
action_1 (7) = happyGoto action_11
action_1 _ = happyReduce_5

action_2 (12) = happyShift action_4
action_2 (13) = happyShift action_5
action_2 (14) = happyShift action_6
action_2 (18) = happyShift action_7
action_2 (19) = happyShift action_8
action_2 (7) = happyGoto action_3
action_2 _ = happyFail

action_3 (12) = happyShift action_4
action_3 (13) = happyShift action_5
action_3 (14) = happyShift action_6
action_3 (18) = happyShift action_7
action_3 (19) = happyShift action_8
action_3 (5) = happyGoto action_13
action_3 (7) = happyGoto action_11
action_3 _ = happyFail

action_4 (18) = happyShift action_23
action_4 _ = happyFail

action_5 (18) = happyShift action_22
action_5 _ = happyFail

action_6 (16) = happyShift action_21
action_6 _ = happyFail

action_7 (17) = happyShift action_17
action_7 (18) = happyShift action_18
action_7 (19) = happyShift action_19
action_7 (20) = happyShift action_20
action_7 (8) = happyGoto action_15
action_7 (9) = happyGoto action_16
action_7 _ = happyReduce_12

action_8 (16) = happyShift action_14
action_8 _ = happyFail

action_9 _ = happyReduce_4

action_10 (23) = happyAccept
action_10 _ = happyFail

action_11 (12) = happyShift action_4
action_11 (13) = happyShift action_5
action_11 (14) = happyShift action_6
action_11 (18) = happyShift action_7
action_11 (19) = happyShift action_8
action_11 (5) = happyGoto action_13
action_11 (7) = happyGoto action_11
action_11 _ = happyReduce_3

action_12 (23) = happyAccept
action_12 _ = happyFail

action_13 _ = happyReduce_2

action_14 _ = happyReduce_9

action_15 (22) = happyShift action_30
action_15 (10) = happyGoto action_28
action_15 (11) = happyGoto action_29
action_15 _ = happyReduce_18

action_16 (17) = happyShift action_17
action_16 (18) = happyShift action_18
action_16 (19) = happyShift action_19
action_16 (20) = happyShift action_20
action_16 (8) = happyGoto action_27
action_16 (9) = happyGoto action_16
action_16 _ = happyReduce_12

action_17 _ = happyReduce_15

action_18 _ = happyReduce_14

action_19 _ = happyReduce_13

action_20 (17) = happyShift action_17
action_20 (18) = happyShift action_18
action_20 (19) = happyShift action_19
action_20 (20) = happyShift action_20
action_20 (8) = happyGoto action_26
action_20 (9) = happyGoto action_16
action_20 _ = happyReduce_12

action_21 _ = happyReduce_8

action_22 (15) = happyShift action_25
action_22 _ = happyFail

action_23 (15) = happyShift action_24
action_23 _ = happyFail

action_24 (19) = happyShift action_36
action_24 _ = happyFail

action_25 (19) = happyShift action_35
action_25 _ = happyFail

action_26 (21) = happyShift action_34
action_26 _ = happyFail

action_27 _ = happyReduce_11

action_28 (16) = happyShift action_33
action_28 _ = happyFail

action_29 (22) = happyShift action_30
action_29 (10) = happyGoto action_32
action_29 (11) = happyGoto action_29
action_29 _ = happyReduce_18

action_30 (17) = happyShift action_17
action_30 (18) = happyShift action_18
action_30 (19) = happyShift action_19
action_30 (20) = happyShift action_20
action_30 (8) = happyGoto action_31
action_30 (9) = happyGoto action_16
action_30 _ = happyReduce_12

action_31 _ = happyReduce_19

action_32 _ = happyReduce_17

action_33 _ = happyReduce_10

action_34 _ = happyReduce_16

action_35 (16) = happyShift action_38
action_35 _ = happyFail

action_36 (19) = happyShift action_37
action_36 _ = happyFail

action_37 (16) = happyShift action_39
action_37 _ = happyFail

action_38 _ = happyReduce_7

action_39 _ = happyReduce_6

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn5
		 ([]
	)

happyReduce_6 = happyReduce 6 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyTerminal (TokTerm happy_var_5)) `HappyStk`
	(HappyTerminal (TokTerm happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (CmdFun happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyTerminal (TokTerm happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (CmdTheorem happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 _
	_
	 =  HappyAbsSyn7
		 (CmdRefl
	)

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 _
	(HappyTerminal (TokTerm happy_var_1))
	 =  HappyAbsSyn7
		 (CmdTerm happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal (TokIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (CmdExpr happy_var_1 happy_var_2 (Map.fromList happy_var_3)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  8 happyReduction_12
happyReduction_12  =  HappyAbsSyn8
		 ([]
	)

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyTerminal (TokTerm happy_var_1))
	 =  HappyAbsSyn9
		 (ExprTerm happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyTerminal (TokIdent happy_var_1))
	 =  HappyAbsSyn9
		 (ExprIdent happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyTerminal (TokQuote happy_var_1))
	 =  HappyAbsSyn9
		 (ExprString happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (ExprList happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  10 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  10 happyReduction_18
happyReduction_18  =  HappyAbsSyn10
		 ([]
	)

happyReduce_19 = happySpecReduce_2  11 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokKeyword happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1, happy_var_2)
	)
happyReduction_19 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 23 23 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokFun -> cont 12;
	TokTheorem -> cont 13;
	TokRefl -> cont 14;
	TokColon -> cont 15;
	TokPeriod -> cont 16;
	TokQuote happy_dollar_dollar -> cont 17;
	TokIdent happy_dollar_dollar -> cont 18;
	TokTerm happy_dollar_dollar -> cont 19;
	TokOpen -> cont 20;
	TokClose -> cont 21;
	TokKeyword happy_dollar_dollar -> cont 22;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Maybe a -> (a -> Maybe b) -> Maybe b
happyThen = (>>=)
happyReturn :: () => a -> Maybe a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Maybe a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> Maybe a
happyError' = parseError

parseCommand tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parseCommands tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Command = CmdTerm Text
             | CmdTheorem Text Text
             | CmdFun Text Text Text
             | CmdRefl
             | CmdExpr Text [Expr] (Map Text [Expr])
             deriving (Show)

data Expr = ExprIdent Text
          | ExprTerm Text
          | ExprString Text
          | ExprList [Expr]
          deriving (Show)

parseError :: [Token] -> Maybe a
parseError _ = Nothing
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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

{-# LINE 311 "templates/GenericTemplate.hs" #-}
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
