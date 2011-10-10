{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Util.WaldmeisterInput.HParser
    ( parseWMFile
    , parseFuns
    , WMFile (..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Tree

import Lang.Error
import Lang.PrettyPrint
import Lang.Util.WaldmeisterInput.Tokenizer

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 (WMFile)
	| HappyAbsSyn6 ([Text])
	| HappyAbsSyn9 (Text)
	| HappyAbsSyn10 ((Text, [Text]))
	| HappyAbsSyn11 ([(Text, [Text])])
	| HappyAbsSyn12 (Bool)
	| HappyAbsSyn14 ([(Text, Int)])
	| HappyAbsSyn16 ((Text, Int))
	| HappyAbsSyn20 (Tree Text)
	| HappyAbsSyn21 (Forest Text)
	| HappyAbsSyn22 ((Tree Text, Tree Text))
	| HappyAbsSyn23 ([(Tree Text, Tree Text)])

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
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66 :: () => Int -> ({-HappyReduction (Error) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Error) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Error) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Error) HappyAbsSyn)

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
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33 :: () => ({-HappyReduction (Error) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Error) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Error) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Error) HappyAbsSyn)

action_0 (24) = happyShift action_3
action_0 (5) = happyGoto action_9
action_0 _ = happyFail

action_1 (26) = happyShift action_7
action_1 (41) = happyShift action_8
action_1 (9) = happyGoto action_4
action_1 (10) = happyGoto action_5
action_1 (11) = happyGoto action_6
action_1 _ = happyFail

action_2 (24) = happyShift action_3
action_2 _ = happyFail

action_3 (25) = happyShift action_12
action_3 _ = happyFail

action_4 (28) = happyShift action_11
action_4 _ = happyFail

action_5 (26) = happyShift action_7
action_5 (41) = happyShift action_8
action_5 (9) = happyGoto action_4
action_5 (10) = happyGoto action_5
action_5 (11) = happyGoto action_10
action_5 _ = happyReduce_12

action_6 (43) = happyAccept
action_6 _ = happyFail

action_7 _ = happyReduce_8

action_8 _ = happyReduce_9

action_9 (43) = happyAccept
action_9 _ = happyFail

action_10 _ = happyReduce_11

action_11 (26) = happyShift action_7
action_11 (41) = happyShift action_8
action_11 (8) = happyGoto action_15
action_11 (9) = happyGoto action_16
action_11 _ = happyReduce_7

action_12 (27) = happyShift action_14
action_12 (6) = happyGoto action_13
action_12 _ = happyFail

action_13 (30) = happyShift action_21
action_13 _ = happyFail

action_14 (26) = happyShift action_7
action_14 (41) = happyShift action_8
action_14 (7) = happyGoto action_19
action_14 (9) = happyGoto action_20
action_14 _ = happyFail

action_15 (29) = happyShift action_18
action_15 _ = happyFail

action_16 (26) = happyShift action_7
action_16 (41) = happyShift action_8
action_16 (8) = happyGoto action_17
action_16 (9) = happyGoto action_16
action_16 _ = happyReduce_7

action_17 _ = happyReduce_6

action_18 (26) = happyShift action_7
action_18 (41) = happyShift action_8
action_18 (9) = happyGoto action_24
action_18 _ = happyFail

action_19 _ = happyReduce_3

action_20 (26) = happyShift action_7
action_20 (41) = happyShift action_8
action_20 (7) = happyGoto action_23
action_20 (9) = happyGoto action_20
action_20 _ = happyReduce_5

action_21 (26) = happyShift action_7
action_21 (41) = happyShift action_8
action_21 (9) = happyGoto action_4
action_21 (10) = happyGoto action_5
action_21 (11) = happyGoto action_22
action_21 _ = happyFail

action_22 (31) = happyShift action_25
action_22 _ = happyFail

action_23 _ = happyReduce_4

action_24 _ = happyReduce_10

action_25 (32) = happyShift action_27
action_25 (33) = happyShift action_28
action_25 (12) = happyGoto action_26
action_25 _ = happyFail

action_26 (26) = happyShift action_7
action_26 (41) = happyShift action_8
action_26 (9) = happyGoto action_29
action_26 (14) = happyGoto action_30
action_26 (15) = happyGoto action_31
action_26 (16) = happyGoto action_32
action_26 _ = happyFail

action_27 _ = happyReduce_14

action_28 _ = happyReduce_13

action_29 (38) = happyShift action_36
action_29 _ = happyFail

action_30 (26) = happyShift action_7
action_30 (41) = happyShift action_8
action_30 (9) = happyGoto action_34
action_30 (13) = happyGoto action_35
action_30 _ = happyFail

action_31 _ = happyReduce_17

action_32 (36) = happyShift action_33
action_32 _ = happyReduce_20

action_33 (26) = happyShift action_7
action_33 (41) = happyShift action_8
action_33 (9) = happyGoto action_29
action_33 (15) = happyGoto action_40
action_33 (16) = happyGoto action_32
action_33 _ = happyFail

action_34 (34) = happyShift action_39
action_34 _ = happyReduce_16

action_35 (35) = happyShift action_38
action_35 _ = happyFail

action_36 (41) = happyShift action_37
action_36 _ = happyFail

action_37 _ = happyReduce_21

action_38 (26) = happyShift action_7
action_38 (41) = happyShift action_8
action_38 (9) = happyGoto action_42
action_38 (17) = happyGoto action_43
action_38 (18) = happyGoto action_44
action_38 (19) = happyGoto action_45
action_38 _ = happyFail

action_39 (26) = happyShift action_7
action_39 (41) = happyShift action_8
action_39 (9) = happyGoto action_34
action_39 (13) = happyGoto action_41
action_39 _ = happyFail

action_40 _ = happyReduce_19

action_41 _ = happyReduce_15

action_42 (36) = happyShift action_49
action_42 _ = happyReduce_23

action_43 (28) = happyShift action_48
action_43 _ = happyFail

action_44 (26) = happyShift action_7
action_44 (41) = happyShift action_8
action_44 (9) = happyGoto action_42
action_44 (17) = happyGoto action_43
action_44 (18) = happyGoto action_44
action_44 (19) = happyGoto action_47
action_44 _ = happyReduce_26

action_45 (37) = happyShift action_46
action_45 _ = happyFail

action_46 (26) = happyShift action_7
action_46 (41) = happyShift action_8
action_46 (9) = happyGoto action_52
action_46 (20) = happyGoto action_53
action_46 (22) = happyGoto action_54
action_46 (23) = happyGoto action_55
action_46 _ = happyFail

action_47 _ = happyReduce_25

action_48 (26) = happyShift action_7
action_48 (41) = happyShift action_8
action_48 (9) = happyGoto action_51
action_48 _ = happyFail

action_49 (26) = happyShift action_7
action_49 (41) = happyShift action_8
action_49 (9) = happyGoto action_42
action_49 (17) = happyGoto action_50
action_49 _ = happyFail

action_50 _ = happyReduce_22

action_51 _ = happyReduce_24

action_52 (39) = happyShift action_59
action_52 _ = happyReduce_28

action_53 (38) = happyShift action_58
action_53 _ = happyFail

action_54 (26) = happyShift action_7
action_54 (41) = happyShift action_8
action_54 (9) = happyGoto action_52
action_54 (20) = happyGoto action_53
action_54 (22) = happyGoto action_54
action_54 (23) = happyGoto action_57
action_54 _ = happyReduce_33

action_55 (42) = happyShift action_56
action_55 _ = happyFail

action_56 (26) = happyShift action_7
action_56 (41) = happyShift action_8
action_56 (9) = happyGoto action_52
action_56 (20) = happyGoto action_53
action_56 (22) = happyGoto action_54
action_56 (23) = happyGoto action_63
action_56 _ = happyFail

action_57 _ = happyReduce_32

action_58 (26) = happyShift action_7
action_58 (41) = happyShift action_8
action_58 (9) = happyGoto action_52
action_58 (20) = happyGoto action_62
action_58 _ = happyFail

action_59 (26) = happyShift action_7
action_59 (41) = happyShift action_8
action_59 (9) = happyGoto action_52
action_59 (20) = happyGoto action_60
action_59 (21) = happyGoto action_61
action_59 _ = happyFail

action_60 (36) = happyShift action_65
action_60 _ = happyReduce_30

action_61 (40) = happyShift action_64
action_61 _ = happyFail

action_62 _ = happyReduce_31

action_63 _ = happyReduce_2

action_64 _ = happyReduce_27

action_65 (26) = happyShift action_7
action_65 (41) = happyShift action_8
action_65 (9) = happyGoto action_52
action_65 (20) = happyGoto action_60
action_65 (21) = happyGoto action_66
action_65 _ = happyFail

action_66 _ = happyReduce_29

happyReduce_2 = happyReduce 15 5 happyReduction_2
happyReduction_2 ((HappyAbsSyn23  happy_var_15) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_13) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_9) `HappyStk`
	(HappyAbsSyn14  happy_var_8) `HappyStk`
	(HappyAbsSyn12  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyTerminal (TokMode happy_var_2)) `HappyStk`
	(HappyTerminal (TokName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (WMFile happy_var_1 happy_var_2 happy_var_3 happy_var_5 happy_var_7 happy_var_8 happy_var_9 happy_var_11 happy_var_13 (head happy_var_15)
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  7 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  8 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn6
		 ([]
	)

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyTerminal (TokInt happy_var_1))
	 =  HappyAbsSyn9
		 (T.pack (show happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happyReduce 5 10 happyReduction_10
happyReduction_10 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_1, happy_var_3 ++ [happy_var_5])
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_2  11 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn12
		 (True
	)

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn12
		 (False
	)

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  14 happyReduction_18
happyReduction_18  =  HappyAbsSyn14
		 ([]
	)

happyReduce_19 = happySpecReduce_3  15 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 (HappyTerminal (TokInt happy_var_3))
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn16
		 ((happy_var_1, happy_var_3)
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  17 happyReduction_22
happyReduction_22 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  17 happyReduction_23
happyReduction_23 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  18 happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn10
		 ((happy_var_3, happy_var_1)
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  19 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  19 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 20 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Node happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  20 happyReduction_28
happyReduction_28 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn20
		 (Node happy_var_1 []
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  21 happyReduction_29
happyReduction_29 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  21 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  22 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn22
		 ((happy_var_1, happy_var_3)
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  23 happyReduction_32
happyReduction_32 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  23 happyReduction_33
happyReduction_33 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 43 43 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokName happy_dollar_dollar -> cont 24;
	TokMode happy_dollar_dollar -> cont 25;
	TokIdentifier happy_dollar_dollar -> cont 26;
	TokSorts -> cont 27;
	TokColon -> cont 28;
	TokArrow -> cont 29;
	TokSignature -> cont 30;
	TokOrdering -> cont 31;
	TokLPO -> cont 32;
	TokKBO -> cont 33;
	TokGT -> cont 34;
	TokVariables -> cont 35;
	TokComma -> cont 36;
	TokEquations -> cont 37;
	TokEq -> cont 38;
	TokOpenBracket -> cont 39;
	TokCloseBracket -> cont 40;
	TokInt happy_dollar_dollar -> cont 41;
	TokConclusion -> cont 42;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Error a -> (a -> Error b) -> Error b
happyThen = (>>=)
happyReturn :: () => a -> Error a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Error a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> Error a
happyError' = parseError

parseWMFile tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

parseFuns tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data WMFile = WMFile
    { wmName :: Text
    , wmMode :: Text
    , wmSorts :: [Text]
    , wmSig :: [(Text, [Text])]
    , wmKBO :: Bool
    , wmWeights :: [(Text, Int)]
    , wmOrdering :: [Text]
    , wmVariables :: [(Text, [Text])]
    , wmEquations :: [(Tree Text, Tree Text)]
    , wmConclusion :: (Tree Text, Tree Text)
    } deriving (Show)

data ParseError = PErr [Token]

instance ErrorMsg ParseError where
    toPrettyText (PErr tokens) sch =
        T.concat [ colorBanner sch errorColor "PARSE ERROR"
                 , "\nParser failed when parsing tokens:\n"
                 , T.pack (show (take 10 tokens))
                 ]

parseError :: [Token] -> Error a
parseError = throwError . PErr
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
