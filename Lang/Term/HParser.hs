{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Term.HParser 
    ( sugarParse
    , sugarParseVarList
    , SugarTerm (..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lang.Error
import Lang.PrettyPrint
import Lang.Term
import Lang.Term.Tokenizer

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 (SugarTerm)
	| HappyAbsSyn12 ([Text])
	| HappyAbsSyn13 ([SugarTerm])

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
 action_60 :: () => Int -> ({-HappyReduction (Error) = -}
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
 happyReduce_29 :: () => ({-HappyReduction (Error) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Error) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Error) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Error) HappyAbsSyn)

action_0 (15) = happyShift action_22
action_0 (16) = happyShift action_23
action_0 (18) = happyShift action_10
action_0 (19) = happyShift action_24
action_0 (21) = happyShift action_25
action_0 (22) = happyShift action_11
action_0 (23) = happyShift action_12
action_0 (24) = happyShift action_13
action_0 (25) = happyShift action_14
action_0 (26) = happyShift action_15
action_0 (28) = happyShift action_16
action_0 (5) = happyGoto action_17
action_0 (6) = happyGoto action_18
action_0 (7) = happyGoto action_19
action_0 (8) = happyGoto action_6
action_0 (9) = happyGoto action_20
action_0 (10) = happyGoto action_21
action_0 (11) = happyGoto action_3
action_0 _ = happyFail

action_1 (16) = happyShift action_9
action_1 (18) = happyShift action_10
action_1 (22) = happyShift action_11
action_1 (23) = happyShift action_12
action_1 (24) = happyShift action_13
action_1 (25) = happyShift action_14
action_1 (26) = happyShift action_15
action_1 (28) = happyShift action_16
action_1 (7) = happyGoto action_5
action_1 (8) = happyGoto action_6
action_1 (9) = happyGoto action_7
action_1 (13) = happyGoto action_8
action_1 _ = happyFail

action_2 (16) = happyShift action_4
action_2 (11) = happyGoto action_3
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (18) = happyShift action_32
action_4 (12) = happyGoto action_29
action_4 _ = happyFail

action_5 _ = happyReduce_21

action_6 _ = happyReduce_17

action_7 (16) = happyShift action_9
action_7 (18) = happyShift action_10
action_7 (22) = happyShift action_11
action_7 (23) = happyShift action_12
action_7 (24) = happyShift action_13
action_7 (25) = happyShift action_14
action_7 (26) = happyShift action_15
action_7 (28) = happyShift action_16
action_7 (7) = happyGoto action_5
action_7 (8) = happyGoto action_6
action_7 (9) = happyGoto action_7
action_7 (13) = happyGoto action_39
action_7 _ = happyReduce_29

action_8 (29) = happyAccept
action_8 _ = happyFail

action_9 (15) = happyShift action_22
action_9 (16) = happyShift action_23
action_9 (18) = happyShift action_10
action_9 (19) = happyShift action_24
action_9 (21) = happyShift action_25
action_9 (22) = happyShift action_11
action_9 (23) = happyShift action_12
action_9 (24) = happyShift action_13
action_9 (25) = happyShift action_14
action_9 (26) = happyShift action_15
action_9 (28) = happyShift action_16
action_9 (5) = happyGoto action_38
action_9 (6) = happyGoto action_18
action_9 (7) = happyGoto action_19
action_9 (8) = happyGoto action_6
action_9 (9) = happyGoto action_20
action_9 (10) = happyGoto action_21
action_9 (11) = happyGoto action_3
action_9 _ = happyFail

action_10 _ = happyReduce_12

action_11 (16) = happyShift action_9
action_11 (18) = happyShift action_10
action_11 (22) = happyShift action_11
action_11 (23) = happyShift action_12
action_11 (24) = happyShift action_13
action_11 (25) = happyShift action_14
action_11 (26) = happyShift action_15
action_11 (28) = happyShift action_16
action_11 (7) = happyGoto action_5
action_11 (8) = happyGoto action_6
action_11 (9) = happyGoto action_37
action_11 _ = happyFail

action_12 _ = happyReduce_18

action_13 _ = happyReduce_14

action_14 _ = happyReduce_13

action_15 _ = happyReduce_15

action_16 _ = happyReduce_16

action_17 (29) = happyAccept
action_17 _ = happyFail

action_18 (14) = happyShift action_36
action_18 _ = happyReduce_5

action_19 (16) = happyReduce_21
action_19 (18) = happyReduce_21
action_19 (22) = happyReduce_21
action_19 (23) = happyReduce_21
action_19 (24) = happyReduce_21
action_19 (25) = happyReduce_21
action_19 (26) = happyReduce_21
action_19 (27) = happyReduce_21
action_19 (28) = happyReduce_21
action_19 _ = happyReduce_9

action_20 (16) = happyShift action_9
action_20 (18) = happyShift action_10
action_20 (22) = happyShift action_11
action_20 (23) = happyShift action_12
action_20 (24) = happyShift action_13
action_20 (25) = happyShift action_14
action_20 (26) = happyShift action_15
action_20 (27) = happyShift action_35
action_20 (28) = happyShift action_16
action_20 (7) = happyGoto action_5
action_20 (8) = happyGoto action_6
action_20 (9) = happyGoto action_34
action_20 _ = happyFail

action_21 (16) = happyShift action_9
action_21 (18) = happyShift action_10
action_21 (22) = happyShift action_11
action_21 (23) = happyShift action_12
action_21 (24) = happyShift action_13
action_21 (25) = happyShift action_14
action_21 (26) = happyShift action_15
action_21 (28) = happyShift action_16
action_21 (7) = happyGoto action_5
action_21 (8) = happyGoto action_6
action_21 (9) = happyGoto action_33
action_21 _ = happyReduce_6

action_22 (18) = happyShift action_32
action_22 (12) = happyGoto action_31
action_22 _ = happyFail

action_23 (15) = happyShift action_22
action_23 (16) = happyShift action_23
action_23 (18) = happyShift action_30
action_23 (19) = happyShift action_24
action_23 (21) = happyShift action_25
action_23 (22) = happyShift action_11
action_23 (23) = happyShift action_12
action_23 (24) = happyShift action_13
action_23 (25) = happyShift action_14
action_23 (26) = happyShift action_15
action_23 (28) = happyShift action_16
action_23 (5) = happyGoto action_28
action_23 (6) = happyGoto action_18
action_23 (7) = happyGoto action_19
action_23 (8) = happyGoto action_6
action_23 (9) = happyGoto action_20
action_23 (10) = happyGoto action_21
action_23 (11) = happyGoto action_3
action_23 (12) = happyGoto action_29
action_23 _ = happyFail

action_24 (16) = happyShift action_9
action_24 (18) = happyShift action_10
action_24 (22) = happyShift action_11
action_24 (23) = happyShift action_12
action_24 (24) = happyShift action_13
action_24 (25) = happyShift action_14
action_24 (26) = happyShift action_15
action_24 (28) = happyShift action_16
action_24 (7) = happyGoto action_5
action_24 (8) = happyGoto action_6
action_24 (9) = happyGoto action_27
action_24 _ = happyFail

action_25 (16) = happyShift action_9
action_25 (18) = happyShift action_10
action_25 (22) = happyShift action_11
action_25 (23) = happyShift action_12
action_25 (24) = happyShift action_13
action_25 (25) = happyShift action_14
action_25 (26) = happyShift action_15
action_25 (28) = happyShift action_16
action_25 (7) = happyGoto action_5
action_25 (8) = happyGoto action_6
action_25 (9) = happyGoto action_26
action_25 _ = happyFail

action_26 (16) = happyShift action_9
action_26 (18) = happyShift action_10
action_26 (22) = happyShift action_11
action_26 (23) = happyShift action_12
action_26 (24) = happyShift action_13
action_26 (25) = happyShift action_14
action_26 (26) = happyShift action_15
action_26 (28) = happyShift action_16
action_26 (7) = happyGoto action_5
action_26 (8) = happyGoto action_6
action_26 (9) = happyGoto action_49
action_26 _ = happyFail

action_27 (16) = happyShift action_9
action_27 (18) = happyShift action_10
action_27 (22) = happyShift action_11
action_27 (23) = happyShift action_12
action_27 (24) = happyShift action_13
action_27 (25) = happyShift action_14
action_27 (26) = happyShift action_15
action_27 (28) = happyShift action_16
action_27 (7) = happyGoto action_5
action_27 (8) = happyGoto action_6
action_27 (9) = happyGoto action_48
action_27 _ = happyFail

action_28 (17) = happyShift action_47
action_28 _ = happyFail

action_29 (20) = happyShift action_46
action_29 _ = happyFail

action_30 (18) = happyShift action_32
action_30 (20) = happyReduce_27
action_30 (12) = happyGoto action_44
action_30 _ = happyReduce_12

action_31 (14) = happyShift action_45
action_31 _ = happyFail

action_32 (18) = happyShift action_32
action_32 (12) = happyGoto action_44
action_32 _ = happyReduce_27

action_33 _ = happyReduce_22

action_34 _ = happyReduce_23

action_35 (16) = happyShift action_9
action_35 (18) = happyShift action_10
action_35 (22) = happyShift action_11
action_35 (23) = happyShift action_12
action_35 (24) = happyShift action_13
action_35 (25) = happyShift action_14
action_35 (26) = happyShift action_15
action_35 (28) = happyShift action_16
action_35 (7) = happyGoto action_5
action_35 (8) = happyGoto action_6
action_35 (9) = happyGoto action_43
action_35 _ = happyFail

action_36 (15) = happyShift action_22
action_36 (16) = happyShift action_23
action_36 (18) = happyShift action_10
action_36 (19) = happyShift action_24
action_36 (21) = happyShift action_25
action_36 (22) = happyShift action_11
action_36 (23) = happyShift action_12
action_36 (24) = happyShift action_13
action_36 (25) = happyShift action_14
action_36 (26) = happyShift action_15
action_36 (28) = happyShift action_16
action_36 (5) = happyGoto action_42
action_36 (6) = happyGoto action_18
action_36 (7) = happyGoto action_19
action_36 (8) = happyGoto action_6
action_36 (9) = happyGoto action_20
action_36 (10) = happyGoto action_21
action_36 (11) = happyGoto action_3
action_36 _ = happyFail

action_37 (16) = happyShift action_9
action_37 (18) = happyShift action_10
action_37 (22) = happyShift action_11
action_37 (23) = happyShift action_12
action_37 (24) = happyShift action_13
action_37 (25) = happyShift action_14
action_37 (26) = happyShift action_15
action_37 (28) = happyShift action_16
action_37 (7) = happyGoto action_5
action_37 (8) = happyGoto action_6
action_37 (9) = happyGoto action_41
action_37 _ = happyFail

action_38 (17) = happyShift action_40
action_38 _ = happyFail

action_39 _ = happyReduce_28

action_40 _ = happyReduce_20

action_41 (16) = happyShift action_9
action_41 (18) = happyShift action_10
action_41 (22) = happyShift action_11
action_41 (23) = happyShift action_12
action_41 (24) = happyShift action_13
action_41 (25) = happyShift action_14
action_41 (26) = happyShift action_15
action_41 (28) = happyShift action_16
action_41 (7) = happyGoto action_5
action_41 (8) = happyGoto action_6
action_41 (9) = happyGoto action_53
action_41 _ = happyFail

action_42 _ = happyReduce_3

action_43 _ = happyReduce_10

action_44 _ = happyReduce_26

action_45 (15) = happyShift action_22
action_45 (16) = happyShift action_23
action_45 (18) = happyShift action_10
action_45 (19) = happyShift action_24
action_45 (21) = happyShift action_25
action_45 (22) = happyShift action_11
action_45 (23) = happyShift action_12
action_45 (24) = happyShift action_13
action_45 (25) = happyShift action_14
action_45 (26) = happyShift action_15
action_45 (28) = happyShift action_16
action_45 (5) = happyGoto action_52
action_45 (6) = happyGoto action_18
action_45 (7) = happyGoto action_19
action_45 (8) = happyGoto action_6
action_45 (9) = happyGoto action_20
action_45 (10) = happyGoto action_21
action_45 (11) = happyGoto action_3
action_45 _ = happyFail

action_46 (15) = happyShift action_22
action_46 (16) = happyShift action_23
action_46 (18) = happyShift action_10
action_46 (19) = happyShift action_24
action_46 (21) = happyShift action_25
action_46 (22) = happyShift action_11
action_46 (23) = happyShift action_12
action_46 (24) = happyShift action_13
action_46 (25) = happyShift action_14
action_46 (26) = happyShift action_15
action_46 (28) = happyShift action_16
action_46 (5) = happyGoto action_51
action_46 (6) = happyGoto action_18
action_46 (7) = happyGoto action_19
action_46 (8) = happyGoto action_6
action_46 (9) = happyGoto action_20
action_46 (10) = happyGoto action_21
action_46 (11) = happyGoto action_3
action_46 _ = happyFail

action_47 (16) = happyReduce_20
action_47 (18) = happyReduce_20
action_47 (22) = happyReduce_20
action_47 (23) = happyReduce_20
action_47 (24) = happyReduce_20
action_47 (25) = happyReduce_20
action_47 (26) = happyReduce_20
action_47 (27) = happyReduce_20
action_47 (28) = happyReduce_20
action_47 _ = happyReduce_11

action_48 _ = happyReduce_7

action_49 (16) = happyShift action_9
action_49 (18) = happyShift action_10
action_49 (22) = happyShift action_11
action_49 (23) = happyShift action_12
action_49 (24) = happyShift action_13
action_49 (25) = happyShift action_14
action_49 (26) = happyShift action_15
action_49 (28) = happyShift action_16
action_49 (7) = happyGoto action_5
action_49 (8) = happyGoto action_6
action_49 (9) = happyGoto action_50
action_49 _ = happyFail

action_50 _ = happyReduce_8

action_51 (17) = happyShift action_55
action_51 _ = happyFail

action_52 _ = happyReduce_4

action_53 (16) = happyShift action_9
action_53 (18) = happyShift action_10
action_53 (22) = happyShift action_11
action_53 (23) = happyShift action_12
action_53 (24) = happyShift action_13
action_53 (25) = happyShift action_14
action_53 (26) = happyShift action_15
action_53 (28) = happyShift action_16
action_53 (7) = happyGoto action_5
action_53 (8) = happyGoto action_6
action_53 (9) = happyGoto action_54
action_53 _ = happyFail

action_54 (16) = happyShift action_9
action_54 (18) = happyShift action_10
action_54 (22) = happyShift action_11
action_54 (23) = happyShift action_12
action_54 (24) = happyShift action_13
action_54 (25) = happyShift action_14
action_54 (26) = happyShift action_15
action_54 (28) = happyShift action_16
action_54 (7) = happyGoto action_5
action_54 (8) = happyGoto action_6
action_54 (9) = happyGoto action_58
action_54 _ = happyFail

action_55 (14) = happyShift action_57
action_55 (16) = happyShift action_4
action_55 (11) = happyGoto action_56
action_55 _ = happyFail

action_56 _ = happyReduce_24

action_57 (15) = happyShift action_22
action_57 (16) = happyShift action_23
action_57 (18) = happyShift action_10
action_57 (19) = happyShift action_24
action_57 (21) = happyShift action_25
action_57 (22) = happyShift action_11
action_57 (23) = happyShift action_12
action_57 (24) = happyShift action_13
action_57 (25) = happyShift action_14
action_57 (26) = happyShift action_15
action_57 (28) = happyShift action_16
action_57 (5) = happyGoto action_60
action_57 (6) = happyGoto action_18
action_57 (7) = happyGoto action_19
action_57 (8) = happyGoto action_6
action_57 (9) = happyGoto action_20
action_57 (10) = happyGoto action_21
action_57 (11) = happyGoto action_3
action_57 _ = happyFail

action_58 (16) = happyShift action_9
action_58 (18) = happyShift action_10
action_58 (22) = happyShift action_11
action_58 (23) = happyShift action_12
action_58 (24) = happyShift action_13
action_58 (25) = happyShift action_14
action_58 (26) = happyShift action_15
action_58 (28) = happyShift action_16
action_58 (7) = happyGoto action_5
action_58 (8) = happyGoto action_6
action_58 (9) = happyGoto action_59
action_58 _ = happyFail

action_59 _ = happyReduce_19

action_60 _ = happyReduce_25

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (STySig ["_"] happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokRewrite happy_var_1))
	 =  HappyAbsSyn5
		 (SRewrite happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SId happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (SAnnotate happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn5
		 (SVar happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 (HappyTerminal (TokNumeric happy_var_1))
	 =  HappyAbsSyn5
		 (SNatLiteral happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn5
		 (SNat
	)

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 (HappyTerminal (TokSort happy_var_1))
	 =  HappyAbsSyn5
		 (SSort happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn5
		 (SMeta
	)

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn5
		 (SRefl
	)

happyReduce_19 = happyReduce 7 8 happyReduction_19
happyReduction_19 ((HappyAbsSyn5  happy_var_7) `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SJ happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  9 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  9 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  10 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (SApp happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  10 happyReduction_23
happyReduction_23 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (SApp happy_var_1 happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 6 11 happyReduction_24
happyReduction_24 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (STySig happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 7 11 happyReduction_25
happyReduction_25 ((HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (STySig happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_2  12 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_2)
	(HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  12 happyReduction_27
happyReduction_27 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  13 happyReduction_28
happyReduction_28 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  13 happyReduction_29
happyReduction_29 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokArrow -> cont 14;
	TokLambda -> cont 15;
	TokOpenBracket -> cont 16;
	TokCloseBracket -> cont 17;
	TokIdentifier happy_dollar_dollar -> cont 18;
	TokRewrite happy_dollar_dollar -> cont 19;
	TokHasType -> cont 20;
	TokId -> cont 21;
	TokJ -> cont 22;
	TokRefl -> cont 23;
	TokNat -> cont 24;
	TokNumeric happy_dollar_dollar -> cont 25;
	TokSort happy_dollar_dollar -> cont 26;
	TokAnnotate -> cont 27;
	TokMeta -> cont 28;
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

sugarParse tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

sugarParseVarList tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data SugarTerm = SLam [Text] SugarTerm
               | SApp SugarTerm SugarTerm
               | SId SugarTerm SugarTerm SugarTerm
               | SJ SugarTerm SugarTerm SugarTerm SugarTerm SugarTerm SugarTerm
               | SRefl
               | STySig [Text] SugarTerm SugarTerm
               | SVar Text
               | SNatLiteral Int
               | SNat
               | SAnnotate SugarTerm SugarTerm
               | SSort Sort
               | SRewrite Direction SugarTerm SugarTerm
               | SMeta
               deriving (Show)

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
