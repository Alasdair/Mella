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
 action_57 :: () => Int -> ({-HappyReduction (Error) = -}
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
 happyReduce_28 :: () => ({-HappyReduction (Error) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Error) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Error) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Error) HappyAbsSyn)

action_0 (15) = happyShift action_22
action_0 (16) = happyShift action_23
action_0 (18) = happyShift action_10
action_0 (20) = happyShift action_24
action_0 (21) = happyShift action_11
action_0 (22) = happyShift action_12
action_0 (23) = happyShift action_13
action_0 (24) = happyShift action_14
action_0 (25) = happyShift action_15
action_0 (27) = happyShift action_16
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
action_1 (21) = happyShift action_11
action_1 (22) = happyShift action_12
action_1 (23) = happyShift action_13
action_1 (24) = happyShift action_14
action_1 (25) = happyShift action_15
action_1 (27) = happyShift action_16
action_1 (7) = happyGoto action_5
action_1 (8) = happyGoto action_6
action_1 (9) = happyGoto action_7
action_1 (13) = happyGoto action_8
action_1 _ = happyFail

action_2 (16) = happyShift action_4
action_2 (11) = happyGoto action_3
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (18) = happyShift action_30
action_4 (12) = happyGoto action_27
action_4 _ = happyFail

action_5 _ = happyReduce_20

action_6 _ = happyReduce_16

action_7 (16) = happyShift action_9
action_7 (18) = happyShift action_10
action_7 (21) = happyShift action_11
action_7 (22) = happyShift action_12
action_7 (23) = happyShift action_13
action_7 (24) = happyShift action_14
action_7 (25) = happyShift action_15
action_7 (27) = happyShift action_16
action_7 (7) = happyGoto action_5
action_7 (8) = happyGoto action_6
action_7 (9) = happyGoto action_7
action_7 (13) = happyGoto action_37
action_7 _ = happyReduce_28

action_8 (28) = happyAccept
action_8 _ = happyFail

action_9 (15) = happyShift action_22
action_9 (16) = happyShift action_23
action_9 (18) = happyShift action_10
action_9 (20) = happyShift action_24
action_9 (21) = happyShift action_11
action_9 (22) = happyShift action_12
action_9 (23) = happyShift action_13
action_9 (24) = happyShift action_14
action_9 (25) = happyShift action_15
action_9 (27) = happyShift action_16
action_9 (5) = happyGoto action_36
action_9 (6) = happyGoto action_18
action_9 (7) = happyGoto action_19
action_9 (8) = happyGoto action_6
action_9 (9) = happyGoto action_20
action_9 (10) = happyGoto action_21
action_9 (11) = happyGoto action_3
action_9 _ = happyFail

action_10 _ = happyReduce_11

action_11 (16) = happyShift action_9
action_11 (18) = happyShift action_10
action_11 (21) = happyShift action_11
action_11 (22) = happyShift action_12
action_11 (23) = happyShift action_13
action_11 (24) = happyShift action_14
action_11 (25) = happyShift action_15
action_11 (27) = happyShift action_16
action_11 (7) = happyGoto action_5
action_11 (8) = happyGoto action_6
action_11 (9) = happyGoto action_35
action_11 _ = happyFail

action_12 _ = happyReduce_17

action_13 _ = happyReduce_13

action_14 _ = happyReduce_12

action_15 _ = happyReduce_14

action_16 _ = happyReduce_15

action_17 (28) = happyAccept
action_17 _ = happyFail

action_18 (14) = happyShift action_34
action_18 _ = happyReduce_5

action_19 (16) = happyReduce_20
action_19 (18) = happyReduce_20
action_19 (21) = happyReduce_20
action_19 (22) = happyReduce_20
action_19 (23) = happyReduce_20
action_19 (24) = happyReduce_20
action_19 (25) = happyReduce_20
action_19 (26) = happyReduce_20
action_19 (27) = happyReduce_20
action_19 _ = happyReduce_8

action_20 (16) = happyShift action_9
action_20 (18) = happyShift action_10
action_20 (21) = happyShift action_11
action_20 (22) = happyShift action_12
action_20 (23) = happyShift action_13
action_20 (24) = happyShift action_14
action_20 (25) = happyShift action_15
action_20 (26) = happyShift action_33
action_20 (27) = happyShift action_16
action_20 (7) = happyGoto action_5
action_20 (8) = happyGoto action_6
action_20 (9) = happyGoto action_32
action_20 _ = happyFail

action_21 (16) = happyShift action_9
action_21 (18) = happyShift action_10
action_21 (21) = happyShift action_11
action_21 (22) = happyShift action_12
action_21 (23) = happyShift action_13
action_21 (24) = happyShift action_14
action_21 (25) = happyShift action_15
action_21 (27) = happyShift action_16
action_21 (7) = happyGoto action_5
action_21 (8) = happyGoto action_6
action_21 (9) = happyGoto action_31
action_21 _ = happyReduce_6

action_22 (18) = happyShift action_30
action_22 (12) = happyGoto action_29
action_22 _ = happyFail

action_23 (15) = happyShift action_22
action_23 (16) = happyShift action_23
action_23 (18) = happyShift action_28
action_23 (20) = happyShift action_24
action_23 (21) = happyShift action_11
action_23 (22) = happyShift action_12
action_23 (23) = happyShift action_13
action_23 (24) = happyShift action_14
action_23 (25) = happyShift action_15
action_23 (27) = happyShift action_16
action_23 (5) = happyGoto action_26
action_23 (6) = happyGoto action_18
action_23 (7) = happyGoto action_19
action_23 (8) = happyGoto action_6
action_23 (9) = happyGoto action_20
action_23 (10) = happyGoto action_21
action_23 (11) = happyGoto action_3
action_23 (12) = happyGoto action_27
action_23 _ = happyFail

action_24 (16) = happyShift action_9
action_24 (18) = happyShift action_10
action_24 (21) = happyShift action_11
action_24 (22) = happyShift action_12
action_24 (23) = happyShift action_13
action_24 (24) = happyShift action_14
action_24 (25) = happyShift action_15
action_24 (27) = happyShift action_16
action_24 (7) = happyGoto action_5
action_24 (8) = happyGoto action_6
action_24 (9) = happyGoto action_25
action_24 _ = happyFail

action_25 (16) = happyShift action_9
action_25 (18) = happyShift action_10
action_25 (21) = happyShift action_11
action_25 (22) = happyShift action_12
action_25 (23) = happyShift action_13
action_25 (24) = happyShift action_14
action_25 (25) = happyShift action_15
action_25 (27) = happyShift action_16
action_25 (7) = happyGoto action_5
action_25 (8) = happyGoto action_6
action_25 (9) = happyGoto action_46
action_25 _ = happyFail

action_26 (17) = happyShift action_45
action_26 _ = happyFail

action_27 (19) = happyShift action_44
action_27 _ = happyFail

action_28 (18) = happyShift action_30
action_28 (19) = happyReduce_26
action_28 (12) = happyGoto action_42
action_28 _ = happyReduce_11

action_29 (14) = happyShift action_43
action_29 _ = happyFail

action_30 (18) = happyShift action_30
action_30 (12) = happyGoto action_42
action_30 _ = happyReduce_26

action_31 _ = happyReduce_21

action_32 _ = happyReduce_22

action_33 (16) = happyShift action_9
action_33 (18) = happyShift action_10
action_33 (21) = happyShift action_11
action_33 (22) = happyShift action_12
action_33 (23) = happyShift action_13
action_33 (24) = happyShift action_14
action_33 (25) = happyShift action_15
action_33 (27) = happyShift action_16
action_33 (7) = happyGoto action_5
action_33 (8) = happyGoto action_6
action_33 (9) = happyGoto action_41
action_33 _ = happyFail

action_34 (15) = happyShift action_22
action_34 (16) = happyShift action_23
action_34 (18) = happyShift action_10
action_34 (20) = happyShift action_24
action_34 (21) = happyShift action_11
action_34 (22) = happyShift action_12
action_34 (23) = happyShift action_13
action_34 (24) = happyShift action_14
action_34 (25) = happyShift action_15
action_34 (27) = happyShift action_16
action_34 (5) = happyGoto action_40
action_34 (6) = happyGoto action_18
action_34 (7) = happyGoto action_19
action_34 (8) = happyGoto action_6
action_34 (9) = happyGoto action_20
action_34 (10) = happyGoto action_21
action_34 (11) = happyGoto action_3
action_34 _ = happyFail

action_35 (16) = happyShift action_9
action_35 (18) = happyShift action_10
action_35 (21) = happyShift action_11
action_35 (22) = happyShift action_12
action_35 (23) = happyShift action_13
action_35 (24) = happyShift action_14
action_35 (25) = happyShift action_15
action_35 (27) = happyShift action_16
action_35 (7) = happyGoto action_5
action_35 (8) = happyGoto action_6
action_35 (9) = happyGoto action_39
action_35 _ = happyFail

action_36 (17) = happyShift action_38
action_36 _ = happyFail

action_37 _ = happyReduce_27

action_38 _ = happyReduce_19

action_39 (16) = happyShift action_9
action_39 (18) = happyShift action_10
action_39 (21) = happyShift action_11
action_39 (22) = happyShift action_12
action_39 (23) = happyShift action_13
action_39 (24) = happyShift action_14
action_39 (25) = happyShift action_15
action_39 (27) = happyShift action_16
action_39 (7) = happyGoto action_5
action_39 (8) = happyGoto action_6
action_39 (9) = happyGoto action_50
action_39 _ = happyFail

action_40 _ = happyReduce_3

action_41 _ = happyReduce_9

action_42 _ = happyReduce_25

action_43 (15) = happyShift action_22
action_43 (16) = happyShift action_23
action_43 (18) = happyShift action_10
action_43 (20) = happyShift action_24
action_43 (21) = happyShift action_11
action_43 (22) = happyShift action_12
action_43 (23) = happyShift action_13
action_43 (24) = happyShift action_14
action_43 (25) = happyShift action_15
action_43 (27) = happyShift action_16
action_43 (5) = happyGoto action_49
action_43 (6) = happyGoto action_18
action_43 (7) = happyGoto action_19
action_43 (8) = happyGoto action_6
action_43 (9) = happyGoto action_20
action_43 (10) = happyGoto action_21
action_43 (11) = happyGoto action_3
action_43 _ = happyFail

action_44 (15) = happyShift action_22
action_44 (16) = happyShift action_23
action_44 (18) = happyShift action_10
action_44 (20) = happyShift action_24
action_44 (21) = happyShift action_11
action_44 (22) = happyShift action_12
action_44 (23) = happyShift action_13
action_44 (24) = happyShift action_14
action_44 (25) = happyShift action_15
action_44 (27) = happyShift action_16
action_44 (5) = happyGoto action_48
action_44 (6) = happyGoto action_18
action_44 (7) = happyGoto action_19
action_44 (8) = happyGoto action_6
action_44 (9) = happyGoto action_20
action_44 (10) = happyGoto action_21
action_44 (11) = happyGoto action_3
action_44 _ = happyFail

action_45 (16) = happyReduce_19
action_45 (18) = happyReduce_19
action_45 (21) = happyReduce_19
action_45 (22) = happyReduce_19
action_45 (23) = happyReduce_19
action_45 (24) = happyReduce_19
action_45 (25) = happyReduce_19
action_45 (26) = happyReduce_19
action_45 (27) = happyReduce_19
action_45 _ = happyReduce_10

action_46 (16) = happyShift action_9
action_46 (18) = happyShift action_10
action_46 (21) = happyShift action_11
action_46 (22) = happyShift action_12
action_46 (23) = happyShift action_13
action_46 (24) = happyShift action_14
action_46 (25) = happyShift action_15
action_46 (27) = happyShift action_16
action_46 (7) = happyGoto action_5
action_46 (8) = happyGoto action_6
action_46 (9) = happyGoto action_47
action_46 _ = happyFail

action_47 _ = happyReduce_7

action_48 (17) = happyShift action_52
action_48 _ = happyFail

action_49 _ = happyReduce_4

action_50 (16) = happyShift action_9
action_50 (18) = happyShift action_10
action_50 (21) = happyShift action_11
action_50 (22) = happyShift action_12
action_50 (23) = happyShift action_13
action_50 (24) = happyShift action_14
action_50 (25) = happyShift action_15
action_50 (27) = happyShift action_16
action_50 (7) = happyGoto action_5
action_50 (8) = happyGoto action_6
action_50 (9) = happyGoto action_51
action_50 _ = happyFail

action_51 (16) = happyShift action_9
action_51 (18) = happyShift action_10
action_51 (21) = happyShift action_11
action_51 (22) = happyShift action_12
action_51 (23) = happyShift action_13
action_51 (24) = happyShift action_14
action_51 (25) = happyShift action_15
action_51 (27) = happyShift action_16
action_51 (7) = happyGoto action_5
action_51 (8) = happyGoto action_6
action_51 (9) = happyGoto action_55
action_51 _ = happyFail

action_52 (14) = happyShift action_54
action_52 (16) = happyShift action_4
action_52 (11) = happyGoto action_53
action_52 _ = happyFail

action_53 _ = happyReduce_23

action_54 (15) = happyShift action_22
action_54 (16) = happyShift action_23
action_54 (18) = happyShift action_10
action_54 (20) = happyShift action_24
action_54 (21) = happyShift action_11
action_54 (22) = happyShift action_12
action_54 (23) = happyShift action_13
action_54 (24) = happyShift action_14
action_54 (25) = happyShift action_15
action_54 (27) = happyShift action_16
action_54 (5) = happyGoto action_57
action_54 (6) = happyGoto action_18
action_54 (7) = happyGoto action_19
action_54 (8) = happyGoto action_6
action_54 (9) = happyGoto action_20
action_54 (10) = happyGoto action_21
action_54 (11) = happyGoto action_3
action_54 _ = happyFail

action_55 (16) = happyShift action_9
action_55 (18) = happyShift action_10
action_55 (21) = happyShift action_11
action_55 (22) = happyShift action_12
action_55 (23) = happyShift action_13
action_55 (24) = happyShift action_14
action_55 (25) = happyShift action_15
action_55 (27) = happyShift action_16
action_55 (7) = happyGoto action_5
action_55 (8) = happyGoto action_6
action_55 (9) = happyGoto action_56
action_55 _ = happyFail

action_56 _ = happyReduce_18

action_57 _ = happyReduce_24

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

happyReduce_7 = happyReduce 4 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SId happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (SAnnotate happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn5
		 (SVar happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyTerminal (TokNumeric happy_var_1))
	 =  HappyAbsSyn5
		 (SNatLiteral happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn5
		 (SNat
	)

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 (HappyTerminal (TokSort happy_var_1))
	 =  HappyAbsSyn5
		 (SSort happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn5
		 (SMeta
	)

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn5
		 (SRefl
	)

happyReduce_18 = happyReduce 7 8 happyReduction_18
happyReduction_18 ((HappyAbsSyn5  happy_var_7) `HappyStk`
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

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  9 happyReduction_20
happyReduction_20 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  10 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (SApp happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  10 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (SApp happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 6 11 happyReduction_23
happyReduction_23 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (STySig happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 7 11 happyReduction_24
happyReduction_24 ((HappyAbsSyn5  happy_var_7) `HappyStk`
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

happyReduce_25 = happySpecReduce_2  12 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_2)
	(HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  12 happyReduction_26
happyReduction_26 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  13 happyReduction_27
happyReduction_27 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  13 happyReduction_28
happyReduction_28 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 28 28 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokArrow -> cont 14;
	TokLambda -> cont 15;
	TokOpenBracket -> cont 16;
	TokCloseBracket -> cont 17;
	TokIdentifier happy_dollar_dollar -> cont 18;
	TokHasType -> cont 19;
	TokId -> cont 20;
	TokJ -> cont 21;
	TokRefl -> cont 22;
	TokNat -> cont 23;
	TokNumeric happy_dollar_dollar -> cont 24;
	TokSort happy_dollar_dollar -> cont 25;
	TokAnnotate -> cont 26;
	TokMeta -> cont 27;
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
