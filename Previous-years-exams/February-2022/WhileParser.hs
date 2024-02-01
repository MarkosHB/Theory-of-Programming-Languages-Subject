{-# OPTIONS_GHC -w #-}
module WhileParser ( happyParseWhile
                   , parser
                   ) where

import Aexp
import Bexp
import While
import WhileLexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 ((String, [Var], Stm))
	| HappyAbsSyn5 ([Var])
	| HappyAbsSyn7 (Stm)
	| HappyAbsSyn9 (DecVar)
	| HappyAbsSyn10 (SelectCases)
	| HappyAbsSyn11 (Bexp)
	| HappyAbsSyn12 (Aexp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
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
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
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
 happyReduce_29 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,133) ([0,0,8,0,512,512,0,0,0,0,512,0,0,512,0,2,0,4096,16384,0,0,0,8,101,0,0,0,8192,0,0,2,0,0,0,0,2,2048,0,0,0,4,0,0,512,0,0,0,0,32,404,2048,0,0,16386,25,0,2048,12288,90,0,140,0,512,6464,0,0,0,448,0,0,0,0,0,0,2240,0,0,16416,0,24688,0,41728,5,0,0,0,0,0,35840,22,0,0,0,0,32,0,512,0,0,128,0,0,49152,8,0,23088,0,0,0,0,576,0,5888,6,12288,2,0,140,0,8960,0,49152,8,0,560,0,35840,22,0,16386,25,5888,0,0,0,0,0,8,0,0,0,7,0,448,0,0,0,0,16,0,1024,0,0,0,0,2048,2,7168,128,0,0,16,32,404,0,0,0,64,0,0,0,0,16384,0,8,101,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happyParseWhile","Program","Outputs","Var_List","Stm_List","Stm","Dec_Vars","Select_Cases","Bexp","Aexp","NUM","ID","'+'","'-'","'*'","'('","')'","'true'","'false'","'&&'","'!'","'='","'<='","':='","'skip'","';'","'select'","'when'","'do'","'for'","'begin'","'end'","'var'","'program'","','","%eof"]
        bit_start = st Prelude.* 38
        bit_end = (st Prelude.+ 1) Prelude.* 38
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..37]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (36) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (36) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (14) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (38) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (18) = happyShift action_6
action_4 (5) = happyGoto action_5
action_4 _ = happyReduce_3

action_5 (28) = happyShift action_9
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (14) = happyShift action_8
action_6 (6) = happyGoto action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (19) = happyShift action_17
action_7 (37) = happyShift action_18
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_4

action_9 (14) = happyShift action_12
action_9 (27) = happyShift action_13
action_9 (29) = happyShift action_14
action_9 (32) = happyShift action_15
action_9 (33) = happyShift action_16
action_9 (7) = happyGoto action_10
action_9 (8) = happyGoto action_11
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_1

action_11 (28) = happyShift action_26
action_11 _ = happyReduce_6

action_12 (26) = happyShift action_25
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_9

action_14 (30) = happyShift action_24
action_14 (10) = happyGoto action_23
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (18) = happyShift action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (35) = happyShift action_21
action_16 (9) = happyGoto action_20
action_16 _ = happyReduce_14

action_17 _ = happyReduce_2

action_18 (14) = happyShift action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_5

action_20 (14) = happyShift action_12
action_20 (27) = happyShift action_13
action_20 (29) = happyShift action_14
action_20 (32) = happyShift action_15
action_20 (33) = happyShift action_16
action_20 (7) = happyGoto action_41
action_20 (8) = happyGoto action_11
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (14) = happyShift action_40
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (14) = happyShift action_12
action_22 (27) = happyShift action_13
action_22 (29) = happyShift action_14
action_22 (32) = happyShift action_15
action_22 (33) = happyShift action_16
action_22 (8) = happyGoto action_39
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (34) = happyShift action_38
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (13) = happyShift action_29
action_24 (14) = happyShift action_30
action_24 (18) = happyShift action_34
action_24 (20) = happyShift action_35
action_24 (21) = happyShift action_36
action_24 (23) = happyShift action_37
action_24 (11) = happyGoto action_32
action_24 (12) = happyGoto action_33
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (13) = happyShift action_29
action_25 (14) = happyShift action_30
action_25 (18) = happyShift action_31
action_25 (12) = happyGoto action_28
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (14) = happyShift action_12
action_26 (27) = happyShift action_13
action_26 (29) = happyShift action_14
action_26 (32) = happyShift action_15
action_26 (33) = happyShift action_16
action_26 (7) = happyGoto action_27
action_26 (8) = happyGoto action_11
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_7

action_28 (15) = happyShift action_48
action_28 (16) = happyShift action_49
action_28 (17) = happyShift action_50
action_28 _ = happyReduce_8

action_29 _ = happyReduce_24

action_30 _ = happyReduce_25

action_31 (13) = happyShift action_29
action_31 (14) = happyShift action_30
action_31 (18) = happyShift action_31
action_31 (12) = happyGoto action_55
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (22) = happyShift action_53
action_32 (31) = happyShift action_54
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (15) = happyShift action_48
action_33 (16) = happyShift action_49
action_33 (17) = happyShift action_50
action_33 (24) = happyShift action_51
action_33 (25) = happyShift action_52
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (13) = happyShift action_29
action_34 (14) = happyShift action_30
action_34 (18) = happyShift action_34
action_34 (20) = happyShift action_35
action_34 (21) = happyShift action_36
action_34 (23) = happyShift action_37
action_34 (11) = happyGoto action_46
action_34 (12) = happyGoto action_47
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_17

action_36 _ = happyReduce_18

action_37 (13) = happyShift action_29
action_37 (14) = happyShift action_30
action_37 (18) = happyShift action_34
action_37 (20) = happyShift action_35
action_37 (21) = happyShift action_36
action_37 (23) = happyShift action_37
action_37 (11) = happyGoto action_45
action_37 (12) = happyGoto action_33
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_10

action_39 (28) = happyShift action_44
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (26) = happyShift action_43
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (34) = happyShift action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_12

action_43 (13) = happyShift action_29
action_43 (14) = happyShift action_30
action_43 (18) = happyShift action_31
action_43 (12) = happyGoto action_66
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (13) = happyShift action_29
action_44 (14) = happyShift action_30
action_44 (18) = happyShift action_34
action_44 (20) = happyShift action_35
action_44 (21) = happyShift action_36
action_44 (23) = happyShift action_37
action_44 (11) = happyGoto action_65
action_44 (12) = happyGoto action_33
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_21

action_46 (19) = happyShift action_64
action_46 (22) = happyShift action_53
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (15) = happyShift action_48
action_47 (16) = happyShift action_49
action_47 (17) = happyShift action_50
action_47 (19) = happyShift action_56
action_47 (24) = happyShift action_51
action_47 (25) = happyShift action_52
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (13) = happyShift action_29
action_48 (14) = happyShift action_30
action_48 (18) = happyShift action_31
action_48 (12) = happyGoto action_63
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (13) = happyShift action_29
action_49 (14) = happyShift action_30
action_49 (18) = happyShift action_31
action_49 (12) = happyGoto action_62
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (13) = happyShift action_29
action_50 (14) = happyShift action_30
action_50 (18) = happyShift action_31
action_50 (12) = happyGoto action_61
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (13) = happyShift action_29
action_51 (14) = happyShift action_30
action_51 (18) = happyShift action_31
action_51 (12) = happyGoto action_60
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (13) = happyShift action_29
action_52 (14) = happyShift action_30
action_52 (18) = happyShift action_31
action_52 (12) = happyGoto action_59
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (13) = happyShift action_29
action_53 (14) = happyShift action_30
action_53 (18) = happyShift action_34
action_53 (20) = happyShift action_35
action_53 (21) = happyShift action_36
action_53 (23) = happyShift action_37
action_53 (11) = happyGoto action_58
action_53 (12) = happyGoto action_33
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (14) = happyShift action_12
action_54 (27) = happyShift action_13
action_54 (29) = happyShift action_14
action_54 (32) = happyShift action_15
action_54 (33) = happyShift action_16
action_54 (8) = happyGoto action_57
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (15) = happyShift action_48
action_55 (16) = happyShift action_49
action_55 (17) = happyShift action_50
action_55 (19) = happyShift action_56
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_29

action_57 (30) = happyShift action_24
action_57 (10) = happyGoto action_69
action_57 _ = happyReduce_16

action_58 _ = happyReduce_22

action_59 (15) = happyShift action_48
action_59 (16) = happyShift action_49
action_59 (17) = happyShift action_50
action_59 _ = happyReduce_20

action_60 (15) = happyShift action_48
action_60 (16) = happyShift action_49
action_60 (17) = happyShift action_50
action_60 _ = happyReduce_19

action_61 _ = happyReduce_28

action_62 (17) = happyShift action_50
action_62 _ = happyReduce_27

action_63 (17) = happyShift action_50
action_63 _ = happyReduce_26

action_64 _ = happyReduce_23

action_65 (22) = happyShift action_53
action_65 (28) = happyShift action_68
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (15) = happyShift action_48
action_66 (16) = happyShift action_49
action_66 (17) = happyShift action_50
action_66 (28) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (35) = happyShift action_21
action_67 (9) = happyGoto action_71
action_67 _ = happyReduce_14

action_68 (14) = happyShift action_12
action_68 (27) = happyShift action_13
action_68 (29) = happyShift action_14
action_68 (32) = happyShift action_15
action_68 (33) = happyShift action_16
action_68 (8) = happyGoto action_70
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_15

action_70 (19) = happyShift action_72
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_13

action_72 (31) = happyShift action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (14) = happyShift action_12
action_73 (27) = happyShift action_13
action_73 (29) = happyShift action_14
action_73 (32) = happyShift action_15
action_73 (33) = happyShift action_16
action_73 (8) = happyGoto action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_11

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_2, reverse happy_var_3, happy_var_5)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyTerminal (IDENTIFIER happy_var_1))
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyTerminal (IDENTIFIER happy_var_3))
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_3:happy_var_1
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Comp happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (IDENTIFIER happy_var_1))
	 =  HappyAbsSyn7
		 (Ass happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (Skip
	)

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Select happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 10 8 happyReduction_11
happyReduction_11 ((HappyAbsSyn7  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (For happy_var_3 happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 8 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Block happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 6 9 happyReduction_13
happyReduction_13 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Dec happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_0  9 happyReduction_14
happyReduction_14  =  HappyAbsSyn9
		 (EndDec
	)

happyReduce_15 = happyReduce 5 10 happyReduction_15
happyReduction_15 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Case happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 10 happyReduction_16
happyReduction_16 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (EndCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn11
		 (TRUE
	)

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn11
		 (FALSE
	)

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (Le happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Neg happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (And happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 (HappyTerminal (LITERAL_INT happy_var_1))
	 =  HappyAbsSyn12
		 (N happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  12 happyReduction_25
happyReduction_25 (HappyTerminal (IDENTIFIER happy_var_1))
	 =  HappyAbsSyn12
		 (V happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Add happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 38 38 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LITERAL_INT happy_dollar_dollar -> cont 13;
	IDENTIFIER happy_dollar_dollar -> cont 14;
	PLUS -> cont 15;
	MINUS -> cont 16;
	ASTERISK -> cont 17;
	LPAREN -> cont 18;
	RPAREN -> cont 19;
	LITERAL_TRUE -> cont 20;
	LITERAL_FALSE -> cont 21;
	AMPERSANDS -> cont 22;
	EXCLAMATION -> cont 23;
	EQUALS -> cont 24;
	LESSEQUALS -> cont 25;
	ASSIGN -> cont 26;
	SKIP -> cont 27;
	SEMICOLON -> cont 28;
	SELECT -> cont 29;
	WHEN -> cont 30;
	DO -> cont 31;
	FOR -> cont 32;
	BEGIN -> cont 33;
	END -> cont 34;
	VAR -> cont 35;
	PROGRAM -> cont 36;
	COMMA -> cont 37;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 38 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> happyError tokens)
happyParseWhile tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

parser :: FilePath -> IO((String, [Var], Stm))
parser filename = do
  s <- readFile filename
  return $ (happyParseWhile . alexScanTokens) s
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































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
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

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
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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
