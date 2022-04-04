{-# OPTIONS_GHC -w #-}
-- MyParser.hs is automatically generated from MyParser.y using the happy parser generator.

-- Description for MyParser.y:
-- this file contains the definition of the grammar.
-- In the beginning, the parser created a one-to-one corresponding concrete syntax tree for the grammar.
-- Now, the functionalities of the happy parser are used to directly create an abstract syntax tree with 
-- the data types 'Stmt' and 'Expr', as defined in Definitions.hs
--
-- The parser uses the Either Monad, and returns Left err if a parser error occurs.
--
-- One problem in the grammar was that function applications like "f(x,y)" have different semantic meanings:
-- In definitions like "f(x,y) = expr", anything inside f(..) is a binding. Here only variables (ids) must occur.
-- If used in expressions, like calling the function f with arguments "f(5, 2x)",
-- anything inside f(..) is an argument and can be any expression.
-- Because of this, we needed to distinguish between List (containing expressions) and IdList (containing only ids).
-- We thus introduce two variations of each rule: rule and ruleN, where rule can be with IDs and ruleN is without IDs.
-- For example: 
--      AtomN ::= num | "(" <expr> ")" | <app>
--      Atom ::= id | <AtomN>






module NomadParser( parse ) where

import Control.Monad.Trans.Except( ExceptT, throwE )
import Lexer ( lexer, Token(..) )
import Definitions ( Stmt(..), Expr(..), Op(..), MyError(..), MyException )
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,397) ([0,18016,1,13056,10,0,0,0,128,0,0,0,0,0,6144,0,0,3,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2060,0,16384,0,13056,2,38912,17,49152,652,0,4,0,0,0,512,0,4096,8,24576,326,0,3635,0,20888,0,512,4,0,0,0,64,0,0,0,0,0,16384,0,0,0,0,8,0,0,0,0,0,8192,0,384,1,3072,8,24576,64,0,563,0,4504,0,36032,2,4096,8,32768,64,0,0,0,2048,0,0,0,0,0,0,8,0,64,0,0,0,0,0,0,1,0,8,16384,0,0,2611,0,0,0,512,1,4096,8,0,0,0,0,0,10444,0,0,0,13056,10,0,0,0,0,0,0,0,41776,0,0,0,0,0,0,129,0,1032,0,0,0,0,0,0,4,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Stmt","IdFunc","App","Abs","List","IdList","Expr2","Expr","Term2","Term","Factor","Atom","Expr2N","ExprN","Term2N","TermN","FactorN","AtomN","num","id","'='","','","'+'","'-'","'*'","'/'","'^'","'('","')'","lam","'->'","%eof"]
        bit_start = st Prelude.* 35
        bit_end = (st Prelude.+ 1) Prelude.* 35
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..34]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (22) = happyShift action_15
action_0 (23) = happyShift action_23
action_0 (26) = happyShift action_17
action_0 (27) = happyShift action_18
action_0 (31) = happyShift action_19
action_0 (33) = happyShift action_20
action_0 (4) = happyGoto action_21
action_0 (5) = happyGoto action_22
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (11) = happyGoto action_6
action_0 (13) = happyGoto action_7
action_0 (14) = happyGoto action_8
action_0 (16) = happyGoto action_9
action_0 (17) = happyGoto action_10
action_0 (18) = happyGoto action_11
action_0 (19) = happyGoto action_12
action_0 (20) = happyGoto action_13
action_0 (21) = happyGoto action_14
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (22) = happyShift action_15
action_1 (23) = happyShift action_16
action_1 (26) = happyShift action_17
action_1 (27) = happyShift action_18
action_1 (31) = happyShift action_19
action_1 (33) = happyShift action_20
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (10) = happyGoto action_5
action_1 (11) = happyGoto action_6
action_1 (13) = happyGoto action_7
action_1 (14) = happyGoto action_8
action_1 (16) = happyGoto action_9
action_1 (17) = happyGoto action_10
action_1 (18) = happyGoto action_11
action_1 (19) = happyGoto action_12
action_1 (20) = happyGoto action_13
action_1 (21) = happyGoto action_14
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_6

action_3 (31) = happyShift action_43
action_3 _ = happyReduce_46

action_4 _ = happyReduce_30

action_5 _ = happyReduce_1

action_6 (26) = happyShift action_41
action_6 (27) = happyShift action_42
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (28) = happyShift action_39
action_7 (29) = happyShift action_40
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (30) = happyShift action_38
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_19

action_10 (26) = happyReduce_21
action_10 (27) = happyReduce_21
action_10 _ = happyReduce_31

action_11 _ = happyReduce_34

action_12 (28) = happyReduce_25
action_12 (29) = happyReduce_25
action_12 _ = happyReduce_37

action_13 (30) = happyReduce_27
action_13 _ = happyReduce_41

action_14 _ = happyReduce_43

action_15 (22) = happyShift action_36
action_15 (23) = happyShift action_37
action_15 (31) = happyShift action_19
action_15 (5) = happyGoto action_2
action_15 (6) = happyGoto action_3
action_15 (14) = happyGoto action_34
action_15 (20) = happyGoto action_35
action_15 (21) = happyGoto action_14
action_15 _ = happyReduce_44

action_16 (26) = happyReduce_20
action_16 (27) = happyReduce_20
action_16 (28) = happyReduce_24
action_16 (29) = happyReduce_24
action_16 (30) = happyReduce_26
action_16 (31) = happyShift action_25
action_16 _ = happyReduce_18

action_17 (22) = happyShift action_15
action_17 (23) = happyShift action_32
action_17 (26) = happyShift action_17
action_17 (27) = happyShift action_18
action_17 (31) = happyShift action_19
action_17 (5) = happyGoto action_2
action_17 (6) = happyGoto action_3
action_17 (12) = happyGoto action_33
action_17 (13) = happyGoto action_7
action_17 (14) = happyGoto action_8
action_17 (18) = happyGoto action_31
action_17 (19) = happyGoto action_12
action_17 (20) = happyGoto action_13
action_17 (21) = happyGoto action_14
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (22) = happyShift action_15
action_18 (23) = happyShift action_32
action_18 (26) = happyShift action_17
action_18 (27) = happyShift action_18
action_18 (31) = happyShift action_19
action_18 (5) = happyGoto action_2
action_18 (6) = happyGoto action_3
action_18 (12) = happyGoto action_30
action_18 (13) = happyGoto action_7
action_18 (14) = happyGoto action_8
action_18 (18) = happyGoto action_31
action_18 (19) = happyGoto action_12
action_18 (20) = happyGoto action_13
action_18 (21) = happyGoto action_14
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (22) = happyShift action_15
action_19 (23) = happyShift action_16
action_19 (26) = happyShift action_17
action_19 (27) = happyShift action_18
action_19 (31) = happyShift action_19
action_19 (33) = happyShift action_20
action_19 (5) = happyGoto action_2
action_19 (6) = happyGoto action_3
action_19 (7) = happyGoto action_4
action_19 (10) = happyGoto action_29
action_19 (11) = happyGoto action_6
action_19 (13) = happyGoto action_7
action_19 (14) = happyGoto action_8
action_19 (16) = happyGoto action_9
action_19 (17) = happyGoto action_10
action_19 (18) = happyGoto action_11
action_19 (19) = happyGoto action_12
action_19 (20) = happyGoto action_13
action_19 (21) = happyGoto action_14
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (23) = happyShift action_28
action_20 (9) = happyGoto action_27
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (35) = happyAccept
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (24) = happyShift action_26
action_22 _ = happyReduce_6

action_23 (24) = happyShift action_24
action_23 (28) = happyReduce_24
action_23 (29) = happyReduce_24
action_23 (30) = happyReduce_26
action_23 (31) = happyShift action_25
action_23 (35) = happyReduce_18
action_23 _ = happyReduce_20

action_24 (22) = happyShift action_15
action_24 (23) = happyShift action_16
action_24 (26) = happyShift action_17
action_24 (27) = happyShift action_18
action_24 (31) = happyShift action_19
action_24 (33) = happyShift action_20
action_24 (5) = happyGoto action_2
action_24 (6) = happyGoto action_3
action_24 (7) = happyGoto action_4
action_24 (10) = happyGoto action_62
action_24 (11) = happyGoto action_6
action_24 (13) = happyGoto action_7
action_24 (14) = happyGoto action_8
action_24 (16) = happyGoto action_9
action_24 (17) = happyGoto action_10
action_24 (18) = happyGoto action_11
action_24 (19) = happyGoto action_12
action_24 (20) = happyGoto action_13
action_24 (21) = happyGoto action_14
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (22) = happyShift action_15
action_25 (23) = happyShift action_47
action_25 (26) = happyShift action_17
action_25 (27) = happyShift action_18
action_25 (31) = happyShift action_19
action_25 (32) = happyShift action_61
action_25 (33) = happyShift action_20
action_25 (5) = happyGoto action_2
action_25 (6) = happyGoto action_3
action_25 (7) = happyGoto action_4
action_25 (8) = happyGoto action_59
action_25 (9) = happyGoto action_60
action_25 (11) = happyGoto action_6
action_25 (13) = happyGoto action_7
action_25 (14) = happyGoto action_8
action_25 (16) = happyGoto action_46
action_25 (17) = happyGoto action_10
action_25 (18) = happyGoto action_11
action_25 (19) = happyGoto action_12
action_25 (20) = happyGoto action_13
action_25 (21) = happyGoto action_14
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (22) = happyShift action_15
action_26 (23) = happyShift action_16
action_26 (26) = happyShift action_17
action_26 (27) = happyShift action_18
action_26 (31) = happyShift action_19
action_26 (33) = happyShift action_20
action_26 (5) = happyGoto action_2
action_26 (6) = happyGoto action_3
action_26 (7) = happyGoto action_4
action_26 (10) = happyGoto action_58
action_26 (11) = happyGoto action_6
action_26 (13) = happyGoto action_7
action_26 (14) = happyGoto action_8
action_26 (16) = happyGoto action_9
action_26 (17) = happyGoto action_10
action_26 (18) = happyGoto action_11
action_26 (19) = happyGoto action_12
action_26 (20) = happyGoto action_13
action_26 (21) = happyGoto action_14
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (25) = happyShift action_56
action_27 (34) = happyShift action_57
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_16

action_29 (32) = happyShift action_55
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_35

action_31 _ = happyReduce_23

action_32 (28) = happyReduce_24
action_32 (29) = happyReduce_24
action_32 (30) = happyReduce_26
action_32 (31) = happyShift action_25
action_32 _ = happyReduce_22

action_33 _ = happyReduce_36

action_34 (30) = happyShift action_38
action_34 _ = happyReduce_40

action_35 _ = happyReduce_27

action_36 _ = happyReduce_44

action_37 (31) = happyShift action_25
action_37 _ = happyReduce_26

action_38 (22) = happyShift action_36
action_38 (23) = happyShift action_54
action_38 (31) = happyShift action_19
action_38 (5) = happyGoto action_2
action_38 (6) = happyGoto action_3
action_38 (15) = happyGoto action_52
action_38 (21) = happyGoto action_53
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (22) = happyShift action_36
action_39 (23) = happyShift action_37
action_39 (31) = happyShift action_19
action_39 (5) = happyGoto action_2
action_39 (6) = happyGoto action_3
action_39 (14) = happyGoto action_51
action_39 (20) = happyGoto action_35
action_39 (21) = happyGoto action_14
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (22) = happyShift action_36
action_40 (23) = happyShift action_37
action_40 (31) = happyShift action_19
action_40 (5) = happyGoto action_2
action_40 (6) = happyGoto action_3
action_40 (14) = happyGoto action_50
action_40 (20) = happyGoto action_35
action_40 (21) = happyGoto action_14
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (22) = happyShift action_15
action_41 (23) = happyShift action_32
action_41 (26) = happyShift action_17
action_41 (27) = happyShift action_18
action_41 (31) = happyShift action_19
action_41 (5) = happyGoto action_2
action_41 (6) = happyGoto action_3
action_41 (12) = happyGoto action_49
action_41 (13) = happyGoto action_7
action_41 (14) = happyGoto action_8
action_41 (18) = happyGoto action_31
action_41 (19) = happyGoto action_12
action_41 (20) = happyGoto action_13
action_41 (21) = happyGoto action_14
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (22) = happyShift action_15
action_42 (23) = happyShift action_32
action_42 (26) = happyShift action_17
action_42 (27) = happyShift action_18
action_42 (31) = happyShift action_19
action_42 (5) = happyGoto action_2
action_42 (6) = happyGoto action_3
action_42 (12) = happyGoto action_48
action_42 (13) = happyGoto action_7
action_42 (14) = happyGoto action_8
action_42 (18) = happyGoto action_31
action_42 (19) = happyGoto action_12
action_42 (20) = happyGoto action_13
action_42 (21) = happyGoto action_14
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (22) = happyShift action_15
action_43 (23) = happyShift action_47
action_43 (26) = happyShift action_17
action_43 (27) = happyShift action_18
action_43 (31) = happyShift action_19
action_43 (33) = happyShift action_20
action_43 (5) = happyGoto action_2
action_43 (6) = happyGoto action_3
action_43 (7) = happyGoto action_4
action_43 (8) = happyGoto action_44
action_43 (9) = happyGoto action_45
action_43 (11) = happyGoto action_6
action_43 (13) = happyGoto action_7
action_43 (14) = happyGoto action_8
action_43 (16) = happyGoto action_46
action_43 (17) = happyGoto action_10
action_43 (18) = happyGoto action_11
action_43 (19) = happyGoto action_12
action_43 (20) = happyGoto action_13
action_43 (21) = happyGoto action_14
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (25) = happyShift action_65
action_44 (32) = happyShift action_71
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (25) = happyShift action_63
action_45 (32) = happyShift action_70
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_13

action_47 (26) = happyReduce_20
action_47 (27) = happyReduce_20
action_47 (28) = happyReduce_24
action_47 (29) = happyReduce_24
action_47 (30) = happyReduce_26
action_47 (31) = happyShift action_25
action_47 _ = happyReduce_16

action_48 _ = happyReduce_33

action_49 _ = happyReduce_32

action_50 (30) = happyShift action_38
action_50 _ = happyReduce_38

action_51 (30) = happyShift action_38
action_51 _ = happyReduce_39

action_52 _ = happyReduce_42

action_53 _ = happyReduce_29

action_54 (31) = happyShift action_25
action_54 _ = happyReduce_28

action_55 (31) = happyShift action_69
action_55 _ = happyReduce_45

action_56 (23) = happyShift action_68
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (22) = happyShift action_15
action_57 (23) = happyShift action_16
action_57 (26) = happyShift action_17
action_57 (27) = happyShift action_18
action_57 (31) = happyShift action_19
action_57 (33) = happyShift action_20
action_57 (5) = happyGoto action_2
action_57 (6) = happyGoto action_3
action_57 (7) = happyGoto action_4
action_57 (10) = happyGoto action_67
action_57 (11) = happyGoto action_6
action_57 (13) = happyGoto action_7
action_57 (14) = happyGoto action_8
action_57 (16) = happyGoto action_9
action_57 (17) = happyGoto action_10
action_57 (18) = happyGoto action_11
action_57 (19) = happyGoto action_12
action_57 (20) = happyGoto action_13
action_57 (21) = happyGoto action_14
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_2

action_59 (25) = happyShift action_65
action_59 (32) = happyShift action_66
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (25) = happyShift action_63
action_60 (32) = happyShift action_64
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_4

action_62 _ = happyReduce_3

action_63 (22) = happyShift action_15
action_63 (23) = happyShift action_76
action_63 (26) = happyShift action_17
action_63 (27) = happyShift action_18
action_63 (31) = happyShift action_19
action_63 (33) = happyShift action_20
action_63 (5) = happyGoto action_2
action_63 (6) = happyGoto action_3
action_63 (7) = happyGoto action_4
action_63 (11) = happyGoto action_6
action_63 (13) = happyGoto action_7
action_63 (14) = happyGoto action_8
action_63 (16) = happyGoto action_75
action_63 (17) = happyGoto action_10
action_63 (18) = happyGoto action_11
action_63 (19) = happyGoto action_12
action_63 (20) = happyGoto action_13
action_63 (21) = happyGoto action_14
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_5

action_65 (22) = happyShift action_15
action_65 (23) = happyShift action_16
action_65 (26) = happyShift action_17
action_65 (27) = happyShift action_18
action_65 (31) = happyShift action_19
action_65 (33) = happyShift action_20
action_65 (5) = happyGoto action_2
action_65 (6) = happyGoto action_3
action_65 (7) = happyGoto action_4
action_65 (10) = happyGoto action_74
action_65 (11) = happyGoto action_6
action_65 (13) = happyGoto action_7
action_65 (14) = happyGoto action_8
action_65 (16) = happyGoto action_9
action_65 (17) = happyGoto action_10
action_65 (18) = happyGoto action_11
action_65 (19) = happyGoto action_12
action_65 (20) = happyGoto action_13
action_65 (21) = happyGoto action_14
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_7

action_67 _ = happyReduce_12

action_68 _ = happyReduce_17

action_69 (22) = happyShift action_15
action_69 (23) = happyShift action_47
action_69 (26) = happyShift action_17
action_69 (27) = happyShift action_18
action_69 (31) = happyShift action_19
action_69 (33) = happyShift action_20
action_69 (5) = happyGoto action_2
action_69 (6) = happyGoto action_3
action_69 (7) = happyGoto action_4
action_69 (8) = happyGoto action_72
action_69 (9) = happyGoto action_73
action_69 (11) = happyGoto action_6
action_69 (13) = happyGoto action_7
action_69 (14) = happyGoto action_8
action_69 (16) = happyGoto action_46
action_69 (17) = happyGoto action_10
action_69 (18) = happyGoto action_11
action_69 (19) = happyGoto action_12
action_69 (20) = happyGoto action_13
action_69 (21) = happyGoto action_14
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_9

action_71 _ = happyReduce_8

action_72 (25) = happyShift action_65
action_72 (32) = happyShift action_78
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (25) = happyShift action_63
action_73 (32) = happyShift action_77
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_14

action_75 _ = happyReduce_15

action_76 (26) = happyReduce_20
action_76 (27) = happyReduce_20
action_76 (28) = happyReduce_24
action_76 (29) = happyReduce_24
action_76 (30) = happyReduce_26
action_76 (31) = happyShift action_25
action_76 _ = happyReduce_17

action_77 _ = happyReduce_11

action_78 _ = happyReduce_10

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn4
		 (Expr happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (makeDef happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn4
		 (Def happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 _
	_
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn5
		 (IdFunc happy_var_1 []
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IdFunc happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (idFunc2App happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 6 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (App (Var happy_var_1) (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 4 6 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (App happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 6 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (App happy_var_1 (idlist2list (reverse happy_var_3))
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 6 6 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (App happy_var_2 (reverse happy_var_5)
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 6 6 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (App happy_var_2 (idlist2list (reverse happy_var_5))
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 7 happyReduction_12
happyReduction_12 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Abs (reverse happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3 : happy_var_1
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3 : (idlist2list happy_var_1)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyTerminal (TokenId happy_var_3))
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn10
		 (Var happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  10 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn11
		 (Var happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  12 happyReduction_22
happyReduction_22 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn12
		 (Var happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  13 happyReduction_24
happyReduction_24 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn13
		 (Var happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  13 happyReduction_25
happyReduction_25 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn14
		 (Var happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15 happyReduction_28
happyReduction_28 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn15
		 (Var happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15 happyReduction_29
happyReduction_29 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  16 happyReduction_30
happyReduction_30 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  16 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  17 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn17
		 (BinOp Add happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  17 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn17
		 (BinOp Sub happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (BinOp Mul (Num (-1.0)) happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  18 happyReduction_36
happyReduction_36 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  18 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn19
		 (BinOp Div happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  19 happyReduction_39
happyReduction_39 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn19
		 (BinOp Mul happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  19 happyReduction_40
happyReduction_40 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn19
		 (BinOp Mul (Num happy_var_1) happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  19 happyReduction_41
happyReduction_41 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  20 happyReduction_42
happyReduction_42 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn20
		 (BinOp Pow happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  20 happyReduction_43
happyReduction_43 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn21
		 (Num happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  21 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  21 happyReduction_46
happyReduction_46 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 22;
	TokenId happy_dollar_dollar -> cont 23;
	TokenEq -> cont 24;
	TokenComma -> cont 25;
	TokenAdd -> cont 26;
	TokenSub -> cont 27;
	TokenMul -> cont 28;
	TokenDiv -> cont 29;
	TokenPow -> cont 30;
	TokenLB -> cont 31;
	TokenRB -> cont 32;
	TokenLam -> cont 33;
	TokenArr -> cont 34;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 35 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either MyError a -> (a -> Either MyError b) -> Either MyError b
happyThen = ((>>=))
happyReturn :: () => a -> Either MyError a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either MyError a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Either MyError a
happyError' = (\(tokens, _) -> parseError tokens)
calc tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either MyError a 
parseError tokenList = Left $ ParseError $ "Parse error at " ++ show tokenList

data IdFunc = IdFunc String [String] deriving(Show)

idlist2list :: [String] -> [Expr]
idlist2list [] = []
idlist2list (x:xs) = (Var x) : (idlist2list xs)

idFunc2App :: IdFunc -> Expr 
idFunc2App (IdFunc i idList) = App (Var i) (idlist2list idList)

makeDef :: IdFunc -> Expr -> Stmt 
makeDef (IdFunc s idList) ex = Def s (Abs idList ex)



parse :: (Monad m) => String -> MyException m Stmt
parse s = do
    tokens <- lexer s
    let res = calc tokens
    
    case res of
        Left err -> throwE err
        Right stmt -> return stmt
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
