{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParBNF where
import AbsBNF
import LexBNF
import ErrM
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn35 (String)
	| HappyAbsSyn36 (Ident)
	| HappyAbsSyn37 (Integer)
	| HappyAbsSyn38 (Char)
	| HappyAbsSyn39 (Double)
	| HappyAbsSyn40 (LGrammar)
	| HappyAbsSyn41 (LDef)
	| HappyAbsSyn42 ([LDef])
	| HappyAbsSyn43 (Grammar)
	| HappyAbsSyn44 ([Def])
	| HappyAbsSyn45 ([Item])
	| HappyAbsSyn46 (Def)
	| HappyAbsSyn47 (Item)
	| HappyAbsSyn48 (Cat)
	| HappyAbsSyn49 (Label)
	| HappyAbsSyn50 (LabelId)
	| HappyAbsSyn51 (ProfItem)
	| HappyAbsSyn52 (IntList)
	| HappyAbsSyn53 ([Integer])
	| HappyAbsSyn54 ([IntList])
	| HappyAbsSyn55 ([ProfItem])
	| HappyAbsSyn56 (Arg)
	| HappyAbsSyn57 ([Arg])
	| HappyAbsSyn58 (Exp)
	| HappyAbsSyn61 ([Exp])
	| HappyAbsSyn63 ([String])
	| HappyAbsSyn64 ([RHS])
	| HappyAbsSyn65 (RHS)
	| HappyAbsSyn66 (MinimumSize)
	| HappyAbsSyn67 (Reg)
	| HappyAbsSyn71 ([Ident])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Happy_GHC_Exts.Int# 
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
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240 :: () => Happy_GHC_Exts.Int# -> ({-HappyReduction (Err) = -}
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135 :: () => ({-HappyReduction (Err) = -}
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (72#) = happyShift action_97
action_0 (84#) = happyShift action_98
action_0 (86#) = happyShift action_99
action_0 (88#) = happyShift action_110
action_0 (89#) = happyShift action_111
action_0 (90#) = happyShift action_112
action_0 (91#) = happyShift action_113
action_0 (93#) = happyShift action_114
action_0 (95#) = happyShift action_115
action_0 (96#) = happyShift action_116
action_0 (100#) = happyShift action_117
action_0 (101#) = happyShift action_118
action_0 (102#) = happyShift action_119
action_0 (104#) = happyShift action_120
action_0 (105#) = happyShift action_121
action_0 (108#) = happyShift action_132
action_0 (113#) = happyShift action_36
action_0 (36#) = happyGoto action_127
action_0 (40#) = happyGoto action_134
action_0 (41#) = happyGoto action_128
action_0 (42#) = happyGoto action_135
action_0 (46#) = happyGoto action_130
action_0 (49#) = happyGoto action_109
action_0 (50#) = happyGoto action_101
action_0 (71#) = happyGoto action_131
action_0 x = happyTcHack x happyReduce_41

action_1 (72#) = happyShift action_97
action_1 (84#) = happyShift action_98
action_1 (86#) = happyShift action_99
action_1 (88#) = happyShift action_110
action_1 (89#) = happyShift action_111
action_1 (90#) = happyShift action_112
action_1 (91#) = happyShift action_113
action_1 (93#) = happyShift action_114
action_1 (95#) = happyShift action_115
action_1 (96#) = happyShift action_116
action_1 (100#) = happyShift action_117
action_1 (101#) = happyShift action_118
action_1 (102#) = happyShift action_119
action_1 (104#) = happyShift action_120
action_1 (105#) = happyShift action_121
action_1 (108#) = happyShift action_132
action_1 (113#) = happyShift action_36
action_1 (36#) = happyGoto action_127
action_1 (41#) = happyGoto action_133
action_1 (46#) = happyGoto action_130
action_1 (49#) = happyGoto action_109
action_1 (50#) = happyGoto action_101
action_1 (71#) = happyGoto action_131
action_1 x = happyTcHack x happyFail

action_2 (72#) = happyShift action_97
action_2 (84#) = happyShift action_98
action_2 (86#) = happyShift action_99
action_2 (88#) = happyShift action_110
action_2 (89#) = happyShift action_111
action_2 (90#) = happyShift action_112
action_2 (91#) = happyShift action_113
action_2 (93#) = happyShift action_114
action_2 (95#) = happyShift action_115
action_2 (96#) = happyShift action_116
action_2 (100#) = happyShift action_117
action_2 (101#) = happyShift action_118
action_2 (102#) = happyShift action_119
action_2 (104#) = happyShift action_120
action_2 (105#) = happyShift action_121
action_2 (108#) = happyShift action_132
action_2 (113#) = happyShift action_36
action_2 (36#) = happyGoto action_127
action_2 (41#) = happyGoto action_128
action_2 (42#) = happyGoto action_129
action_2 (46#) = happyGoto action_130
action_2 (49#) = happyGoto action_109
action_2 (50#) = happyGoto action_101
action_2 (71#) = happyGoto action_131
action_2 x = happyTcHack x happyReduce_41

action_3 (72#) = happyShift action_97
action_3 (84#) = happyShift action_98
action_3 (86#) = happyShift action_99
action_3 (88#) = happyShift action_110
action_3 (89#) = happyShift action_111
action_3 (90#) = happyShift action_112
action_3 (91#) = happyShift action_113
action_3 (93#) = happyShift action_114
action_3 (95#) = happyShift action_115
action_3 (96#) = happyShift action_116
action_3 (100#) = happyShift action_117
action_3 (101#) = happyShift action_118
action_3 (102#) = happyShift action_119
action_3 (104#) = happyShift action_120
action_3 (105#) = happyShift action_121
action_3 (113#) = happyShift action_36
action_3 (36#) = happyGoto action_95
action_3 (43#) = happyGoto action_125
action_3 (44#) = happyGoto action_126
action_3 (46#) = happyGoto action_124
action_3 (49#) = happyGoto action_109
action_3 (50#) = happyGoto action_101
action_3 x = happyTcHack x happyReduce_45

action_4 (72#) = happyShift action_97
action_4 (84#) = happyShift action_98
action_4 (86#) = happyShift action_99
action_4 (88#) = happyShift action_110
action_4 (89#) = happyShift action_111
action_4 (90#) = happyShift action_112
action_4 (91#) = happyShift action_113
action_4 (93#) = happyShift action_114
action_4 (95#) = happyShift action_115
action_4 (96#) = happyShift action_116
action_4 (100#) = happyShift action_117
action_4 (101#) = happyShift action_118
action_4 (102#) = happyShift action_119
action_4 (104#) = happyShift action_120
action_4 (105#) = happyShift action_121
action_4 (113#) = happyShift action_36
action_4 (36#) = happyGoto action_95
action_4 (44#) = happyGoto action_123
action_4 (46#) = happyGoto action_124
action_4 (49#) = happyGoto action_109
action_4 (50#) = happyGoto action_101
action_4 x = happyTcHack x happyReduce_45

action_5 (45#) = happyGoto action_122
action_5 x = happyTcHack x happyReduce_48

action_6 (72#) = happyShift action_97
action_6 (84#) = happyShift action_98
action_6 (86#) = happyShift action_99
action_6 (88#) = happyShift action_110
action_6 (89#) = happyShift action_111
action_6 (90#) = happyShift action_112
action_6 (91#) = happyShift action_113
action_6 (93#) = happyShift action_114
action_6 (95#) = happyShift action_115
action_6 (96#) = happyShift action_116
action_6 (100#) = happyShift action_117
action_6 (101#) = happyShift action_118
action_6 (102#) = happyShift action_119
action_6 (104#) = happyShift action_120
action_6 (105#) = happyShift action_121
action_6 (113#) = happyShift action_36
action_6 (36#) = happyGoto action_95
action_6 (46#) = happyGoto action_108
action_6 (49#) = happyGoto action_109
action_6 (50#) = happyGoto action_101
action_6 x = happyTcHack x happyFail

action_7 (84#) = happyShift action_104
action_7 (112#) = happyShift action_33
action_7 (113#) = happyShift action_36
action_7 (35#) = happyGoto action_105
action_7 (36#) = happyGoto action_102
action_7 (47#) = happyGoto action_106
action_7 (48#) = happyGoto action_107
action_7 x = happyTcHack x happyFail

action_8 (84#) = happyShift action_104
action_8 (113#) = happyShift action_36
action_8 (36#) = happyGoto action_102
action_8 (48#) = happyGoto action_103
action_8 x = happyTcHack x happyFail

action_9 (72#) = happyShift action_97
action_9 (84#) = happyShift action_98
action_9 (86#) = happyShift action_99
action_9 (113#) = happyShift action_36
action_9 (36#) = happyGoto action_95
action_9 (49#) = happyGoto action_100
action_9 (50#) = happyGoto action_101
action_9 x = happyTcHack x happyFail

action_10 (72#) = happyShift action_97
action_10 (84#) = happyShift action_98
action_10 (86#) = happyShift action_99
action_10 (113#) = happyShift action_36
action_10 (36#) = happyGoto action_95
action_10 (50#) = happyGoto action_96
action_10 x = happyTcHack x happyFail

action_11 (72#) = happyShift action_87
action_11 (51#) = happyGoto action_94
action_11 x = happyTcHack x happyFail

action_12 (84#) = happyShift action_90
action_12 (52#) = happyGoto action_93
action_12 x = happyTcHack x happyFail

action_13 (114#) = happyShift action_74
action_13 (37#) = happyGoto action_91
action_13 (53#) = happyGoto action_92
action_13 x = happyTcHack x happyReduce_81

action_14 (84#) = happyShift action_90
action_14 (52#) = happyGoto action_88
action_14 (54#) = happyGoto action_89
action_14 x = happyTcHack x happyReduce_84

action_15 (72#) = happyShift action_87
action_15 (51#) = happyGoto action_85
action_15 (55#) = happyGoto action_86
action_15 x = happyTcHack x happyFail

action_16 (113#) = happyShift action_36
action_16 (36#) = happyGoto action_83
action_16 (56#) = happyGoto action_84
action_16 x = happyTcHack x happyFail

action_17 (57#) = happyGoto action_82
action_17 x = happyTcHack x happyReduce_90

action_18 (72#) = happyShift action_72
action_18 (84#) = happyShift action_73
action_18 (112#) = happyShift action_33
action_18 (113#) = happyShift action_36
action_18 (114#) = happyShift action_74
action_18 (115#) = happyShift action_51
action_18 (116#) = happyShift action_75
action_18 (35#) = happyGoto action_63
action_18 (36#) = happyGoto action_64
action_18 (37#) = happyGoto action_65
action_18 (38#) = happyGoto action_66
action_18 (39#) = happyGoto action_67
action_18 (58#) = happyGoto action_81
action_18 (59#) = happyGoto action_69
action_18 (60#) = happyGoto action_70
action_18 x = happyTcHack x happyFail

action_19 (72#) = happyShift action_72
action_19 (84#) = happyShift action_73
action_19 (112#) = happyShift action_33
action_19 (113#) = happyShift action_36
action_19 (114#) = happyShift action_74
action_19 (115#) = happyShift action_51
action_19 (116#) = happyShift action_75
action_19 (35#) = happyGoto action_63
action_19 (36#) = happyGoto action_64
action_19 (37#) = happyGoto action_65
action_19 (38#) = happyGoto action_66
action_19 (39#) = happyGoto action_67
action_19 (59#) = happyGoto action_80
action_19 (60#) = happyGoto action_70
action_19 x = happyTcHack x happyFail

action_20 (72#) = happyShift action_72
action_20 (84#) = happyShift action_73
action_20 (112#) = happyShift action_33
action_20 (113#) = happyShift action_36
action_20 (114#) = happyShift action_74
action_20 (115#) = happyShift action_51
action_20 (116#) = happyShift action_75
action_20 (35#) = happyGoto action_63
action_20 (36#) = happyGoto action_76
action_20 (37#) = happyGoto action_65
action_20 (38#) = happyGoto action_66
action_20 (39#) = happyGoto action_67
action_20 (60#) = happyGoto action_79
action_20 x = happyTcHack x happyFail

action_21 (72#) = happyShift action_72
action_21 (84#) = happyShift action_73
action_21 (112#) = happyShift action_33
action_21 (113#) = happyShift action_36
action_21 (114#) = happyShift action_74
action_21 (115#) = happyShift action_51
action_21 (116#) = happyShift action_75
action_21 (35#) = happyGoto action_63
action_21 (36#) = happyGoto action_76
action_21 (37#) = happyGoto action_65
action_21 (38#) = happyGoto action_66
action_21 (39#) = happyGoto action_67
action_21 (60#) = happyGoto action_77
action_21 (61#) = happyGoto action_78
action_21 x = happyTcHack x happyFail

action_22 (72#) = happyShift action_72
action_22 (84#) = happyShift action_73
action_22 (112#) = happyShift action_33
action_22 (113#) = happyShift action_36
action_22 (114#) = happyShift action_74
action_22 (115#) = happyShift action_51
action_22 (116#) = happyShift action_75
action_22 (35#) = happyGoto action_63
action_22 (36#) = happyGoto action_64
action_22 (37#) = happyGoto action_65
action_22 (38#) = happyGoto action_66
action_22 (39#) = happyGoto action_67
action_22 (58#) = happyGoto action_68
action_22 (59#) = happyGoto action_69
action_22 (60#) = happyGoto action_70
action_22 (62#) = happyGoto action_71
action_22 x = happyTcHack x happyReduce_105

action_23 (112#) = happyShift action_33
action_23 (35#) = happyGoto action_61
action_23 (63#) = happyGoto action_62
action_23 x = happyTcHack x happyFail

action_24 (45#) = happyGoto action_57
action_24 (64#) = happyGoto action_59
action_24 (65#) = happyGoto action_60
action_24 x = happyTcHack x happyReduce_48

action_25 (45#) = happyGoto action_57
action_25 (65#) = happyGoto action_58
action_25 x = happyTcHack x happyReduce_48

action_26 (99#) = happyShift action_56
action_26 (66#) = happyGoto action_55
action_26 x = happyTcHack x happyReduce_114

action_27 (72#) = happyShift action_42
action_27 (84#) = happyShift action_43
action_27 (87#) = happyShift action_44
action_27 (92#) = happyShift action_45
action_27 (94#) = happyShift action_46
action_27 (97#) = happyShift action_47
action_27 (98#) = happyShift action_48
action_27 (107#) = happyShift action_49
action_27 (109#) = happyShift action_50
action_27 (115#) = happyShift action_51
action_27 (38#) = happyGoto action_37
action_27 (67#) = happyGoto action_54
action_27 (69#) = happyGoto action_40
action_27 x = happyTcHack x happyFail

action_28 (72#) = happyShift action_42
action_28 (84#) = happyShift action_43
action_28 (87#) = happyShift action_44
action_28 (92#) = happyShift action_45
action_28 (94#) = happyShift action_46
action_28 (97#) = happyShift action_47
action_28 (98#) = happyShift action_48
action_28 (107#) = happyShift action_49
action_28 (109#) = happyShift action_50
action_28 (115#) = happyShift action_51
action_28 (38#) = happyGoto action_37
action_28 (67#) = happyGoto action_38
action_28 (68#) = happyGoto action_53
action_28 (69#) = happyGoto action_40
action_28 x = happyTcHack x happyFail

action_29 (72#) = happyShift action_42
action_29 (84#) = happyShift action_43
action_29 (87#) = happyShift action_44
action_29 (92#) = happyShift action_45
action_29 (94#) = happyShift action_46
action_29 (97#) = happyShift action_47
action_29 (98#) = happyShift action_48
action_29 (107#) = happyShift action_49
action_29 (109#) = happyShift action_50
action_29 (115#) = happyShift action_51
action_29 (38#) = happyGoto action_37
action_29 (69#) = happyGoto action_52
action_29 x = happyTcHack x happyFail

action_30 (72#) = happyShift action_42
action_30 (84#) = happyShift action_43
action_30 (87#) = happyShift action_44
action_30 (92#) = happyShift action_45
action_30 (94#) = happyShift action_46
action_30 (97#) = happyShift action_47
action_30 (98#) = happyShift action_48
action_30 (107#) = happyShift action_49
action_30 (109#) = happyShift action_50
action_30 (115#) = happyShift action_51
action_30 (38#) = happyGoto action_37
action_30 (67#) = happyGoto action_38
action_30 (68#) = happyGoto action_39
action_30 (69#) = happyGoto action_40
action_30 (70#) = happyGoto action_41
action_30 x = happyTcHack x happyFail

action_31 (113#) = happyShift action_36
action_31 (36#) = happyGoto action_34
action_31 (71#) = happyGoto action_35
action_31 x = happyTcHack x happyFail

action_32 (112#) = happyShift action_33
action_32 x = happyTcHack x happyFail

action_33 x = happyTcHack x happyReduce_32

action_34 (76#) = happyShift action_139
action_34 x = happyTcHack x happyReduce_134

action_35 (118#) = happyAccept
action_35 x = happyTcHack x happyFail

action_36 x = happyTcHack x happyReduce_33

action_37 x = happyTcHack x happyReduce_124

action_38 (72#) = happyShift action_42
action_38 (77#) = happyShift action_185
action_38 (84#) = happyShift action_43
action_38 (87#) = happyShift action_44
action_38 (92#) = happyShift action_45
action_38 (94#) = happyShift action_46
action_38 (97#) = happyShift action_47
action_38 (98#) = happyShift action_48
action_38 (107#) = happyShift action_49
action_38 (109#) = happyShift action_50
action_38 (115#) = happyShift action_51
action_38 (38#) = happyGoto action_37
action_38 (69#) = happyGoto action_177
action_38 x = happyTcHack x happyReduce_119

action_39 (110#) = happyShift action_178
action_39 x = happyTcHack x happyReduce_133

action_40 (74#) = happyShift action_179
action_40 (75#) = happyShift action_180
action_40 (83#) = happyShift action_181
action_40 x = happyTcHack x happyReduce_116

action_41 (118#) = happyAccept
action_41 x = happyTcHack x happyFail

action_42 (72#) = happyShift action_42
action_42 (84#) = happyShift action_43
action_42 (87#) = happyShift action_44
action_42 (92#) = happyShift action_45
action_42 (94#) = happyShift action_46
action_42 (97#) = happyShift action_47
action_42 (98#) = happyShift action_48
action_42 (107#) = happyShift action_49
action_42 (109#) = happyShift action_50
action_42 (115#) = happyShift action_51
action_42 (38#) = happyGoto action_37
action_42 (67#) = happyGoto action_38
action_42 (68#) = happyGoto action_39
action_42 (69#) = happyGoto action_40
action_42 (70#) = happyGoto action_184
action_42 x = happyTcHack x happyFail

action_43 (112#) = happyShift action_33
action_43 (35#) = happyGoto action_183
action_43 x = happyTcHack x happyFail

action_44 x = happyTcHack x happyReduce_131

action_45 x = happyTcHack x happyReduce_127

action_46 x = happyTcHack x happyReduce_123

action_47 x = happyTcHack x happyReduce_128

action_48 x = happyTcHack x happyReduce_130

action_49 x = happyTcHack x happyReduce_129

action_50 (112#) = happyShift action_33
action_50 (35#) = happyGoto action_182
action_50 x = happyTcHack x happyFail

action_51 x = happyTcHack x happyReduce_35

action_52 (74#) = happyShift action_179
action_52 (75#) = happyShift action_180
action_52 (83#) = happyShift action_181
action_52 (118#) = happyAccept
action_52 x = happyTcHack x happyFail

action_53 (110#) = happyShift action_178
action_53 (118#) = happyAccept
action_53 x = happyTcHack x happyFail

action_54 (72#) = happyShift action_42
action_54 (84#) = happyShift action_43
action_54 (87#) = happyShift action_44
action_54 (92#) = happyShift action_45
action_54 (94#) = happyShift action_46
action_54 (97#) = happyShift action_47
action_54 (98#) = happyShift action_48
action_54 (107#) = happyShift action_49
action_54 (109#) = happyShift action_50
action_54 (115#) = happyShift action_51
action_54 (118#) = happyAccept
action_54 (38#) = happyGoto action_37
action_54 (69#) = happyGoto action_177
action_54 x = happyTcHack x happyFail

action_55 (118#) = happyAccept
action_55 x = happyTcHack x happyFail

action_56 x = happyTcHack x happyReduce_113

action_57 (84#) = happyShift action_104
action_57 (112#) = happyShift action_33
action_57 (113#) = happyShift action_36
action_57 (35#) = happyGoto action_105
action_57 (36#) = happyGoto action_102
action_57 (47#) = happyGoto action_141
action_57 (48#) = happyGoto action_107
action_57 x = happyTcHack x happyReduce_112

action_58 (118#) = happyAccept
action_58 x = happyTcHack x happyFail

action_59 (118#) = happyAccept
action_59 x = happyTcHack x happyFail

action_60 (110#) = happyShift action_176
action_60 x = happyTcHack x happyReduce_110

action_61 (76#) = happyShift action_175
action_61 x = happyTcHack x happyReduce_108

action_62 (118#) = happyAccept
action_62 x = happyTcHack x happyFail

action_63 x = happyTcHack x happyReduce_99

action_64 (72#) = happyShift action_72
action_64 (84#) = happyShift action_73
action_64 (112#) = happyShift action_33
action_64 (113#) = happyShift action_36
action_64 (114#) = happyShift action_74
action_64 (115#) = happyShift action_51
action_64 (116#) = happyShift action_75
action_64 (35#) = happyGoto action_63
action_64 (36#) = happyGoto action_76
action_64 (37#) = happyGoto action_65
action_64 (38#) = happyGoto action_66
action_64 (39#) = happyGoto action_67
action_64 (60#) = happyGoto action_77
action_64 (61#) = happyGoto action_174
action_64 x = happyTcHack x happyReduce_96

action_65 x = happyTcHack x happyReduce_97

action_66 x = happyTcHack x happyReduce_98

action_67 x = happyTcHack x happyReduce_100

action_68 (76#) = happyShift action_173
action_68 x = happyTcHack x happyReduce_106

action_69 (79#) = happyShift action_172
action_69 x = happyTcHack x happyReduce_93

action_70 x = happyTcHack x happyReduce_95

action_71 (118#) = happyAccept
action_71 x = happyTcHack x happyFail

action_72 (72#) = happyShift action_72
action_72 (84#) = happyShift action_73
action_72 (112#) = happyShift action_33
action_72 (113#) = happyShift action_36
action_72 (114#) = happyShift action_74
action_72 (115#) = happyShift action_51
action_72 (116#) = happyShift action_75
action_72 (35#) = happyGoto action_63
action_72 (36#) = happyGoto action_64
action_72 (37#) = happyGoto action_65
action_72 (38#) = happyGoto action_66
action_72 (39#) = happyGoto action_67
action_72 (58#) = happyGoto action_171
action_72 (59#) = happyGoto action_69
action_72 (60#) = happyGoto action_70
action_72 x = happyTcHack x happyFail

action_73 (72#) = happyShift action_72
action_73 (84#) = happyShift action_73
action_73 (112#) = happyShift action_33
action_73 (113#) = happyShift action_36
action_73 (114#) = happyShift action_74
action_73 (115#) = happyShift action_51
action_73 (116#) = happyShift action_75
action_73 (35#) = happyGoto action_63
action_73 (36#) = happyGoto action_64
action_73 (37#) = happyGoto action_65
action_73 (38#) = happyGoto action_66
action_73 (39#) = happyGoto action_67
action_73 (58#) = happyGoto action_68
action_73 (59#) = happyGoto action_69
action_73 (60#) = happyGoto action_70
action_73 (62#) = happyGoto action_170
action_73 x = happyTcHack x happyReduce_105

action_74 x = happyTcHack x happyReduce_34

action_75 x = happyTcHack x happyReduce_36

action_76 x = happyTcHack x happyReduce_96

action_77 (72#) = happyShift action_72
action_77 (84#) = happyShift action_73
action_77 (112#) = happyShift action_33
action_77 (113#) = happyShift action_36
action_77 (114#) = happyShift action_74
action_77 (115#) = happyShift action_51
action_77 (116#) = happyShift action_75
action_77 (35#) = happyGoto action_63
action_77 (36#) = happyGoto action_76
action_77 (37#) = happyGoto action_65
action_77 (38#) = happyGoto action_66
action_77 (39#) = happyGoto action_67
action_77 (60#) = happyGoto action_77
action_77 (61#) = happyGoto action_169
action_77 x = happyTcHack x happyReduce_103

action_78 (118#) = happyAccept
action_78 x = happyTcHack x happyFail

action_79 (118#) = happyAccept
action_79 x = happyTcHack x happyFail

action_80 (118#) = happyAccept
action_80 x = happyTcHack x happyFail

action_81 (118#) = happyAccept
action_81 x = happyTcHack x happyFail

action_82 (113#) = happyShift action_36
action_82 (118#) = happyAccept
action_82 (36#) = happyGoto action_83
action_82 (56#) = happyGoto action_168
action_82 x = happyTcHack x happyFail

action_83 x = happyTcHack x happyReduce_89

action_84 (118#) = happyAccept
action_84 x = happyTcHack x happyFail

action_85 (72#) = happyShift action_87
action_85 (51#) = happyGoto action_85
action_85 (55#) = happyGoto action_167
action_85 x = happyTcHack x happyReduce_87

action_86 (118#) = happyAccept
action_86 x = happyTcHack x happyFail

action_87 (84#) = happyShift action_166
action_87 x = happyTcHack x happyFail

action_88 (76#) = happyShift action_165
action_88 x = happyTcHack x happyReduce_85

action_89 (118#) = happyAccept
action_89 x = happyTcHack x happyFail

action_90 (114#) = happyShift action_74
action_90 (37#) = happyGoto action_91
action_90 (53#) = happyGoto action_164
action_90 x = happyTcHack x happyReduce_81

action_91 (76#) = happyShift action_163
action_91 x = happyTcHack x happyReduce_82

action_92 (118#) = happyAccept
action_92 x = happyTcHack x happyFail

action_93 (118#) = happyAccept
action_93 x = happyTcHack x happyFail

action_94 (118#) = happyAccept
action_94 x = happyTcHack x happyFail

action_95 x = happyTcHack x happyReduce_74

action_96 (118#) = happyAccept
action_96 x = happyTcHack x happyFail

action_97 (79#) = happyShift action_162
action_97 x = happyTcHack x happyFail

action_98 (85#) = happyShift action_161
action_98 x = happyTcHack x happyFail

action_99 x = happyTcHack x happyReduce_75

action_100 (118#) = happyAccept
action_100 x = happyTcHack x happyFail

action_101 (72#) = happyShift action_160
action_101 (84#) = happyShift action_98
action_101 (86#) = happyShift action_99
action_101 (113#) = happyShift action_36
action_101 (36#) = happyGoto action_95
action_101 (50#) = happyGoto action_158
action_101 (51#) = happyGoto action_85
action_101 (55#) = happyGoto action_159
action_101 x = happyTcHack x happyReduce_70

action_102 x = happyTcHack x happyReduce_69

action_103 (118#) = happyAccept
action_103 x = happyTcHack x happyFail

action_104 (84#) = happyShift action_104
action_104 (113#) = happyShift action_36
action_104 (36#) = happyGoto action_102
action_104 (48#) = happyGoto action_157
action_104 x = happyTcHack x happyFail

action_105 x = happyTcHack x happyReduce_66

action_106 (118#) = happyAccept
action_106 x = happyTcHack x happyFail

action_107 x = happyTcHack x happyReduce_67

action_108 (118#) = happyAccept
action_108 x = happyTcHack x happyFail

action_109 (78#) = happyShift action_156
action_109 x = happyTcHack x happyFail

action_110 (113#) = happyShift action_36
action_110 (36#) = happyGoto action_155
action_110 x = happyTcHack x happyFail

action_111 (112#) = happyShift action_33
action_111 (35#) = happyGoto action_154
action_111 x = happyTcHack x happyFail

action_112 (113#) = happyShift action_36
action_112 (36#) = happyGoto action_153
action_112 x = happyTcHack x happyFail

action_113 (84#) = happyShift action_104
action_113 (113#) = happyShift action_36
action_113 (36#) = happyGoto action_102
action_113 (48#) = happyGoto action_152
action_113 x = happyTcHack x happyFail

action_114 (113#) = happyShift action_36
action_114 (36#) = happyGoto action_34
action_114 (71#) = happyGoto action_151
action_114 x = happyTcHack x happyFail

action_115 (72#) = happyShift action_97
action_115 (84#) = happyShift action_98
action_115 (86#) = happyShift action_99
action_115 (113#) = happyShift action_36
action_115 (36#) = happyGoto action_95
action_115 (49#) = happyGoto action_150
action_115 (50#) = happyGoto action_101
action_115 x = happyTcHack x happyFail

action_116 (103#) = happyShift action_148
action_116 (106#) = happyShift action_149
action_116 (112#) = happyShift action_33
action_116 (35#) = happyGoto action_61
action_116 (63#) = happyGoto action_147
action_116 x = happyTcHack x happyFail

action_117 (105#) = happyShift action_146
action_117 x = happyTcHack x happyFail

action_118 (113#) = happyShift action_36
action_118 (36#) = happyGoto action_145
action_118 x = happyTcHack x happyFail

action_119 (99#) = happyShift action_56
action_119 (66#) = happyGoto action_144
action_119 x = happyTcHack x happyReduce_114

action_120 (99#) = happyShift action_56
action_120 (66#) = happyGoto action_143
action_120 x = happyTcHack x happyReduce_114

action_121 (113#) = happyShift action_36
action_121 (36#) = happyGoto action_142
action_121 x = happyTcHack x happyFail

action_122 (84#) = happyShift action_104
action_122 (112#) = happyShift action_33
action_122 (113#) = happyShift action_36
action_122 (118#) = happyAccept
action_122 (35#) = happyGoto action_105
action_122 (36#) = happyGoto action_102
action_122 (47#) = happyGoto action_141
action_122 (48#) = happyGoto action_107
action_122 x = happyTcHack x happyFail

action_123 (118#) = happyAccept
action_123 x = happyTcHack x happyFail

action_124 (81#) = happyShift action_140
action_124 x = happyTcHack x happyReduce_46

action_125 (118#) = happyAccept
action_125 x = happyTcHack x happyFail

action_126 x = happyTcHack x happyReduce_44

action_127 (76#) = happyShift action_139
action_127 (79#) = happyReduce_134
action_127 x = happyTcHack x happyReduce_74

action_128 (81#) = happyShift action_138
action_128 x = happyTcHack x happyReduce_42

action_129 (118#) = happyAccept
action_129 x = happyTcHack x happyFail

action_130 x = happyTcHack x happyReduce_38

action_131 (79#) = happyShift action_137
action_131 x = happyTcHack x happyFail

action_132 (113#) = happyShift action_36
action_132 (36#) = happyGoto action_34
action_132 (71#) = happyGoto action_136
action_132 x = happyTcHack x happyFail

action_133 (118#) = happyAccept
action_133 x = happyTcHack x happyFail

action_134 (118#) = happyAccept
action_134 x = happyTcHack x happyFail

action_135 x = happyTcHack x happyReduce_37

action_136 x = happyTcHack x happyReduce_40

action_137 (72#) = happyShift action_97
action_137 (84#) = happyShift action_98
action_137 (86#) = happyShift action_99
action_137 (88#) = happyShift action_110
action_137 (89#) = happyShift action_111
action_137 (90#) = happyShift action_112
action_137 (91#) = happyShift action_113
action_137 (93#) = happyShift action_114
action_137 (95#) = happyShift action_115
action_137 (96#) = happyShift action_116
action_137 (100#) = happyShift action_117
action_137 (101#) = happyShift action_118
action_137 (102#) = happyShift action_119
action_137 (104#) = happyShift action_120
action_137 (105#) = happyShift action_121
action_137 (113#) = happyShift action_36
action_137 (36#) = happyGoto action_95
action_137 (46#) = happyGoto action_220
action_137 (49#) = happyGoto action_109
action_137 (50#) = happyGoto action_101
action_137 x = happyTcHack x happyFail

action_138 (72#) = happyShift action_97
action_138 (84#) = happyShift action_98
action_138 (86#) = happyShift action_99
action_138 (88#) = happyShift action_110
action_138 (89#) = happyShift action_111
action_138 (90#) = happyShift action_112
action_138 (91#) = happyShift action_113
action_138 (93#) = happyShift action_114
action_138 (95#) = happyShift action_115
action_138 (96#) = happyShift action_116
action_138 (100#) = happyShift action_117
action_138 (101#) = happyShift action_118
action_138 (102#) = happyShift action_119
action_138 (104#) = happyShift action_120
action_138 (105#) = happyShift action_121
action_138 (108#) = happyShift action_132
action_138 (113#) = happyShift action_36
action_138 (36#) = happyGoto action_127
action_138 (41#) = happyGoto action_128
action_138 (42#) = happyGoto action_219
action_138 (46#) = happyGoto action_130
action_138 (49#) = happyGoto action_109
action_138 (50#) = happyGoto action_101
action_138 (71#) = happyGoto action_131
action_138 x = happyTcHack x happyReduce_41

action_139 (113#) = happyShift action_36
action_139 (36#) = happyGoto action_34
action_139 (71#) = happyGoto action_218
action_139 x = happyTcHack x happyFail

action_140 (72#) = happyShift action_97
action_140 (84#) = happyShift action_98
action_140 (86#) = happyShift action_99
action_140 (88#) = happyShift action_110
action_140 (89#) = happyShift action_111
action_140 (90#) = happyShift action_112
action_140 (91#) = happyShift action_113
action_140 (93#) = happyShift action_114
action_140 (95#) = happyShift action_115
action_140 (96#) = happyShift action_116
action_140 (100#) = happyShift action_117
action_140 (101#) = happyShift action_118
action_140 (102#) = happyShift action_119
action_140 (104#) = happyShift action_120
action_140 (105#) = happyShift action_121
action_140 (113#) = happyShift action_36
action_140 (36#) = happyGoto action_95
action_140 (44#) = happyGoto action_217
action_140 (46#) = happyGoto action_124
action_140 (49#) = happyGoto action_109
action_140 (50#) = happyGoto action_101
action_140 x = happyTcHack x happyReduce_45

action_141 x = happyTcHack x happyReduce_49

action_142 (72#) = happyShift action_42
action_142 (84#) = happyShift action_43
action_142 (87#) = happyShift action_44
action_142 (92#) = happyShift action_45
action_142 (94#) = happyShift action_46
action_142 (97#) = happyShift action_47
action_142 (98#) = happyShift action_48
action_142 (107#) = happyShift action_49
action_142 (109#) = happyShift action_50
action_142 (115#) = happyShift action_51
action_142 (38#) = happyGoto action_37
action_142 (67#) = happyGoto action_38
action_142 (68#) = happyGoto action_39
action_142 (69#) = happyGoto action_40
action_142 (70#) = happyGoto action_216
action_142 x = happyTcHack x happyFail

action_143 (84#) = happyShift action_104
action_143 (113#) = happyShift action_36
action_143 (36#) = happyGoto action_102
action_143 (48#) = happyGoto action_215
action_143 x = happyTcHack x happyFail

action_144 (84#) = happyShift action_104
action_144 (113#) = happyShift action_36
action_144 (36#) = happyGoto action_102
action_144 (48#) = happyGoto action_214
action_144 x = happyTcHack x happyFail

action_145 (80#) = happyShift action_213
action_145 x = happyTcHack x happyFail

action_146 (113#) = happyShift action_36
action_146 (36#) = happyGoto action_212
action_146 x = happyTcHack x happyFail

action_147 x = happyTcHack x happyReduce_63

action_148 (112#) = happyShift action_33
action_148 (35#) = happyGoto action_61
action_148 (63#) = happyGoto action_211
action_148 x = happyTcHack x happyFail

action_149 x = happyTcHack x happyReduce_65

action_150 (78#) = happyShift action_210
action_150 x = happyTcHack x happyFail

action_151 x = happyTcHack x happyReduce_56

action_152 (112#) = happyShift action_33
action_152 (35#) = happyGoto action_209
action_152 x = happyTcHack x happyFail

action_153 (57#) = happyGoto action_208
action_153 x = happyTcHack x happyReduce_90

action_154 (112#) = happyShift action_33
action_154 (35#) = happyGoto action_207
action_154 x = happyTcHack x happyReduce_51

action_155 (114#) = happyShift action_74
action_155 (37#) = happyGoto action_206
action_155 x = happyTcHack x happyFail

action_156 (84#) = happyShift action_104
action_156 (113#) = happyShift action_36
action_156 (36#) = happyGoto action_102
action_156 (48#) = happyGoto action_205
action_156 x = happyTcHack x happyFail

action_157 (85#) = happyShift action_204
action_157 x = happyTcHack x happyFail

action_158 (72#) = happyShift action_87
action_158 (51#) = happyGoto action_85
action_158 (55#) = happyGoto action_203
action_158 x = happyTcHack x happyReduce_73

action_159 x = happyTcHack x happyReduce_71

action_160 (79#) = happyShift action_162
action_160 (84#) = happyShift action_166
action_160 x = happyTcHack x happyFail

action_161 x = happyTcHack x happyReduce_76

action_162 (73#) = happyShift action_201
action_162 (84#) = happyShift action_202
action_162 x = happyTcHack x happyFail

action_163 (114#) = happyShift action_74
action_163 (37#) = happyGoto action_91
action_163 (53#) = happyGoto action_200
action_163 x = happyTcHack x happyReduce_81

action_164 (85#) = happyShift action_199
action_164 x = happyTcHack x happyFail

action_165 (84#) = happyShift action_90
action_165 (52#) = happyGoto action_88
action_165 (54#) = happyGoto action_198
action_165 x = happyTcHack x happyReduce_84

action_166 (84#) = happyShift action_90
action_166 (52#) = happyGoto action_88
action_166 (54#) = happyGoto action_197
action_166 x = happyTcHack x happyReduce_84

action_167 x = happyTcHack x happyReduce_88

action_168 x = happyTcHack x happyReduce_91

action_169 x = happyTcHack x happyReduce_104

action_170 (85#) = happyShift action_196
action_170 x = happyTcHack x happyFail

action_171 (73#) = happyShift action_195
action_171 x = happyTcHack x happyFail

action_172 (72#) = happyShift action_72
action_172 (84#) = happyShift action_73
action_172 (112#) = happyShift action_33
action_172 (113#) = happyShift action_36
action_172 (114#) = happyShift action_74
action_172 (115#) = happyShift action_51
action_172 (116#) = happyShift action_75
action_172 (35#) = happyGoto action_63
action_172 (36#) = happyGoto action_64
action_172 (37#) = happyGoto action_65
action_172 (38#) = happyGoto action_66
action_172 (39#) = happyGoto action_67
action_172 (58#) = happyGoto action_194
action_172 (59#) = happyGoto action_69
action_172 (60#) = happyGoto action_70
action_172 x = happyTcHack x happyFail

action_173 (72#) = happyShift action_72
action_173 (84#) = happyShift action_73
action_173 (112#) = happyShift action_33
action_173 (113#) = happyShift action_36
action_173 (114#) = happyShift action_74
action_173 (115#) = happyShift action_51
action_173 (116#) = happyShift action_75
action_173 (35#) = happyGoto action_63
action_173 (36#) = happyGoto action_64
action_173 (37#) = happyGoto action_65
action_173 (38#) = happyGoto action_66
action_173 (39#) = happyGoto action_67
action_173 (58#) = happyGoto action_68
action_173 (59#) = happyGoto action_69
action_173 (60#) = happyGoto action_70
action_173 (62#) = happyGoto action_193
action_173 x = happyTcHack x happyReduce_105

action_174 x = happyTcHack x happyReduce_94

action_175 (112#) = happyShift action_33
action_175 (35#) = happyGoto action_61
action_175 (63#) = happyGoto action_192
action_175 x = happyTcHack x happyFail

action_176 (45#) = happyGoto action_57
action_176 (64#) = happyGoto action_191
action_176 (65#) = happyGoto action_60
action_176 x = happyTcHack x happyReduce_48

action_177 (74#) = happyShift action_179
action_177 (75#) = happyShift action_180
action_177 (83#) = happyShift action_181
action_177 x = happyTcHack x happyReduce_115

action_178 (72#) = happyShift action_42
action_178 (84#) = happyShift action_43
action_178 (87#) = happyShift action_44
action_178 (92#) = happyShift action_45
action_178 (94#) = happyShift action_46
action_178 (97#) = happyShift action_47
action_178 (98#) = happyShift action_48
action_178 (107#) = happyShift action_49
action_178 (109#) = happyShift action_50
action_178 (115#) = happyShift action_51
action_178 (38#) = happyGoto action_37
action_178 (67#) = happyGoto action_190
action_178 (69#) = happyGoto action_40
action_178 x = happyTcHack x happyFail

action_179 x = happyTcHack x happyReduce_120

action_180 x = happyTcHack x happyReduce_121

action_181 x = happyTcHack x happyReduce_122

action_182 (111#) = happyShift action_189
action_182 x = happyTcHack x happyFail

action_183 (85#) = happyShift action_188
action_183 x = happyTcHack x happyFail

action_184 (73#) = happyShift action_187
action_184 x = happyTcHack x happyFail

action_185 (72#) = happyShift action_42
action_185 (84#) = happyShift action_43
action_185 (87#) = happyShift action_44
action_185 (92#) = happyShift action_45
action_185 (94#) = happyShift action_46
action_185 (97#) = happyShift action_47
action_185 (98#) = happyShift action_48
action_185 (107#) = happyShift action_49
action_185 (109#) = happyShift action_50
action_185 (115#) = happyShift action_51
action_185 (38#) = happyGoto action_37
action_185 (67#) = happyGoto action_186
action_185 (69#) = happyGoto action_40
action_185 x = happyTcHack x happyFail

action_186 (72#) = happyShift action_42
action_186 (84#) = happyShift action_43
action_186 (87#) = happyShift action_44
action_186 (92#) = happyShift action_45
action_186 (94#) = happyShift action_46
action_186 (97#) = happyShift action_47
action_186 (98#) = happyShift action_48
action_186 (107#) = happyShift action_49
action_186 (109#) = happyShift action_50
action_186 (115#) = happyShift action_51
action_186 (38#) = happyGoto action_37
action_186 (69#) = happyGoto action_177
action_186 x = happyTcHack x happyReduce_118

action_187 x = happyTcHack x happyReduce_132

action_188 x = happyTcHack x happyReduce_125

action_189 x = happyTcHack x happyReduce_126

action_190 (72#) = happyShift action_42
action_190 (84#) = happyShift action_43
action_190 (87#) = happyShift action_44
action_190 (92#) = happyShift action_45
action_190 (94#) = happyShift action_46
action_190 (97#) = happyShift action_47
action_190 (98#) = happyShift action_48
action_190 (107#) = happyShift action_49
action_190 (109#) = happyShift action_50
action_190 (115#) = happyShift action_51
action_190 (38#) = happyGoto action_37
action_190 (69#) = happyGoto action_177
action_190 x = happyTcHack x happyReduce_117

action_191 x = happyTcHack x happyReduce_111

action_192 x = happyTcHack x happyReduce_109

action_193 x = happyTcHack x happyReduce_107

action_194 x = happyTcHack x happyReduce_92

action_195 x = happyTcHack x happyReduce_102

action_196 x = happyTcHack x happyReduce_101

action_197 (85#) = happyShift action_230
action_197 x = happyTcHack x happyFail

action_198 x = happyTcHack x happyReduce_86

action_199 x = happyTcHack x happyReduce_80

action_200 x = happyTcHack x happyReduce_83

action_201 x = happyTcHack x happyReduce_77

action_202 (85#) = happyShift action_229
action_202 x = happyTcHack x happyFail

action_203 x = happyTcHack x happyReduce_72

action_204 x = happyTcHack x happyReduce_68

action_205 (80#) = happyShift action_228
action_205 x = happyTcHack x happyFail

action_206 x = happyTcHack x happyReduce_60

action_207 x = happyTcHack x happyReduce_52

action_208 (82#) = happyShift action_227
action_208 (113#) = happyShift action_36
action_208 (36#) = happyGoto action_83
action_208 (56#) = happyGoto action_168
action_208 x = happyTcHack x happyFail

action_209 (112#) = happyShift action_33
action_209 (35#) = happyGoto action_226
action_209 x = happyTcHack x happyFail

action_210 (84#) = happyShift action_104
action_210 (113#) = happyShift action_36
action_210 (36#) = happyGoto action_102
action_210 (48#) = happyGoto action_225
action_210 x = happyTcHack x happyFail

action_211 x = happyTcHack x happyReduce_64

action_212 (72#) = happyShift action_42
action_212 (84#) = happyShift action_43
action_212 (87#) = happyShift action_44
action_212 (92#) = happyShift action_45
action_212 (94#) = happyShift action_46
action_212 (97#) = happyShift action_47
action_212 (98#) = happyShift action_48
action_212 (107#) = happyShift action_49
action_212 (109#) = happyShift action_50
action_212 (115#) = happyShift action_51
action_212 (38#) = happyGoto action_37
action_212 (67#) = happyGoto action_38
action_212 (68#) = happyGoto action_39
action_212 (69#) = happyGoto action_40
action_212 (70#) = happyGoto action_224
action_212 x = happyTcHack x happyFail

action_213 (45#) = happyGoto action_57
action_213 (64#) = happyGoto action_223
action_213 (65#) = happyGoto action_60
action_213 x = happyTcHack x happyReduce_48

action_214 (112#) = happyShift action_33
action_214 (35#) = happyGoto action_222
action_214 x = happyTcHack x happyFail

action_215 (112#) = happyShift action_33
action_215 (35#) = happyGoto action_221
action_215 x = happyTcHack x happyFail

action_216 x = happyTcHack x happyReduce_54

action_217 x = happyTcHack x happyReduce_47

action_218 x = happyTcHack x happyReduce_135

action_219 x = happyTcHack x happyReduce_43

action_220 x = happyTcHack x happyReduce_39

action_221 x = happyTcHack x happyReduce_58

action_222 x = happyTcHack x happyReduce_57

action_223 x = happyTcHack x happyReduce_61

action_224 x = happyTcHack x happyReduce_55

action_225 (80#) = happyShift action_235
action_225 x = happyTcHack x happyFail

action_226 x = happyTcHack x happyReduce_59

action_227 (72#) = happyShift action_72
action_227 (84#) = happyShift action_73
action_227 (112#) = happyShift action_33
action_227 (113#) = happyShift action_36
action_227 (114#) = happyShift action_74
action_227 (115#) = happyShift action_51
action_227 (116#) = happyShift action_75
action_227 (35#) = happyGoto action_63
action_227 (36#) = happyGoto action_64
action_227 (37#) = happyGoto action_65
action_227 (38#) = happyGoto action_66
action_227 (39#) = happyGoto action_67
action_227 (58#) = happyGoto action_234
action_227 (59#) = happyGoto action_69
action_227 (60#) = happyGoto action_70
action_227 x = happyTcHack x happyFail

action_228 (45#) = happyGoto action_233
action_228 x = happyTcHack x happyReduce_48

action_229 (73#) = happyShift action_232
action_229 x = happyTcHack x happyFail

action_230 (76#) = happyShift action_231
action_230 x = happyTcHack x happyFail

action_231 (84#) = happyShift action_237
action_231 x = happyTcHack x happyFail

action_232 x = happyTcHack x happyReduce_78

action_233 (84#) = happyShift action_104
action_233 (112#) = happyShift action_33
action_233 (113#) = happyShift action_36
action_233 (35#) = happyGoto action_105
action_233 (36#) = happyGoto action_102
action_233 (47#) = happyGoto action_141
action_233 (48#) = happyGoto action_107
action_233 x = happyTcHack x happyReduce_50

action_234 x = happyTcHack x happyReduce_62

action_235 (45#) = happyGoto action_236
action_235 x = happyTcHack x happyReduce_48

action_236 (84#) = happyShift action_104
action_236 (112#) = happyShift action_33
action_236 (113#) = happyShift action_36
action_236 (35#) = happyGoto action_105
action_236 (36#) = happyGoto action_102
action_236 (47#) = happyGoto action_141
action_236 (48#) = happyGoto action_107
action_236 x = happyTcHack x happyReduce_53

action_237 (114#) = happyShift action_74
action_237 (37#) = happyGoto action_91
action_237 (53#) = happyGoto action_238
action_237 x = happyTcHack x happyReduce_81

action_238 (85#) = happyShift action_239
action_238 x = happyTcHack x happyFail

action_239 (73#) = happyShift action_240
action_239 x = happyTcHack x happyFail

action_240 x = happyTcHack x happyReduce_79

happyReduce_32 = happySpecReduce_1  35# happyReduction_32
happyReduction_32 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  36# happyReduction_33
happyReduction_33 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn36
		 (Ident happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  37# happyReduction_34
happyReduction_34 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn37
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  38# happyReduction_35
happyReduction_35 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn38
		 ((read ( happy_var_1)) :: Char
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  39# happyReduction_36
happyReduction_36 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn39
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  40# happyReduction_37
happyReduction_37 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 (LGr happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  41# happyReduction_38
happyReduction_38 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn41
		 (DefAll happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  41# happyReduction_39
happyReduction_39 (HappyAbsSyn46  happy_var_3)
	_
	(HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn41
		 (DefSome happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  41# happyReduction_40
happyReduction_40 (HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (LDefView happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  42# happyReduction_41
happyReduction_41  =  HappyAbsSyn42
		 ([]
	)

happyReduce_42 = happySpecReduce_1  42# happyReduction_42
happyReduction_42 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn42
		 ((:[]) happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  42# happyReduction_43
happyReduction_43 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn42
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  43# happyReduction_44
happyReduction_44 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn43
		 (Grammar happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  44# happyReduction_45
happyReduction_45  =  HappyAbsSyn44
		 ([]
	)

happyReduce_46 = happySpecReduce_1  44# happyReduction_46
happyReduction_46 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn44
		 ((:[]) happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  44# happyReduction_47
happyReduction_47 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn44
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  45# happyReduction_48
happyReduction_48  =  HappyAbsSyn45
		 ([]
	)

happyReduce_49 = happySpecReduce_2  45# happyReduction_49
happyReduction_49 (HappyAbsSyn47  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happyReduce 5# 46# happyReduction_50
happyReduction_50 ((HappyAbsSyn45  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Rule happy_var_1 happy_var_3 (reverse happy_var_5)
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_2  46# happyReduction_51
happyReduction_51 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Comment happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  46# happyReduction_52
happyReduction_52 (HappyAbsSyn35  happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Comments happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 6# 46# happyReduction_53
happyReduction_53 ((HappyAbsSyn45  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Internal happy_var_2 happy_var_4 (reverse happy_var_6)
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_3  46# happyReduction_54
happyReduction_54 (HappyAbsSyn67  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Token happy_var_2 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happyReduce 4# 46# happyReduction_55
happyReduction_55 ((HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (PosToken happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_2  46# happyReduction_56
happyReduction_56 (HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Entryp happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happyReduce 4# 46# happyReduction_57
happyReduction_57 ((HappyAbsSyn35  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	(HappyAbsSyn66  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Separator happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_58 = happyReduce 4# 46# happyReduction_58
happyReduction_58 ((HappyAbsSyn35  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	(HappyAbsSyn66  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Terminator happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 4# 46# happyReduction_59
happyReduction_59 ((HappyAbsSyn35  happy_var_4) `HappyStk`
	(HappyAbsSyn35  happy_var_3) `HappyStk`
	(HappyAbsSyn48  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Delimiters happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_3  46# happyReduction_60
happyReduction_60 (HappyAbsSyn37  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Coercions happy_var_2 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 4# 46# happyReduction_61
happyReduction_61 ((HappyAbsSyn64  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Rules happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 5# 46# happyReduction_62
happyReduction_62 ((HappyAbsSyn58  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn57  happy_var_3) `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Function happy_var_2 (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_2  46# happyReduction_63
happyReduction_63 (HappyAbsSyn63  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Layout happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  46# happyReduction_64
happyReduction_64 (HappyAbsSyn63  happy_var_3)
	_
	_
	 =  HappyAbsSyn46
		 (LayoutStop happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  46# happyReduction_65
happyReduction_65 _
	_
	 =  HappyAbsSyn46
		 (LayoutTop
	)

happyReduce_66 = happySpecReduce_1  47# happyReduction_66
happyReduction_66 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn47
		 (Terminal happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  47# happyReduction_67
happyReduction_67 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn47
		 (NTerminal happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  48# happyReduction_68
happyReduction_68 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn48
		 (ListCat happy_var_2
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  48# happyReduction_69
happyReduction_69 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn48
		 (IdCat happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  49# happyReduction_70
happyReduction_70 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (LabNoP happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  49# happyReduction_71
happyReduction_71 (HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (LabP happy_var_1 happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  49# happyReduction_72
happyReduction_72 (HappyAbsSyn55  happy_var_3)
	(HappyAbsSyn50  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (LabPF happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_2  49# happyReduction_73
happyReduction_73 (HappyAbsSyn50  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (LabF happy_var_1 happy_var_2
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  50# happyReduction_74
happyReduction_74 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn50
		 (Id happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  50# happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn50
		 (Wild
	)

happyReduce_76 = happySpecReduce_2  50# happyReduction_76
happyReduction_76 _
	_
	 =  HappyAbsSyn50
		 (ListE
	)

happyReduce_77 = happySpecReduce_3  50# happyReduction_77
happyReduction_77 _
	_
	_
	 =  HappyAbsSyn50
		 (ListCons
	)

happyReduce_78 = happyReduce 5# 50# happyReduction_78
happyReduction_78 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn50
		 (ListOne
	) `HappyStk` happyRest

happyReduce_79 = happyReduce 9# 51# happyReduction_79
happyReduction_79 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn51
		 (ProfIt happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_80 = happySpecReduce_3  52# happyReduction_80
happyReduction_80 _
	(HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn52
		 (Ints happy_var_2
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_0  53# happyReduction_81
happyReduction_81  =  HappyAbsSyn53
		 ([]
	)

happyReduce_82 = happySpecReduce_1  53# happyReduction_82
happyReduction_82 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn53
		 ((:[]) happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  53# happyReduction_83
happyReduction_83 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn53
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_0  54# happyReduction_84
happyReduction_84  =  HappyAbsSyn54
		 ([]
	)

happyReduce_85 = happySpecReduce_1  54# happyReduction_85
happyReduction_85 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn54
		 ((:[]) happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  54# happyReduction_86
happyReduction_86 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn54
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  55# happyReduction_87
happyReduction_87 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn55
		 ((:[]) happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_2  55# happyReduction_88
happyReduction_88 (HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn55
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_88 _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  56# happyReduction_89
happyReduction_89 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn56
		 (Arg happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_0  57# happyReduction_90
happyReduction_90  =  HappyAbsSyn57
		 ([]
	)

happyReduce_91 = happySpecReduce_2  57# happyReduction_91
happyReduction_91 (HappyAbsSyn56  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_91 _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  58# happyReduction_92
happyReduction_92 (HappyAbsSyn58  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (Cons happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  58# happyReduction_93
happyReduction_93 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_2  59# happyReduction_94
happyReduction_94 (HappyAbsSyn61  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn58
		 (App happy_var_1 happy_var_2
	)
happyReduction_94 _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  59# happyReduction_95
happyReduction_95 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  60# happyReduction_96
happyReduction_96 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn58
		 (Var happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  60# happyReduction_97
happyReduction_97 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn58
		 (LitInt happy_var_1
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  60# happyReduction_98
happyReduction_98 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn58
		 (LitChar happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  60# happyReduction_99
happyReduction_99 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn58
		 (LitString happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  60# happyReduction_100
happyReduction_100 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn58
		 (LitDouble happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  60# happyReduction_101
happyReduction_101 _
	(HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn58
		 (List happy_var_2
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_3  60# happyReduction_102
happyReduction_102 _
	(HappyAbsSyn58  happy_var_2)
	_
	 =  HappyAbsSyn58
		 (happy_var_2
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  61# happyReduction_103
happyReduction_103 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 ((:[]) happy_var_1
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_2  61# happyReduction_104
happyReduction_104 (HappyAbsSyn61  happy_var_2)
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_104 _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_0  62# happyReduction_105
happyReduction_105  =  HappyAbsSyn61
		 ([]
	)

happyReduce_106 = happySpecReduce_1  62# happyReduction_106
happyReduction_106 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 ((:[]) happy_var_1
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  62# happyReduction_107
happyReduction_107 (HappyAbsSyn61  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  63# happyReduction_108
happyReduction_108 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn63
		 ((:[]) happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  63# happyReduction_109
happyReduction_109 (HappyAbsSyn63  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn63
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  64# happyReduction_110
happyReduction_110 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn64
		 ((:[]) happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_3  64# happyReduction_111
happyReduction_111 (HappyAbsSyn64  happy_var_3)
	_
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn64
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_111 _ _ _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  65# happyReduction_112
happyReduction_112 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn65
		 (RHS (reverse happy_var_1)
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  66# happyReduction_113
happyReduction_113 _
	 =  HappyAbsSyn66
		 (MNonempty
	)

happyReduce_114 = happySpecReduce_0  66# happyReduction_114
happyReduction_114  =  HappyAbsSyn66
		 (MEmpty
	)

happyReduce_115 = happySpecReduce_2  67# happyReduction_115
happyReduction_115 (HappyAbsSyn67  happy_var_2)
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RSeq happy_var_1 happy_var_2
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  67# happyReduction_116
happyReduction_116 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_3  68# happyReduction_117
happyReduction_117 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RAlt happy_var_1 happy_var_3
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_3  68# happyReduction_118
happyReduction_118 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RMinus happy_var_1 happy_var_3
	)
happyReduction_118 _ _ _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  68# happyReduction_119
happyReduction_119 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_2  69# happyReduction_120
happyReduction_120 _
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RStar happy_var_1
	)
happyReduction_120 _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_2  69# happyReduction_121
happyReduction_121 _
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RPlus happy_var_1
	)
happyReduction_121 _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_2  69# happyReduction_122
happyReduction_122 _
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (ROpt happy_var_1
	)
happyReduction_122 _ _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  69# happyReduction_123
happyReduction_123 _
	 =  HappyAbsSyn67
		 (REps
	)

happyReduce_124 = happySpecReduce_1  69# happyReduction_124
happyReduction_124 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn67
		 (RChar happy_var_1
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_3  69# happyReduction_125
happyReduction_125 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (RAlts happy_var_2
	)
happyReduction_125 _ _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_3  69# happyReduction_126
happyReduction_126 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (RSeqs happy_var_2
	)
happyReduction_126 _ _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  69# happyReduction_127
happyReduction_127 _
	 =  HappyAbsSyn67
		 (RDigit
	)

happyReduce_128 = happySpecReduce_1  69# happyReduction_128
happyReduction_128 _
	 =  HappyAbsSyn67
		 (RLetter
	)

happyReduce_129 = happySpecReduce_1  69# happyReduction_129
happyReduction_129 _
	 =  HappyAbsSyn67
		 (RUpper
	)

happyReduce_130 = happySpecReduce_1  69# happyReduction_130
happyReduction_130 _
	 =  HappyAbsSyn67
		 (RLower
	)

happyReduce_131 = happySpecReduce_1  69# happyReduction_131
happyReduction_131 _
	 =  HappyAbsSyn67
		 (RAny
	)

happyReduce_132 = happySpecReduce_3  69# happyReduction_132
happyReduction_132 _
	(HappyAbsSyn67  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (happy_var_2
	)
happyReduction_132 _ _ _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_1  70# happyReduction_133
happyReduction_133 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_1  71# happyReduction_134
happyReduction_134 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn71
		 ((:[]) happy_var_1
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_3  71# happyReduction_135
happyReduction_135 (HappyAbsSyn71  happy_var_3)
	_
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn71
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_135 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 118# 118# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 72#;
	PT _ (TS _ 2) -> cont 73#;
	PT _ (TS _ 3) -> cont 74#;
	PT _ (TS _ 4) -> cont 75#;
	PT _ (TS _ 5) -> cont 76#;
	PT _ (TS _ 6) -> cont 77#;
	PT _ (TS _ 7) -> cont 78#;
	PT _ (TS _ 8) -> cont 79#;
	PT _ (TS _ 9) -> cont 80#;
	PT _ (TS _ 10) -> cont 81#;
	PT _ (TS _ 11) -> cont 82#;
	PT _ (TS _ 12) -> cont 83#;
	PT _ (TS _ 13) -> cont 84#;
	PT _ (TS _ 14) -> cont 85#;
	PT _ (TS _ 15) -> cont 86#;
	PT _ (TS _ 16) -> cont 87#;
	PT _ (TS _ 17) -> cont 88#;
	PT _ (TS _ 18) -> cont 89#;
	PT _ (TS _ 19) -> cont 90#;
	PT _ (TS _ 20) -> cont 91#;
	PT _ (TS _ 21) -> cont 92#;
	PT _ (TS _ 22) -> cont 93#;
	PT _ (TS _ 23) -> cont 94#;
	PT _ (TS _ 24) -> cont 95#;
	PT _ (TS _ 25) -> cont 96#;
	PT _ (TS _ 26) -> cont 97#;
	PT _ (TS _ 27) -> cont 98#;
	PT _ (TS _ 28) -> cont 99#;
	PT _ (TS _ 29) -> cont 100#;
	PT _ (TS _ 30) -> cont 101#;
	PT _ (TS _ 31) -> cont 102#;
	PT _ (TS _ 32) -> cont 103#;
	PT _ (TS _ 33) -> cont 104#;
	PT _ (TS _ 34) -> cont 105#;
	PT _ (TS _ 35) -> cont 106#;
	PT _ (TS _ 36) -> cont 107#;
	PT _ (TS _ 37) -> cont 108#;
	PT _ (TS _ 38) -> cont 109#;
	PT _ (TS _ 39) -> cont 110#;
	PT _ (TS _ 40) -> cont 111#;
	PT _ (TL happy_dollar_dollar) -> cont 112#;
	PT _ (TV happy_dollar_dollar) -> cont 113#;
	PT _ (TI happy_dollar_dollar) -> cont 114#;
	PT _ (TC happy_dollar_dollar) -> cont 115#;
	PT _ (TD happy_dollar_dollar) -> cont 116#;
	_ -> cont 117#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pLGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn40 z -> happyReturn z; _other -> notHappyAtAll })

pLDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pListLDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

pGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

pListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn44 z -> happyReturn z; _other -> notHappyAtAll })

pListItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn45 z -> happyReturn z; _other -> notHappyAtAll })

pDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn46 z -> happyReturn z; _other -> notHappyAtAll })

pItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn47 z -> happyReturn z; _other -> notHappyAtAll })

pCat tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn48 z -> happyReturn z; _other -> notHappyAtAll })

pLabel tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn49 z -> happyReturn z; _other -> notHappyAtAll })

pLabelId tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn50 z -> happyReturn z; _other -> notHappyAtAll })

pProfItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn51 z -> happyReturn z; _other -> notHappyAtAll })

pIntList tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn52 z -> happyReturn z; _other -> notHappyAtAll })

pListInteger tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn53 z -> happyReturn z; _other -> notHappyAtAll })

pListIntList tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_14 tks) (\x -> case x of {HappyAbsSyn54 z -> happyReturn z; _other -> notHappyAtAll })

pListProfItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_15 tks) (\x -> case x of {HappyAbsSyn55 z -> happyReturn z; _other -> notHappyAtAll })

pArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_16 tks) (\x -> case x of {HappyAbsSyn56 z -> happyReturn z; _other -> notHappyAtAll })

pListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_17 tks) (\x -> case x of {HappyAbsSyn57 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_18 tks) (\x -> case x of {HappyAbsSyn58 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_19 tks) (\x -> case x of {HappyAbsSyn58 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_20 tks) (\x -> case x of {HappyAbsSyn58 z -> happyReturn z; _other -> notHappyAtAll })

pListExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_21 tks) (\x -> case x of {HappyAbsSyn61 z -> happyReturn z; _other -> notHappyAtAll })

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_22 tks) (\x -> case x of {HappyAbsSyn61 z -> happyReturn z; _other -> notHappyAtAll })

pListString tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_23 tks) (\x -> case x of {HappyAbsSyn63 z -> happyReturn z; _other -> notHappyAtAll })

pListRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_24 tks) (\x -> case x of {HappyAbsSyn64 z -> happyReturn z; _other -> notHappyAtAll })

pRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_25 tks) (\x -> case x of {HappyAbsSyn65 z -> happyReturn z; _other -> notHappyAtAll })

pMinimumSize tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_26 tks) (\x -> case x of {HappyAbsSyn66 z -> happyReturn z; _other -> notHappyAtAll })

pReg2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_27 tks) (\x -> case x of {HappyAbsSyn67 z -> happyReturn z; _other -> notHappyAtAll })

pReg1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_28 tks) (\x -> case x of {HappyAbsSyn67 z -> happyReturn z; _other -> notHappyAtAll })

pReg3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_29 tks) (\x -> case x of {HappyAbsSyn67 z -> happyReturn z; _other -> notHappyAtAll })

pReg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_30 tks) (\x -> case x of {HappyAbsSyn67 z -> happyReturn z; _other -> notHappyAtAll })

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_31 tks) (\x -> case x of {HappyAbsSyn71 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
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

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  1# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
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
