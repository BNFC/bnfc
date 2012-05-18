{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParBNF where
import AbsBNF
import LexBNF
import ErrM

-- parser produced by Happy Version 1.18.9

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
 action_236 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
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
 happyReduce_134 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (72) = happyShift action_97
action_0 (84) = happyShift action_98
action_0 (86) = happyShift action_99
action_0 (88) = happyShift action_110
action_0 (89) = happyShift action_111
action_0 (90) = happyShift action_112
action_0 (92) = happyShift action_113
action_0 (94) = happyShift action_114
action_0 (95) = happyShift action_115
action_0 (99) = happyShift action_116
action_0 (100) = happyShift action_117
action_0 (101) = happyShift action_118
action_0 (103) = happyShift action_119
action_0 (104) = happyShift action_120
action_0 (107) = happyShift action_131
action_0 (112) = happyShift action_36
action_0 (36) = happyGoto action_126
action_0 (40) = happyGoto action_133
action_0 (41) = happyGoto action_127
action_0 (42) = happyGoto action_134
action_0 (46) = happyGoto action_129
action_0 (49) = happyGoto action_109
action_0 (50) = happyGoto action_101
action_0 (71) = happyGoto action_130
action_0 _ = happyReduce_41

action_1 (72) = happyShift action_97
action_1 (84) = happyShift action_98
action_1 (86) = happyShift action_99
action_1 (88) = happyShift action_110
action_1 (89) = happyShift action_111
action_1 (90) = happyShift action_112
action_1 (92) = happyShift action_113
action_1 (94) = happyShift action_114
action_1 (95) = happyShift action_115
action_1 (99) = happyShift action_116
action_1 (100) = happyShift action_117
action_1 (101) = happyShift action_118
action_1 (103) = happyShift action_119
action_1 (104) = happyShift action_120
action_1 (107) = happyShift action_131
action_1 (112) = happyShift action_36
action_1 (36) = happyGoto action_126
action_1 (41) = happyGoto action_132
action_1 (46) = happyGoto action_129
action_1 (49) = happyGoto action_109
action_1 (50) = happyGoto action_101
action_1 (71) = happyGoto action_130
action_1 _ = happyFail

action_2 (72) = happyShift action_97
action_2 (84) = happyShift action_98
action_2 (86) = happyShift action_99
action_2 (88) = happyShift action_110
action_2 (89) = happyShift action_111
action_2 (90) = happyShift action_112
action_2 (92) = happyShift action_113
action_2 (94) = happyShift action_114
action_2 (95) = happyShift action_115
action_2 (99) = happyShift action_116
action_2 (100) = happyShift action_117
action_2 (101) = happyShift action_118
action_2 (103) = happyShift action_119
action_2 (104) = happyShift action_120
action_2 (107) = happyShift action_131
action_2 (112) = happyShift action_36
action_2 (36) = happyGoto action_126
action_2 (41) = happyGoto action_127
action_2 (42) = happyGoto action_128
action_2 (46) = happyGoto action_129
action_2 (49) = happyGoto action_109
action_2 (50) = happyGoto action_101
action_2 (71) = happyGoto action_130
action_2 _ = happyReduce_41

action_3 (72) = happyShift action_97
action_3 (84) = happyShift action_98
action_3 (86) = happyShift action_99
action_3 (88) = happyShift action_110
action_3 (89) = happyShift action_111
action_3 (90) = happyShift action_112
action_3 (92) = happyShift action_113
action_3 (94) = happyShift action_114
action_3 (95) = happyShift action_115
action_3 (99) = happyShift action_116
action_3 (100) = happyShift action_117
action_3 (101) = happyShift action_118
action_3 (103) = happyShift action_119
action_3 (104) = happyShift action_120
action_3 (112) = happyShift action_36
action_3 (36) = happyGoto action_95
action_3 (43) = happyGoto action_124
action_3 (44) = happyGoto action_125
action_3 (46) = happyGoto action_123
action_3 (49) = happyGoto action_109
action_3 (50) = happyGoto action_101
action_3 _ = happyReduce_45

action_4 (72) = happyShift action_97
action_4 (84) = happyShift action_98
action_4 (86) = happyShift action_99
action_4 (88) = happyShift action_110
action_4 (89) = happyShift action_111
action_4 (90) = happyShift action_112
action_4 (92) = happyShift action_113
action_4 (94) = happyShift action_114
action_4 (95) = happyShift action_115
action_4 (99) = happyShift action_116
action_4 (100) = happyShift action_117
action_4 (101) = happyShift action_118
action_4 (103) = happyShift action_119
action_4 (104) = happyShift action_120
action_4 (112) = happyShift action_36
action_4 (36) = happyGoto action_95
action_4 (44) = happyGoto action_122
action_4 (46) = happyGoto action_123
action_4 (49) = happyGoto action_109
action_4 (50) = happyGoto action_101
action_4 _ = happyReduce_45

action_5 (45) = happyGoto action_121
action_5 _ = happyReduce_48

action_6 (72) = happyShift action_97
action_6 (84) = happyShift action_98
action_6 (86) = happyShift action_99
action_6 (88) = happyShift action_110
action_6 (89) = happyShift action_111
action_6 (90) = happyShift action_112
action_6 (92) = happyShift action_113
action_6 (94) = happyShift action_114
action_6 (95) = happyShift action_115
action_6 (99) = happyShift action_116
action_6 (100) = happyShift action_117
action_6 (101) = happyShift action_118
action_6 (103) = happyShift action_119
action_6 (104) = happyShift action_120
action_6 (112) = happyShift action_36
action_6 (36) = happyGoto action_95
action_6 (46) = happyGoto action_108
action_6 (49) = happyGoto action_109
action_6 (50) = happyGoto action_101
action_6 _ = happyFail

action_7 (84) = happyShift action_104
action_7 (111) = happyShift action_33
action_7 (112) = happyShift action_36
action_7 (35) = happyGoto action_105
action_7 (36) = happyGoto action_102
action_7 (47) = happyGoto action_106
action_7 (48) = happyGoto action_107
action_7 _ = happyFail

action_8 (84) = happyShift action_104
action_8 (112) = happyShift action_36
action_8 (36) = happyGoto action_102
action_8 (48) = happyGoto action_103
action_8 _ = happyFail

action_9 (72) = happyShift action_97
action_9 (84) = happyShift action_98
action_9 (86) = happyShift action_99
action_9 (112) = happyShift action_36
action_9 (36) = happyGoto action_95
action_9 (49) = happyGoto action_100
action_9 (50) = happyGoto action_101
action_9 _ = happyFail

action_10 (72) = happyShift action_97
action_10 (84) = happyShift action_98
action_10 (86) = happyShift action_99
action_10 (112) = happyShift action_36
action_10 (36) = happyGoto action_95
action_10 (50) = happyGoto action_96
action_10 _ = happyFail

action_11 (72) = happyShift action_87
action_11 (51) = happyGoto action_94
action_11 _ = happyFail

action_12 (84) = happyShift action_90
action_12 (52) = happyGoto action_93
action_12 _ = happyFail

action_13 (113) = happyShift action_74
action_13 (37) = happyGoto action_91
action_13 (53) = happyGoto action_92
action_13 _ = happyReduce_80

action_14 (84) = happyShift action_90
action_14 (52) = happyGoto action_88
action_14 (54) = happyGoto action_89
action_14 _ = happyReduce_83

action_15 (72) = happyShift action_87
action_15 (51) = happyGoto action_85
action_15 (55) = happyGoto action_86
action_15 _ = happyFail

action_16 (112) = happyShift action_36
action_16 (36) = happyGoto action_83
action_16 (56) = happyGoto action_84
action_16 _ = happyFail

action_17 (57) = happyGoto action_82
action_17 _ = happyReduce_89

action_18 (72) = happyShift action_72
action_18 (84) = happyShift action_73
action_18 (111) = happyShift action_33
action_18 (112) = happyShift action_36
action_18 (113) = happyShift action_74
action_18 (114) = happyShift action_51
action_18 (115) = happyShift action_75
action_18 (35) = happyGoto action_63
action_18 (36) = happyGoto action_64
action_18 (37) = happyGoto action_65
action_18 (38) = happyGoto action_66
action_18 (39) = happyGoto action_67
action_18 (58) = happyGoto action_81
action_18 (59) = happyGoto action_69
action_18 (60) = happyGoto action_70
action_18 _ = happyFail

action_19 (72) = happyShift action_72
action_19 (84) = happyShift action_73
action_19 (111) = happyShift action_33
action_19 (112) = happyShift action_36
action_19 (113) = happyShift action_74
action_19 (114) = happyShift action_51
action_19 (115) = happyShift action_75
action_19 (35) = happyGoto action_63
action_19 (36) = happyGoto action_64
action_19 (37) = happyGoto action_65
action_19 (38) = happyGoto action_66
action_19 (39) = happyGoto action_67
action_19 (59) = happyGoto action_80
action_19 (60) = happyGoto action_70
action_19 _ = happyFail

action_20 (72) = happyShift action_72
action_20 (84) = happyShift action_73
action_20 (111) = happyShift action_33
action_20 (112) = happyShift action_36
action_20 (113) = happyShift action_74
action_20 (114) = happyShift action_51
action_20 (115) = happyShift action_75
action_20 (35) = happyGoto action_63
action_20 (36) = happyGoto action_76
action_20 (37) = happyGoto action_65
action_20 (38) = happyGoto action_66
action_20 (39) = happyGoto action_67
action_20 (60) = happyGoto action_79
action_20 _ = happyFail

action_21 (72) = happyShift action_72
action_21 (84) = happyShift action_73
action_21 (111) = happyShift action_33
action_21 (112) = happyShift action_36
action_21 (113) = happyShift action_74
action_21 (114) = happyShift action_51
action_21 (115) = happyShift action_75
action_21 (35) = happyGoto action_63
action_21 (36) = happyGoto action_76
action_21 (37) = happyGoto action_65
action_21 (38) = happyGoto action_66
action_21 (39) = happyGoto action_67
action_21 (60) = happyGoto action_77
action_21 (61) = happyGoto action_78
action_21 _ = happyFail

action_22 (72) = happyShift action_72
action_22 (84) = happyShift action_73
action_22 (111) = happyShift action_33
action_22 (112) = happyShift action_36
action_22 (113) = happyShift action_74
action_22 (114) = happyShift action_51
action_22 (115) = happyShift action_75
action_22 (35) = happyGoto action_63
action_22 (36) = happyGoto action_64
action_22 (37) = happyGoto action_65
action_22 (38) = happyGoto action_66
action_22 (39) = happyGoto action_67
action_22 (58) = happyGoto action_68
action_22 (59) = happyGoto action_69
action_22 (60) = happyGoto action_70
action_22 (62) = happyGoto action_71
action_22 _ = happyReduce_104

action_23 (111) = happyShift action_33
action_23 (35) = happyGoto action_61
action_23 (63) = happyGoto action_62
action_23 _ = happyFail

action_24 (45) = happyGoto action_57
action_24 (64) = happyGoto action_59
action_24 (65) = happyGoto action_60
action_24 _ = happyReduce_48

action_25 (45) = happyGoto action_57
action_25 (65) = happyGoto action_58
action_25 _ = happyReduce_48

action_26 (98) = happyShift action_56
action_26 (66) = happyGoto action_55
action_26 _ = happyReduce_113

action_27 (72) = happyShift action_42
action_27 (84) = happyShift action_43
action_27 (87) = happyShift action_44
action_27 (91) = happyShift action_45
action_27 (93) = happyShift action_46
action_27 (96) = happyShift action_47
action_27 (97) = happyShift action_48
action_27 (106) = happyShift action_49
action_27 (108) = happyShift action_50
action_27 (114) = happyShift action_51
action_27 (38) = happyGoto action_37
action_27 (67) = happyGoto action_54
action_27 (69) = happyGoto action_40
action_27 _ = happyFail

action_28 (72) = happyShift action_42
action_28 (84) = happyShift action_43
action_28 (87) = happyShift action_44
action_28 (91) = happyShift action_45
action_28 (93) = happyShift action_46
action_28 (96) = happyShift action_47
action_28 (97) = happyShift action_48
action_28 (106) = happyShift action_49
action_28 (108) = happyShift action_50
action_28 (114) = happyShift action_51
action_28 (38) = happyGoto action_37
action_28 (67) = happyGoto action_38
action_28 (68) = happyGoto action_53
action_28 (69) = happyGoto action_40
action_28 _ = happyFail

action_29 (72) = happyShift action_42
action_29 (84) = happyShift action_43
action_29 (87) = happyShift action_44
action_29 (91) = happyShift action_45
action_29 (93) = happyShift action_46
action_29 (96) = happyShift action_47
action_29 (97) = happyShift action_48
action_29 (106) = happyShift action_49
action_29 (108) = happyShift action_50
action_29 (114) = happyShift action_51
action_29 (38) = happyGoto action_37
action_29 (69) = happyGoto action_52
action_29 _ = happyFail

action_30 (72) = happyShift action_42
action_30 (84) = happyShift action_43
action_30 (87) = happyShift action_44
action_30 (91) = happyShift action_45
action_30 (93) = happyShift action_46
action_30 (96) = happyShift action_47
action_30 (97) = happyShift action_48
action_30 (106) = happyShift action_49
action_30 (108) = happyShift action_50
action_30 (114) = happyShift action_51
action_30 (38) = happyGoto action_37
action_30 (67) = happyGoto action_38
action_30 (68) = happyGoto action_39
action_30 (69) = happyGoto action_40
action_30 (70) = happyGoto action_41
action_30 _ = happyFail

action_31 (112) = happyShift action_36
action_31 (36) = happyGoto action_34
action_31 (71) = happyGoto action_35
action_31 _ = happyFail

action_32 (111) = happyShift action_33
action_32 _ = happyFail

action_33 _ = happyReduce_32

action_34 (76) = happyShift action_138
action_34 _ = happyReduce_133

action_35 (117) = happyAccept
action_35 _ = happyFail

action_36 _ = happyReduce_33

action_37 _ = happyReduce_123

action_38 (72) = happyShift action_42
action_38 (77) = happyShift action_183
action_38 (84) = happyShift action_43
action_38 (87) = happyShift action_44
action_38 (91) = happyShift action_45
action_38 (93) = happyShift action_46
action_38 (96) = happyShift action_47
action_38 (97) = happyShift action_48
action_38 (106) = happyShift action_49
action_38 (108) = happyShift action_50
action_38 (114) = happyShift action_51
action_38 (38) = happyGoto action_37
action_38 (69) = happyGoto action_175
action_38 _ = happyReduce_118

action_39 (109) = happyShift action_176
action_39 _ = happyReduce_132

action_40 (74) = happyShift action_177
action_40 (75) = happyShift action_178
action_40 (83) = happyShift action_179
action_40 _ = happyReduce_115

action_41 (117) = happyAccept
action_41 _ = happyFail

action_42 (72) = happyShift action_42
action_42 (84) = happyShift action_43
action_42 (87) = happyShift action_44
action_42 (91) = happyShift action_45
action_42 (93) = happyShift action_46
action_42 (96) = happyShift action_47
action_42 (97) = happyShift action_48
action_42 (106) = happyShift action_49
action_42 (108) = happyShift action_50
action_42 (114) = happyShift action_51
action_42 (38) = happyGoto action_37
action_42 (67) = happyGoto action_38
action_42 (68) = happyGoto action_39
action_42 (69) = happyGoto action_40
action_42 (70) = happyGoto action_182
action_42 _ = happyFail

action_43 (111) = happyShift action_33
action_43 (35) = happyGoto action_181
action_43 _ = happyFail

action_44 _ = happyReduce_130

action_45 _ = happyReduce_126

action_46 _ = happyReduce_122

action_47 _ = happyReduce_127

action_48 _ = happyReduce_129

action_49 _ = happyReduce_128

action_50 (111) = happyShift action_33
action_50 (35) = happyGoto action_180
action_50 _ = happyFail

action_51 _ = happyReduce_35

action_52 (74) = happyShift action_177
action_52 (75) = happyShift action_178
action_52 (83) = happyShift action_179
action_52 (117) = happyAccept
action_52 _ = happyFail

action_53 (109) = happyShift action_176
action_53 (117) = happyAccept
action_53 _ = happyFail

action_54 (72) = happyShift action_42
action_54 (84) = happyShift action_43
action_54 (87) = happyShift action_44
action_54 (91) = happyShift action_45
action_54 (93) = happyShift action_46
action_54 (96) = happyShift action_47
action_54 (97) = happyShift action_48
action_54 (106) = happyShift action_49
action_54 (108) = happyShift action_50
action_54 (114) = happyShift action_51
action_54 (117) = happyAccept
action_54 (38) = happyGoto action_37
action_54 (69) = happyGoto action_175
action_54 _ = happyFail

action_55 (117) = happyAccept
action_55 _ = happyFail

action_56 _ = happyReduce_112

action_57 (84) = happyShift action_104
action_57 (111) = happyShift action_33
action_57 (112) = happyShift action_36
action_57 (35) = happyGoto action_105
action_57 (36) = happyGoto action_102
action_57 (47) = happyGoto action_140
action_57 (48) = happyGoto action_107
action_57 _ = happyReduce_111

action_58 (117) = happyAccept
action_58 _ = happyFail

action_59 (117) = happyAccept
action_59 _ = happyFail

action_60 (109) = happyShift action_174
action_60 _ = happyReduce_109

action_61 (76) = happyShift action_173
action_61 _ = happyReduce_107

action_62 (117) = happyAccept
action_62 _ = happyFail

action_63 _ = happyReduce_98

action_64 (72) = happyShift action_72
action_64 (84) = happyShift action_73
action_64 (111) = happyShift action_33
action_64 (112) = happyShift action_36
action_64 (113) = happyShift action_74
action_64 (114) = happyShift action_51
action_64 (115) = happyShift action_75
action_64 (35) = happyGoto action_63
action_64 (36) = happyGoto action_76
action_64 (37) = happyGoto action_65
action_64 (38) = happyGoto action_66
action_64 (39) = happyGoto action_67
action_64 (60) = happyGoto action_77
action_64 (61) = happyGoto action_172
action_64 _ = happyReduce_95

action_65 _ = happyReduce_96

action_66 _ = happyReduce_97

action_67 _ = happyReduce_99

action_68 (76) = happyShift action_171
action_68 _ = happyReduce_105

action_69 (79) = happyShift action_170
action_69 _ = happyReduce_92

action_70 _ = happyReduce_94

action_71 (117) = happyAccept
action_71 _ = happyFail

action_72 (72) = happyShift action_72
action_72 (84) = happyShift action_73
action_72 (111) = happyShift action_33
action_72 (112) = happyShift action_36
action_72 (113) = happyShift action_74
action_72 (114) = happyShift action_51
action_72 (115) = happyShift action_75
action_72 (35) = happyGoto action_63
action_72 (36) = happyGoto action_64
action_72 (37) = happyGoto action_65
action_72 (38) = happyGoto action_66
action_72 (39) = happyGoto action_67
action_72 (58) = happyGoto action_169
action_72 (59) = happyGoto action_69
action_72 (60) = happyGoto action_70
action_72 _ = happyFail

action_73 (72) = happyShift action_72
action_73 (84) = happyShift action_73
action_73 (111) = happyShift action_33
action_73 (112) = happyShift action_36
action_73 (113) = happyShift action_74
action_73 (114) = happyShift action_51
action_73 (115) = happyShift action_75
action_73 (35) = happyGoto action_63
action_73 (36) = happyGoto action_64
action_73 (37) = happyGoto action_65
action_73 (38) = happyGoto action_66
action_73 (39) = happyGoto action_67
action_73 (58) = happyGoto action_68
action_73 (59) = happyGoto action_69
action_73 (60) = happyGoto action_70
action_73 (62) = happyGoto action_168
action_73 _ = happyReduce_104

action_74 _ = happyReduce_34

action_75 _ = happyReduce_36

action_76 _ = happyReduce_95

action_77 (72) = happyShift action_72
action_77 (84) = happyShift action_73
action_77 (111) = happyShift action_33
action_77 (112) = happyShift action_36
action_77 (113) = happyShift action_74
action_77 (114) = happyShift action_51
action_77 (115) = happyShift action_75
action_77 (35) = happyGoto action_63
action_77 (36) = happyGoto action_76
action_77 (37) = happyGoto action_65
action_77 (38) = happyGoto action_66
action_77 (39) = happyGoto action_67
action_77 (60) = happyGoto action_77
action_77 (61) = happyGoto action_167
action_77 _ = happyReduce_102

action_78 (117) = happyAccept
action_78 _ = happyFail

action_79 (117) = happyAccept
action_79 _ = happyFail

action_80 (117) = happyAccept
action_80 _ = happyFail

action_81 (117) = happyAccept
action_81 _ = happyFail

action_82 (112) = happyShift action_36
action_82 (117) = happyAccept
action_82 (36) = happyGoto action_83
action_82 (56) = happyGoto action_166
action_82 _ = happyFail

action_83 _ = happyReduce_88

action_84 (117) = happyAccept
action_84 _ = happyFail

action_85 (72) = happyShift action_87
action_85 (51) = happyGoto action_85
action_85 (55) = happyGoto action_165
action_85 _ = happyReduce_86

action_86 (117) = happyAccept
action_86 _ = happyFail

action_87 (84) = happyShift action_164
action_87 _ = happyFail

action_88 (76) = happyShift action_163
action_88 _ = happyReduce_84

action_89 (117) = happyAccept
action_89 _ = happyFail

action_90 (113) = happyShift action_74
action_90 (37) = happyGoto action_91
action_90 (53) = happyGoto action_162
action_90 _ = happyReduce_80

action_91 (76) = happyShift action_161
action_91 _ = happyReduce_81

action_92 (117) = happyAccept
action_92 _ = happyFail

action_93 (117) = happyAccept
action_93 _ = happyFail

action_94 (117) = happyAccept
action_94 _ = happyFail

action_95 _ = happyReduce_73

action_96 (117) = happyAccept
action_96 _ = happyFail

action_97 (79) = happyShift action_160
action_97 _ = happyFail

action_98 (85) = happyShift action_159
action_98 _ = happyFail

action_99 _ = happyReduce_74

action_100 (117) = happyAccept
action_100 _ = happyFail

action_101 (72) = happyShift action_158
action_101 (84) = happyShift action_98
action_101 (86) = happyShift action_99
action_101 (112) = happyShift action_36
action_101 (36) = happyGoto action_95
action_101 (50) = happyGoto action_156
action_101 (51) = happyGoto action_85
action_101 (55) = happyGoto action_157
action_101 _ = happyReduce_69

action_102 _ = happyReduce_68

action_103 (117) = happyAccept
action_103 _ = happyFail

action_104 (84) = happyShift action_104
action_104 (112) = happyShift action_36
action_104 (36) = happyGoto action_102
action_104 (48) = happyGoto action_155
action_104 _ = happyFail

action_105 _ = happyReduce_65

action_106 (117) = happyAccept
action_106 _ = happyFail

action_107 _ = happyReduce_66

action_108 (117) = happyAccept
action_108 _ = happyFail

action_109 (78) = happyShift action_154
action_109 _ = happyFail

action_110 (112) = happyShift action_36
action_110 (36) = happyGoto action_153
action_110 _ = happyFail

action_111 (111) = happyShift action_33
action_111 (35) = happyGoto action_152
action_111 _ = happyFail

action_112 (112) = happyShift action_36
action_112 (36) = happyGoto action_151
action_112 _ = happyFail

action_113 (112) = happyShift action_36
action_113 (36) = happyGoto action_34
action_113 (71) = happyGoto action_150
action_113 _ = happyFail

action_114 (72) = happyShift action_97
action_114 (84) = happyShift action_98
action_114 (86) = happyShift action_99
action_114 (112) = happyShift action_36
action_114 (36) = happyGoto action_95
action_114 (49) = happyGoto action_149
action_114 (50) = happyGoto action_101
action_114 _ = happyFail

action_115 (102) = happyShift action_147
action_115 (105) = happyShift action_148
action_115 (111) = happyShift action_33
action_115 (35) = happyGoto action_61
action_115 (63) = happyGoto action_146
action_115 _ = happyFail

action_116 (104) = happyShift action_145
action_116 _ = happyFail

action_117 (112) = happyShift action_36
action_117 (36) = happyGoto action_144
action_117 _ = happyFail

action_118 (98) = happyShift action_56
action_118 (66) = happyGoto action_143
action_118 _ = happyReduce_113

action_119 (98) = happyShift action_56
action_119 (66) = happyGoto action_142
action_119 _ = happyReduce_113

action_120 (112) = happyShift action_36
action_120 (36) = happyGoto action_141
action_120 _ = happyFail

action_121 (84) = happyShift action_104
action_121 (111) = happyShift action_33
action_121 (112) = happyShift action_36
action_121 (117) = happyAccept
action_121 (35) = happyGoto action_105
action_121 (36) = happyGoto action_102
action_121 (47) = happyGoto action_140
action_121 (48) = happyGoto action_107
action_121 _ = happyFail

action_122 (117) = happyAccept
action_122 _ = happyFail

action_123 (81) = happyShift action_139
action_123 _ = happyReduce_46

action_124 (117) = happyAccept
action_124 _ = happyFail

action_125 _ = happyReduce_44

action_126 (76) = happyShift action_138
action_126 (79) = happyReduce_133
action_126 _ = happyReduce_73

action_127 (81) = happyShift action_137
action_127 _ = happyReduce_42

action_128 (117) = happyAccept
action_128 _ = happyFail

action_129 _ = happyReduce_38

action_130 (79) = happyShift action_136
action_130 _ = happyFail

action_131 (112) = happyShift action_36
action_131 (36) = happyGoto action_34
action_131 (71) = happyGoto action_135
action_131 _ = happyFail

action_132 (117) = happyAccept
action_132 _ = happyFail

action_133 (117) = happyAccept
action_133 _ = happyFail

action_134 _ = happyReduce_37

action_135 _ = happyReduce_40

action_136 (72) = happyShift action_97
action_136 (84) = happyShift action_98
action_136 (86) = happyShift action_99
action_136 (88) = happyShift action_110
action_136 (89) = happyShift action_111
action_136 (90) = happyShift action_112
action_136 (92) = happyShift action_113
action_136 (94) = happyShift action_114
action_136 (95) = happyShift action_115
action_136 (99) = happyShift action_116
action_136 (100) = happyShift action_117
action_136 (101) = happyShift action_118
action_136 (103) = happyShift action_119
action_136 (104) = happyShift action_120
action_136 (112) = happyShift action_36
action_136 (36) = happyGoto action_95
action_136 (46) = happyGoto action_217
action_136 (49) = happyGoto action_109
action_136 (50) = happyGoto action_101
action_136 _ = happyFail

action_137 (72) = happyShift action_97
action_137 (84) = happyShift action_98
action_137 (86) = happyShift action_99
action_137 (88) = happyShift action_110
action_137 (89) = happyShift action_111
action_137 (90) = happyShift action_112
action_137 (92) = happyShift action_113
action_137 (94) = happyShift action_114
action_137 (95) = happyShift action_115
action_137 (99) = happyShift action_116
action_137 (100) = happyShift action_117
action_137 (101) = happyShift action_118
action_137 (103) = happyShift action_119
action_137 (104) = happyShift action_120
action_137 (107) = happyShift action_131
action_137 (112) = happyShift action_36
action_137 (36) = happyGoto action_126
action_137 (41) = happyGoto action_127
action_137 (42) = happyGoto action_216
action_137 (46) = happyGoto action_129
action_137 (49) = happyGoto action_109
action_137 (50) = happyGoto action_101
action_137 (71) = happyGoto action_130
action_137 _ = happyReduce_41

action_138 (112) = happyShift action_36
action_138 (36) = happyGoto action_34
action_138 (71) = happyGoto action_215
action_138 _ = happyFail

action_139 (72) = happyShift action_97
action_139 (84) = happyShift action_98
action_139 (86) = happyShift action_99
action_139 (88) = happyShift action_110
action_139 (89) = happyShift action_111
action_139 (90) = happyShift action_112
action_139 (92) = happyShift action_113
action_139 (94) = happyShift action_114
action_139 (95) = happyShift action_115
action_139 (99) = happyShift action_116
action_139 (100) = happyShift action_117
action_139 (101) = happyShift action_118
action_139 (103) = happyShift action_119
action_139 (104) = happyShift action_120
action_139 (112) = happyShift action_36
action_139 (36) = happyGoto action_95
action_139 (44) = happyGoto action_214
action_139 (46) = happyGoto action_123
action_139 (49) = happyGoto action_109
action_139 (50) = happyGoto action_101
action_139 _ = happyReduce_45

action_140 _ = happyReduce_49

action_141 (72) = happyShift action_42
action_141 (84) = happyShift action_43
action_141 (87) = happyShift action_44
action_141 (91) = happyShift action_45
action_141 (93) = happyShift action_46
action_141 (96) = happyShift action_47
action_141 (97) = happyShift action_48
action_141 (106) = happyShift action_49
action_141 (108) = happyShift action_50
action_141 (114) = happyShift action_51
action_141 (38) = happyGoto action_37
action_141 (67) = happyGoto action_38
action_141 (68) = happyGoto action_39
action_141 (69) = happyGoto action_40
action_141 (70) = happyGoto action_213
action_141 _ = happyFail

action_142 (84) = happyShift action_104
action_142 (112) = happyShift action_36
action_142 (36) = happyGoto action_102
action_142 (48) = happyGoto action_212
action_142 _ = happyFail

action_143 (84) = happyShift action_104
action_143 (112) = happyShift action_36
action_143 (36) = happyGoto action_102
action_143 (48) = happyGoto action_211
action_143 _ = happyFail

action_144 (80) = happyShift action_210
action_144 _ = happyFail

action_145 (112) = happyShift action_36
action_145 (36) = happyGoto action_209
action_145 _ = happyFail

action_146 _ = happyReduce_62

action_147 (111) = happyShift action_33
action_147 (35) = happyGoto action_61
action_147 (63) = happyGoto action_208
action_147 _ = happyFail

action_148 _ = happyReduce_64

action_149 (78) = happyShift action_207
action_149 _ = happyFail

action_150 _ = happyReduce_56

action_151 (57) = happyGoto action_206
action_151 _ = happyReduce_89

action_152 (111) = happyShift action_33
action_152 (35) = happyGoto action_205
action_152 _ = happyReduce_51

action_153 (113) = happyShift action_74
action_153 (37) = happyGoto action_204
action_153 _ = happyFail

action_154 (84) = happyShift action_104
action_154 (112) = happyShift action_36
action_154 (36) = happyGoto action_102
action_154 (48) = happyGoto action_203
action_154 _ = happyFail

action_155 (85) = happyShift action_202
action_155 _ = happyFail

action_156 (72) = happyShift action_87
action_156 (51) = happyGoto action_85
action_156 (55) = happyGoto action_201
action_156 _ = happyReduce_72

action_157 _ = happyReduce_70

action_158 (79) = happyShift action_160
action_158 (84) = happyShift action_164
action_158 _ = happyFail

action_159 _ = happyReduce_75

action_160 (73) = happyShift action_199
action_160 (84) = happyShift action_200
action_160 _ = happyFail

action_161 (113) = happyShift action_74
action_161 (37) = happyGoto action_91
action_161 (53) = happyGoto action_198
action_161 _ = happyReduce_80

action_162 (85) = happyShift action_197
action_162 _ = happyFail

action_163 (84) = happyShift action_90
action_163 (52) = happyGoto action_88
action_163 (54) = happyGoto action_196
action_163 _ = happyReduce_83

action_164 (84) = happyShift action_90
action_164 (52) = happyGoto action_88
action_164 (54) = happyGoto action_195
action_164 _ = happyReduce_83

action_165 _ = happyReduce_87

action_166 _ = happyReduce_90

action_167 _ = happyReduce_103

action_168 (85) = happyShift action_194
action_168 _ = happyFail

action_169 (73) = happyShift action_193
action_169 _ = happyFail

action_170 (72) = happyShift action_72
action_170 (84) = happyShift action_73
action_170 (111) = happyShift action_33
action_170 (112) = happyShift action_36
action_170 (113) = happyShift action_74
action_170 (114) = happyShift action_51
action_170 (115) = happyShift action_75
action_170 (35) = happyGoto action_63
action_170 (36) = happyGoto action_64
action_170 (37) = happyGoto action_65
action_170 (38) = happyGoto action_66
action_170 (39) = happyGoto action_67
action_170 (58) = happyGoto action_192
action_170 (59) = happyGoto action_69
action_170 (60) = happyGoto action_70
action_170 _ = happyFail

action_171 (72) = happyShift action_72
action_171 (84) = happyShift action_73
action_171 (111) = happyShift action_33
action_171 (112) = happyShift action_36
action_171 (113) = happyShift action_74
action_171 (114) = happyShift action_51
action_171 (115) = happyShift action_75
action_171 (35) = happyGoto action_63
action_171 (36) = happyGoto action_64
action_171 (37) = happyGoto action_65
action_171 (38) = happyGoto action_66
action_171 (39) = happyGoto action_67
action_171 (58) = happyGoto action_68
action_171 (59) = happyGoto action_69
action_171 (60) = happyGoto action_70
action_171 (62) = happyGoto action_191
action_171 _ = happyReduce_104

action_172 _ = happyReduce_93

action_173 (111) = happyShift action_33
action_173 (35) = happyGoto action_61
action_173 (63) = happyGoto action_190
action_173 _ = happyFail

action_174 (45) = happyGoto action_57
action_174 (64) = happyGoto action_189
action_174 (65) = happyGoto action_60
action_174 _ = happyReduce_48

action_175 (74) = happyShift action_177
action_175 (75) = happyShift action_178
action_175 (83) = happyShift action_179
action_175 _ = happyReduce_114

action_176 (72) = happyShift action_42
action_176 (84) = happyShift action_43
action_176 (87) = happyShift action_44
action_176 (91) = happyShift action_45
action_176 (93) = happyShift action_46
action_176 (96) = happyShift action_47
action_176 (97) = happyShift action_48
action_176 (106) = happyShift action_49
action_176 (108) = happyShift action_50
action_176 (114) = happyShift action_51
action_176 (38) = happyGoto action_37
action_176 (67) = happyGoto action_188
action_176 (69) = happyGoto action_40
action_176 _ = happyFail

action_177 _ = happyReduce_119

action_178 _ = happyReduce_120

action_179 _ = happyReduce_121

action_180 (110) = happyShift action_187
action_180 _ = happyFail

action_181 (85) = happyShift action_186
action_181 _ = happyFail

action_182 (73) = happyShift action_185
action_182 _ = happyFail

action_183 (72) = happyShift action_42
action_183 (84) = happyShift action_43
action_183 (87) = happyShift action_44
action_183 (91) = happyShift action_45
action_183 (93) = happyShift action_46
action_183 (96) = happyShift action_47
action_183 (97) = happyShift action_48
action_183 (106) = happyShift action_49
action_183 (108) = happyShift action_50
action_183 (114) = happyShift action_51
action_183 (38) = happyGoto action_37
action_183 (67) = happyGoto action_184
action_183 (69) = happyGoto action_40
action_183 _ = happyFail

action_184 (72) = happyShift action_42
action_184 (84) = happyShift action_43
action_184 (87) = happyShift action_44
action_184 (91) = happyShift action_45
action_184 (93) = happyShift action_46
action_184 (96) = happyShift action_47
action_184 (97) = happyShift action_48
action_184 (106) = happyShift action_49
action_184 (108) = happyShift action_50
action_184 (114) = happyShift action_51
action_184 (38) = happyGoto action_37
action_184 (69) = happyGoto action_175
action_184 _ = happyReduce_117

action_185 _ = happyReduce_131

action_186 _ = happyReduce_124

action_187 _ = happyReduce_125

action_188 (72) = happyShift action_42
action_188 (84) = happyShift action_43
action_188 (87) = happyShift action_44
action_188 (91) = happyShift action_45
action_188 (93) = happyShift action_46
action_188 (96) = happyShift action_47
action_188 (97) = happyShift action_48
action_188 (106) = happyShift action_49
action_188 (108) = happyShift action_50
action_188 (114) = happyShift action_51
action_188 (38) = happyGoto action_37
action_188 (69) = happyGoto action_175
action_188 _ = happyReduce_116

action_189 _ = happyReduce_110

action_190 _ = happyReduce_108

action_191 _ = happyReduce_106

action_192 _ = happyReduce_91

action_193 _ = happyReduce_101

action_194 _ = happyReduce_100

action_195 (85) = happyShift action_226
action_195 _ = happyFail

action_196 _ = happyReduce_85

action_197 _ = happyReduce_79

action_198 _ = happyReduce_82

action_199 _ = happyReduce_76

action_200 (85) = happyShift action_225
action_200 _ = happyFail

action_201 _ = happyReduce_71

action_202 _ = happyReduce_67

action_203 (80) = happyShift action_224
action_203 _ = happyFail

action_204 _ = happyReduce_59

action_205 _ = happyReduce_52

action_206 (82) = happyShift action_223
action_206 (112) = happyShift action_36
action_206 (36) = happyGoto action_83
action_206 (56) = happyGoto action_166
action_206 _ = happyFail

action_207 (84) = happyShift action_104
action_207 (112) = happyShift action_36
action_207 (36) = happyGoto action_102
action_207 (48) = happyGoto action_222
action_207 _ = happyFail

action_208 _ = happyReduce_63

action_209 (72) = happyShift action_42
action_209 (84) = happyShift action_43
action_209 (87) = happyShift action_44
action_209 (91) = happyShift action_45
action_209 (93) = happyShift action_46
action_209 (96) = happyShift action_47
action_209 (97) = happyShift action_48
action_209 (106) = happyShift action_49
action_209 (108) = happyShift action_50
action_209 (114) = happyShift action_51
action_209 (38) = happyGoto action_37
action_209 (67) = happyGoto action_38
action_209 (68) = happyGoto action_39
action_209 (69) = happyGoto action_40
action_209 (70) = happyGoto action_221
action_209 _ = happyFail

action_210 (45) = happyGoto action_57
action_210 (64) = happyGoto action_220
action_210 (65) = happyGoto action_60
action_210 _ = happyReduce_48

action_211 (111) = happyShift action_33
action_211 (35) = happyGoto action_219
action_211 _ = happyFail

action_212 (111) = happyShift action_33
action_212 (35) = happyGoto action_218
action_212 _ = happyFail

action_213 _ = happyReduce_54

action_214 _ = happyReduce_47

action_215 _ = happyReduce_134

action_216 _ = happyReduce_43

action_217 _ = happyReduce_39

action_218 _ = happyReduce_58

action_219 _ = happyReduce_57

action_220 _ = happyReduce_60

action_221 _ = happyReduce_55

action_222 (80) = happyShift action_231
action_222 _ = happyFail

action_223 (72) = happyShift action_72
action_223 (84) = happyShift action_73
action_223 (111) = happyShift action_33
action_223 (112) = happyShift action_36
action_223 (113) = happyShift action_74
action_223 (114) = happyShift action_51
action_223 (115) = happyShift action_75
action_223 (35) = happyGoto action_63
action_223 (36) = happyGoto action_64
action_223 (37) = happyGoto action_65
action_223 (38) = happyGoto action_66
action_223 (39) = happyGoto action_67
action_223 (58) = happyGoto action_230
action_223 (59) = happyGoto action_69
action_223 (60) = happyGoto action_70
action_223 _ = happyFail

action_224 (45) = happyGoto action_229
action_224 _ = happyReduce_48

action_225 (73) = happyShift action_228
action_225 _ = happyFail

action_226 (76) = happyShift action_227
action_226 _ = happyFail

action_227 (84) = happyShift action_233
action_227 _ = happyFail

action_228 _ = happyReduce_77

action_229 (84) = happyShift action_104
action_229 (111) = happyShift action_33
action_229 (112) = happyShift action_36
action_229 (35) = happyGoto action_105
action_229 (36) = happyGoto action_102
action_229 (47) = happyGoto action_140
action_229 (48) = happyGoto action_107
action_229 _ = happyReduce_50

action_230 _ = happyReduce_61

action_231 (45) = happyGoto action_232
action_231 _ = happyReduce_48

action_232 (84) = happyShift action_104
action_232 (111) = happyShift action_33
action_232 (112) = happyShift action_36
action_232 (35) = happyGoto action_105
action_232 (36) = happyGoto action_102
action_232 (47) = happyGoto action_140
action_232 (48) = happyGoto action_107
action_232 _ = happyReduce_53

action_233 (113) = happyShift action_74
action_233 (37) = happyGoto action_91
action_233 (53) = happyGoto action_234
action_233 _ = happyReduce_80

action_234 (85) = happyShift action_235
action_234 _ = happyFail

action_235 (73) = happyShift action_236
action_235 _ = happyFail

action_236 _ = happyReduce_78

happyReduce_32 = happySpecReduce_1  35 happyReduction_32
happyReduction_32 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  36 happyReduction_33
happyReduction_33 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn36
		 (Ident happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  37 happyReduction_34
happyReduction_34 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn37
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  38 happyReduction_35
happyReduction_35 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn38
		 ((read ( happy_var_1)) :: Char
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  39 happyReduction_36
happyReduction_36 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn39
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  40 happyReduction_37
happyReduction_37 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 (LGr happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  41 happyReduction_38
happyReduction_38 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn41
		 (DefAll happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  41 happyReduction_39
happyReduction_39 (HappyAbsSyn46  happy_var_3)
	_
	(HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn41
		 (DefSome happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  41 happyReduction_40
happyReduction_40 (HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (LDefView happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  42 happyReduction_41
happyReduction_41  =  HappyAbsSyn42
		 ([]
	)

happyReduce_42 = happySpecReduce_1  42 happyReduction_42
happyReduction_42 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn42
		 ((:[]) happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  42 happyReduction_43
happyReduction_43 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn42
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  43 happyReduction_44
happyReduction_44 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn43
		 (Grammar happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  44 happyReduction_45
happyReduction_45  =  HappyAbsSyn44
		 ([]
	)

happyReduce_46 = happySpecReduce_1  44 happyReduction_46
happyReduction_46 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn44
		 ((:[]) happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  44 happyReduction_47
happyReduction_47 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn44
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  45 happyReduction_48
happyReduction_48  =  HappyAbsSyn45
		 ([]
	)

happyReduce_49 = happySpecReduce_2  45 happyReduction_49
happyReduction_49 (HappyAbsSyn47  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happyReduce 5 46 happyReduction_50
happyReduction_50 ((HappyAbsSyn45  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Rule happy_var_1 happy_var_3 (reverse happy_var_5)
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_2  46 happyReduction_51
happyReduction_51 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Comment happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  46 happyReduction_52
happyReduction_52 (HappyAbsSyn35  happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Comments happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 6 46 happyReduction_53
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

happyReduce_54 = happySpecReduce_3  46 happyReduction_54
happyReduction_54 (HappyAbsSyn67  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Token happy_var_2 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happyReduce 4 46 happyReduction_55
happyReduction_55 ((HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (PosToken happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_2  46 happyReduction_56
happyReduction_56 (HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Entryp happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happyReduce 4 46 happyReduction_57
happyReduction_57 ((HappyAbsSyn35  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	(HappyAbsSyn66  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Separator happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_58 = happyReduce 4 46 happyReduction_58
happyReduction_58 ((HappyAbsSyn35  happy_var_4) `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	(HappyAbsSyn66  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Terminator happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_59 = happySpecReduce_3  46 happyReduction_59
happyReduction_59 (HappyAbsSyn37  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Coercions happy_var_2 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happyReduce 4 46 happyReduction_60
happyReduction_60 ((HappyAbsSyn64  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Rules happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_61 = happyReduce 5 46 happyReduction_61
happyReduction_61 ((HappyAbsSyn58  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn57  happy_var_3) `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (Function happy_var_2 (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_2  46 happyReduction_62
happyReduction_62 (HappyAbsSyn63  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Layout happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  46 happyReduction_63
happyReduction_63 (HappyAbsSyn63  happy_var_3)
	_
	_
	 =  HappyAbsSyn46
		 (LayoutStop happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  46 happyReduction_64
happyReduction_64 _
	_
	 =  HappyAbsSyn46
		 (LayoutTop
	)

happyReduce_65 = happySpecReduce_1  47 happyReduction_65
happyReduction_65 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn47
		 (Terminal happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  47 happyReduction_66
happyReduction_66 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn47
		 (NTerminal happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  48 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn48
		 (ListCat happy_var_2
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  48 happyReduction_68
happyReduction_68 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn48
		 (IdCat happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  49 happyReduction_69
happyReduction_69 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (LabNoP happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  49 happyReduction_70
happyReduction_70 (HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (LabP happy_var_1 happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  49 happyReduction_71
happyReduction_71 (HappyAbsSyn55  happy_var_3)
	(HappyAbsSyn50  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (LabPF happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_2  49 happyReduction_72
happyReduction_72 (HappyAbsSyn50  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (LabF happy_var_1 happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  50 happyReduction_73
happyReduction_73 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn50
		 (Id happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  50 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn50
		 (Wild
	)

happyReduce_75 = happySpecReduce_2  50 happyReduction_75
happyReduction_75 _
	_
	 =  HappyAbsSyn50
		 (ListE
	)

happyReduce_76 = happySpecReduce_3  50 happyReduction_76
happyReduction_76 _
	_
	_
	 =  HappyAbsSyn50
		 (ListCons
	)

happyReduce_77 = happyReduce 5 50 happyReduction_77
happyReduction_77 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn50
		 (ListOne
	) `HappyStk` happyRest

happyReduce_78 = happyReduce 9 51 happyReduction_78
happyReduction_78 (_ `HappyStk`
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

happyReduce_79 = happySpecReduce_3  52 happyReduction_79
happyReduction_79 _
	(HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn52
		 (Ints happy_var_2
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_0  53 happyReduction_80
happyReduction_80  =  HappyAbsSyn53
		 ([]
	)

happyReduce_81 = happySpecReduce_1  53 happyReduction_81
happyReduction_81 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn53
		 ((:[]) happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  53 happyReduction_82
happyReduction_82 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn53
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_0  54 happyReduction_83
happyReduction_83  =  HappyAbsSyn54
		 ([]
	)

happyReduce_84 = happySpecReduce_1  54 happyReduction_84
happyReduction_84 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn54
		 ((:[]) happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_3  54 happyReduction_85
happyReduction_85 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn54
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_85 _ _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  55 happyReduction_86
happyReduction_86 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn55
		 ((:[]) happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_2  55 happyReduction_87
happyReduction_87 (HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn55
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  56 happyReduction_88
happyReduction_88 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn56
		 (Arg happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_0  57 happyReduction_89
happyReduction_89  =  HappyAbsSyn57
		 ([]
	)

happyReduce_90 = happySpecReduce_2  57 happyReduction_90
happyReduction_90 (HappyAbsSyn56  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_90 _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  58 happyReduction_91
happyReduction_91 (HappyAbsSyn58  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (Cons happy_var_1 happy_var_3
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  58 happyReduction_92
happyReduction_92 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_2  59 happyReduction_93
happyReduction_93 (HappyAbsSyn61  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn58
		 (App happy_var_1 happy_var_2
	)
happyReduction_93 _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  59 happyReduction_94
happyReduction_94 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  60 happyReduction_95
happyReduction_95 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn58
		 (Var happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  60 happyReduction_96
happyReduction_96 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn58
		 (LitInt happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  60 happyReduction_97
happyReduction_97 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn58
		 (LitChar happy_var_1
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  60 happyReduction_98
happyReduction_98 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn58
		 (LitString happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  60 happyReduction_99
happyReduction_99 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn58
		 (LitDouble happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  60 happyReduction_100
happyReduction_100 _
	(HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn58
		 (List happy_var_2
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  60 happyReduction_101
happyReduction_101 _
	(HappyAbsSyn58  happy_var_2)
	_
	 =  HappyAbsSyn58
		 (happy_var_2
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  61 happyReduction_102
happyReduction_102 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 ((:[]) happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_2  61 happyReduction_103
happyReduction_103 (HappyAbsSyn61  happy_var_2)
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_103 _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_0  62 happyReduction_104
happyReduction_104  =  HappyAbsSyn61
		 ([]
	)

happyReduce_105 = happySpecReduce_1  62 happyReduction_105
happyReduction_105 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 ((:[]) happy_var_1
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_3  62 happyReduction_106
happyReduction_106 (HappyAbsSyn61  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  63 happyReduction_107
happyReduction_107 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn63
		 ((:[]) happy_var_1
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3  63 happyReduction_108
happyReduction_108 (HappyAbsSyn63  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn63
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  64 happyReduction_109
happyReduction_109 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn64
		 ((:[]) happy_var_1
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_3  64 happyReduction_110
happyReduction_110 (HappyAbsSyn64  happy_var_3)
	_
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn64
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_110 _ _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  65 happyReduction_111
happyReduction_111 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn65
		 (RHS (reverse happy_var_1)
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  66 happyReduction_112
happyReduction_112 _
	 =  HappyAbsSyn66
		 (MNonempty
	)

happyReduce_113 = happySpecReduce_0  66 happyReduction_113
happyReduction_113  =  HappyAbsSyn66
		 (MEmpty
	)

happyReduce_114 = happySpecReduce_2  67 happyReduction_114
happyReduction_114 (HappyAbsSyn67  happy_var_2)
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RSeq happy_var_1 happy_var_2
	)
happyReduction_114 _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  67 happyReduction_115
happyReduction_115 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_3  68 happyReduction_116
happyReduction_116 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RAlt happy_var_1 happy_var_3
	)
happyReduction_116 _ _ _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_3  68 happyReduction_117
happyReduction_117 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RMinus happy_var_1 happy_var_3
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  68 happyReduction_118
happyReduction_118 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_2  69 happyReduction_119
happyReduction_119 _
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RStar happy_var_1
	)
happyReduction_119 _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_2  69 happyReduction_120
happyReduction_120 _
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (RPlus happy_var_1
	)
happyReduction_120 _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_2  69 happyReduction_121
happyReduction_121 _
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (ROpt happy_var_1
	)
happyReduction_121 _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  69 happyReduction_122
happyReduction_122 _
	 =  HappyAbsSyn67
		 (REps
	)

happyReduce_123 = happySpecReduce_1  69 happyReduction_123
happyReduction_123 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn67
		 (RChar happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_3  69 happyReduction_124
happyReduction_124 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (RAlts happy_var_2
	)
happyReduction_124 _ _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_3  69 happyReduction_125
happyReduction_125 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (RSeqs happy_var_2
	)
happyReduction_125 _ _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  69 happyReduction_126
happyReduction_126 _
	 =  HappyAbsSyn67
		 (RDigit
	)

happyReduce_127 = happySpecReduce_1  69 happyReduction_127
happyReduction_127 _
	 =  HappyAbsSyn67
		 (RLetter
	)

happyReduce_128 = happySpecReduce_1  69 happyReduction_128
happyReduction_128 _
	 =  HappyAbsSyn67
		 (RUpper
	)

happyReduce_129 = happySpecReduce_1  69 happyReduction_129
happyReduction_129 _
	 =  HappyAbsSyn67
		 (RLower
	)

happyReduce_130 = happySpecReduce_1  69 happyReduction_130
happyReduction_130 _
	 =  HappyAbsSyn67
		 (RAny
	)

happyReduce_131 = happySpecReduce_3  69 happyReduction_131
happyReduction_131 _
	(HappyAbsSyn67  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (happy_var_2
	)
happyReduction_131 _ _ _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_1  70 happyReduction_132
happyReduction_132 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_132 _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_1  71 happyReduction_133
happyReduction_133 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn71
		 ((:[]) happy_var_1
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_3  71 happyReduction_134
happyReduction_134 (HappyAbsSyn71  happy_var_3)
	_
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn71
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_134 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 117 117 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 72;
	PT _ (TS _ 2) -> cont 73;
	PT _ (TS _ 3) -> cont 74;
	PT _ (TS _ 4) -> cont 75;
	PT _ (TS _ 5) -> cont 76;
	PT _ (TS _ 6) -> cont 77;
	PT _ (TS _ 7) -> cont 78;
	PT _ (TS _ 8) -> cont 79;
	PT _ (TS _ 9) -> cont 80;
	PT _ (TS _ 10) -> cont 81;
	PT _ (TS _ 11) -> cont 82;
	PT _ (TS _ 12) -> cont 83;
	PT _ (TS _ 13) -> cont 84;
	PT _ (TS _ 14) -> cont 85;
	PT _ (TS _ 15) -> cont 86;
	PT _ (TS _ 16) -> cont 87;
	PT _ (TS _ 17) -> cont 88;
	PT _ (TS _ 18) -> cont 89;
	PT _ (TS _ 19) -> cont 90;
	PT _ (TS _ 20) -> cont 91;
	PT _ (TS _ 21) -> cont 92;
	PT _ (TS _ 22) -> cont 93;
	PT _ (TS _ 23) -> cont 94;
	PT _ (TS _ 24) -> cont 95;
	PT _ (TS _ 25) -> cont 96;
	PT _ (TS _ 26) -> cont 97;
	PT _ (TS _ 27) -> cont 98;
	PT _ (TS _ 28) -> cont 99;
	PT _ (TS _ 29) -> cont 100;
	PT _ (TS _ 30) -> cont 101;
	PT _ (TS _ 31) -> cont 102;
	PT _ (TS _ 32) -> cont 103;
	PT _ (TS _ 33) -> cont 104;
	PT _ (TS _ 34) -> cont 105;
	PT _ (TS _ 35) -> cont 106;
	PT _ (TS _ 36) -> cont 107;
	PT _ (TS _ 37) -> cont 108;
	PT _ (TS _ 38) -> cont 109;
	PT _ (TS _ 39) -> cont 110;
	PT _ (TL happy_dollar_dollar) -> cont 111;
	PT _ (TV happy_dollar_dollar) -> cont 112;
	PT _ (TI happy_dollar_dollar) -> cont 113;
	PT _ (TC happy_dollar_dollar) -> cont 114;
	PT _ (TD happy_dollar_dollar) -> cont 115;
	_ -> cont 116;
	_ -> happyError' (tk:tks)
	}

happyError_ 117 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

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
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates\\GenericTemplate.hs" #-}








{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}

{-# LINE 70 "templates\\GenericTemplate.hs" #-}

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

{-# LINE 148 "templates\\GenericTemplate.hs" #-}

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

{-# LINE 246 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

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

{-# LINE 312 "templates\\GenericTemplate.hs" #-}
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
