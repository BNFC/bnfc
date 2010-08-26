{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParBNF where
import AbsBNF
import LexBNF
import ErrM
import Array
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.15

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
happyIn35 :: (String) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (String)
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Ident) -> (HappyAbsSyn )
happyIn36 x = unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Ident)
happyOut36 x = unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Integer) -> (HappyAbsSyn )
happyIn37 x = unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Integer)
happyOut37 x = unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Char) -> (HappyAbsSyn )
happyIn38 x = unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Char)
happyOut38 x = unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Double) -> (HappyAbsSyn )
happyIn39 x = unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Double)
happyOut39 x = unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (LGrammar) -> (HappyAbsSyn )
happyIn40 x = unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (LGrammar)
happyOut40 x = unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (LDef) -> (HappyAbsSyn )
happyIn41 x = unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (LDef)
happyOut41 x = unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ([LDef]) -> (HappyAbsSyn )
happyIn42 x = unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> ([LDef])
happyOut42 x = unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Grammar) -> (HappyAbsSyn )
happyIn43 x = unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Grammar)
happyOut43 x = unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([Def]) -> (HappyAbsSyn )
happyIn44 x = unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([Def])
happyOut44 x = unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([Item]) -> (HappyAbsSyn )
happyIn45 x = unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ([Item])
happyOut45 x = unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (Def) -> (HappyAbsSyn )
happyIn46 x = unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (Def)
happyOut46 x = unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Item) -> (HappyAbsSyn )
happyIn47 x = unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Item)
happyOut47 x = unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (Cat) -> (HappyAbsSyn )
happyIn48 x = unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (Cat)
happyOut48 x = unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (Label) -> (HappyAbsSyn )
happyIn49 x = unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (Label)
happyOut49 x = unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (LabelId) -> (HappyAbsSyn )
happyIn50 x = unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (LabelId)
happyOut50 x = unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (ProfItem) -> (HappyAbsSyn )
happyIn51 x = unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (ProfItem)
happyOut51 x = unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (IntList) -> (HappyAbsSyn )
happyIn52 x = unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (IntList)
happyOut52 x = unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: ([Integer]) -> (HappyAbsSyn )
happyIn53 x = unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> ([Integer])
happyOut53 x = unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: ([IntList]) -> (HappyAbsSyn )
happyIn54 x = unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> ([IntList])
happyOut54 x = unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: ([ProfItem]) -> (HappyAbsSyn )
happyIn55 x = unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> ([ProfItem])
happyOut55 x = unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (Arg) -> (HappyAbsSyn )
happyIn56 x = unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (Arg)
happyOut56 x = unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: ([Arg]) -> (HappyAbsSyn )
happyIn57 x = unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> ([Arg])
happyOut57 x = unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (Exp) -> (HappyAbsSyn )
happyIn58 x = unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (Exp)
happyOut58 x = unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (Exp) -> (HappyAbsSyn )
happyIn59 x = unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (Exp)
happyOut59 x = unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (Exp) -> (HappyAbsSyn )
happyIn60 x = unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (Exp)
happyOut60 x = unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: ([Exp]) -> (HappyAbsSyn )
happyIn61 x = unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> ([Exp])
happyOut61 x = unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: ([Exp]) -> (HappyAbsSyn )
happyIn62 x = unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> ([Exp])
happyOut62 x = unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: ([String]) -> (HappyAbsSyn )
happyIn63 x = unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> ([String])
happyOut63 x = unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: ([RHS]) -> (HappyAbsSyn )
happyIn64 x = unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> ([RHS])
happyOut64 x = unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyIn65 :: (RHS) -> (HappyAbsSyn )
happyIn65 x = unsafeCoerce# x
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> (RHS)
happyOut65 x = unsafeCoerce# x
{-# INLINE happyOut65 #-}
happyIn66 :: (MinimumSize) -> (HappyAbsSyn )
happyIn66 x = unsafeCoerce# x
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> (MinimumSize)
happyOut66 x = unsafeCoerce# x
{-# INLINE happyOut66 #-}
happyIn67 :: (Reg) -> (HappyAbsSyn )
happyIn67 x = unsafeCoerce# x
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> (Reg)
happyOut67 x = unsafeCoerce# x
{-# INLINE happyOut67 #-}
happyIn68 :: (Reg) -> (HappyAbsSyn )
happyIn68 x = unsafeCoerce# x
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> (Reg)
happyOut68 x = unsafeCoerce# x
{-# INLINE happyOut68 #-}
happyIn69 :: (Reg) -> (HappyAbsSyn )
happyIn69 x = unsafeCoerce# x
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> (Reg)
happyOut69 x = unsafeCoerce# x
{-# INLINE happyOut69 #-}
happyIn70 :: (Reg) -> (HappyAbsSyn )
happyIn70 x = unsafeCoerce# x
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> (Reg)
happyOut70 x = unsafeCoerce# x
{-# INLINE happyOut70 #-}
happyIn71 :: ([Ident]) -> (HappyAbsSyn )
happyIn71 x = unsafeCoerce# x
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> ([Ident])
happyOut71 x = unsafeCoerce# x
{-# INLINE happyOut71 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x4c\x00\x4c\x00\x4c\x00\x6f\x00\x6f\x00\x00\x00\x6f\x00\x0d\x00\x11\x00\x73\x00\x73\x00\xcf\x01\xcc\x01\xc7\x01\xcb\x01\xca\x01\xc9\x01\x00\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\xc8\x01\x00\x00\x00\x00\xc6\x01\x33\x00\x33\x00\x33\x00\x33\x00\xc5\x01\xc2\x01\x00\x00\xc4\x01\xc1\x01\x00\x00\x00\x00\x2c\x00\xc0\x01\x86\x01\xb8\x01\xba\x01\x33\x00\xba\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x00\x14\x00\xfc\xff\xb2\x01\x00\x00\x0d\x00\xb2\x01\xb2\x01\xbe\x01\xbf\x01\xaf\x01\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\xbc\x01\xc3\x01\x00\x00\xae\x01\x04\x00\x04\x00\x00\x00\x00\x00\x00\x00\x04\x00\xae\x01\xae\x01\xae\x01\xae\x01\xab\x00\x00\x00\xae\x01\xbb\x01\xad\x01\xbd\x01\xb7\x01\xab\x01\xa9\x01\xb1\x01\xa8\x01\xa8\x01\xa8\x01\x00\x00\xa8\x01\xb9\x01\x00\x00\xb6\x01\xa7\x01\x53\x00\x00\x00\xa7\x01\x11\x00\x00\x00\xa7\x01\x00\x00\xa7\x01\xb5\x01\xa6\x01\xa5\x01\x9f\x01\x9f\x01\x09\x00\x69\x01\x95\x01\x9e\x01\xa0\x01\xa0\x01\x94\x01\xfd\xff\x92\x01\xb4\x01\x8e\x01\x00\x00\x17\x00\xb3\x01\x8c\x01\x00\x00\xb0\x01\x8b\x01\x80\x01\x80\x01\x00\x00\x00\x00\x6f\x00\x4c\x00\x8a\x01\x6f\x00\x00\x00\x33\x00\x11\x00\x11\x00\xac\x01\x89\x01\x00\x00\x87\x01\x00\x00\xaa\x01\x00\x00\x00\x00\x84\x01\x7f\x01\x11\x00\xa4\x01\xa3\x01\x00\x00\x88\x01\x66\x01\x00\x00\x7d\x01\xa2\x01\xa1\x01\xa1\x01\x00\x00\x00\x00\x00\x00\x9c\x01\x9d\x01\x04\x00\x04\x00\x00\x00\x7c\x01\x00\x00\x86\x01\x33\x00\x00\x00\x00\x00\x00\x00\x90\x01\x96\x01\x9b\x01\x33\x00\x33\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x01\x00\x00\x00\x00\x00\x00\x98\x01\x00\x00\x00\x00\x00\x00\x99\x01\x00\x00\x00\x00\xfa\xff\x11\x00\x00\x00\x33\x00\x00\x00\x74\x01\x74\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x97\x01\x04\x00\x00\x00\x91\x01\x8f\x01\x93\x01\x00\x00\x0d\x00\x00\x00\x00\x00\x0d\x00\x6d\x01\x8d\x01\x6c\x01\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x90\x00\xb6\x00\xa7\x00\x5f\x01\x6f\x01\x62\x01\x56\x01\x83\x01\x85\x01\x73\x01\x4d\x00\x41\x01\x3a\x01\xf0\x00\x79\x01\x52\x01\x64\x00\x34\x01\x36\x01\x3b\x01\x40\x01\x0d\x01\xe8\x00\x7d\x00\x3f\x01\x1e\x00\x0e\x01\xbb\x00\x60\x00\x83\x00\xb5\x00\x23\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x2c\x01\xb0\x00\x23\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x7b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\x00\x1c\x01\x00\x00\x00\x00\x00\x00\xed\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x14\x01\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x01\x00\x00\x00\x00\x78\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\xfd\x00\xf3\x00\x1a\x00\x71\x01\x56\x00\x00\x00\xf2\x00\xbd\x00\xa4\x00\xc1\x00\x7b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x01\x9c\x00\x05\x00\x68\x01\x00\x00\xac\x00\x4b\x01\x24\x01\x00\x00\xaf\x00\x00\x00\x55\x00\x00\x00\x00\x00\x00\x00\x96\x00\x7f\x00\x8a\x00\xf7\x00\x00\x00\xf7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x9f\x00\x00\x00\xa8\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x01\xde\x00\x00\x00\x41\x00\x3d\x01\x00\x00\x9d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x99\x00\x00\x00\xa6\x00\x28\x01\x1d\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x01\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x01\x00\x00\xfe\xff\x7b\x01\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd6\xff\x00\x00\xd6\xff\xd2\xff\xd2\xff\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\xff\xac\xff\x00\x00\x00\x00\xa6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x97\xff\x00\x00\xcf\xff\xcf\xff\x8e\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\x7a\xff\x00\x00\xde\xff\x84\xff\x89\xff\x7b\xff\x8c\xff\x00\x00\x00\x00\x00\x00\x00\x00\x7d\xff\x81\xff\x85\xff\x80\xff\x7e\xff\x7f\xff\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x8f\xff\x90\xff\x00\x00\x00\x00\x92\xff\x94\xff\x00\x00\x9d\xff\xa0\xff\x9f\xff\x9e\xff\x9c\xff\x96\xff\xa3\xff\xa1\xff\x00\x00\x97\xff\x00\x00\xdd\xff\xdb\xff\xa0\xff\x99\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa7\xff\x00\x00\xa9\xff\x00\x00\x00\x00\xab\xff\x00\x00\xaf\xff\xae\xff\x00\x00\x00\x00\x00\x00\xb6\xff\x00\x00\x00\x00\xb5\xff\x00\x00\x00\x00\xba\xff\xbb\xff\x00\x00\x00\x00\xbe\xff\x00\x00\xbd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\xff\x8e\xff\x00\x00\x00\x00\x00\x00\xd1\xff\x00\x00\xd3\xff\x7a\xff\xd5\xff\x00\x00\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\xd7\xff\x00\x00\xd6\xff\x00\x00\xd2\xff\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\x00\x00\xbf\xff\x00\x00\xc7\xff\xa6\xff\xcc\xff\x00\x00\x00\x00\x00\x00\xb7\xff\xb9\xff\x00\x00\x00\x00\xb4\xff\xaf\xff\x00\x00\xac\xff\xac\xff\xa8\xff\xa5\xff\x98\xff\x00\x00\x00\x00\x00\x00\x97\xff\xa2\xff\x00\x00\xcf\xff\x8d\xff\x00\x00\x88\xff\x87\xff\x86\xff\x00\x00\x00\x00\x00\x00\x00\x00\x8a\xff\x83\xff\x7c\xff\x82\xff\x8b\xff\x91\xff\x93\xff\x95\xff\xa4\xff\x9b\xff\x9a\xff\x00\x00\xaa\xff\xb0\xff\xad\xff\x00\x00\xb3\xff\xb8\xff\xbc\xff\x00\x00\xc4\xff\xcb\xff\x00\x00\x00\x00\xc0\xff\x00\x00\xcf\xff\x00\x00\x00\x00\xc9\xff\xd0\xff\x79\xff\xd4\xff\xd8\xff\xc5\xff\xc6\xff\xc3\xff\xc8\xff\x00\x00\x00\x00\xcf\xff\x00\x00\x00\x00\x00\x00\xb2\xff\xcd\xff\xc2\xff\xcf\xff\xca\xff\xaf\xff\x00\x00\x00\x00\xb1\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x05\x00\x01\x00\x08\x00\x0b\x00\x01\x00\x10\x00\x0a\x00\x05\x00\x00\x00\x14\x00\x08\x00\x11\x00\x05\x00\x13\x00\x07\x00\x08\x00\x05\x00\x17\x00\x01\x00\x19\x00\x05\x00\x15\x00\x1c\x00\x1d\x00\x03\x00\x01\x00\x05\x00\x00\x00\x07\x00\x08\x00\x0c\x00\x0a\x00\x26\x00\x29\x00\x01\x00\x28\x00\x29\x00\x2b\x00\x0a\x00\x24\x00\x2e\x00\x2e\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x05\x00\x29\x00\x0a\x00\x08\x00\x28\x00\x29\x00\x24\x00\x05\x00\x0d\x00\x29\x00\x08\x00\x1e\x00\x11\x00\x24\x00\x13\x00\x29\x00\x00\x00\x2e\x00\x17\x00\x11\x00\x19\x00\x13\x00\x24\x00\x1c\x00\x1d\x00\x17\x00\x11\x00\x19\x00\x13\x00\x01\x00\x1c\x00\x1d\x00\x05\x00\x26\x00\x07\x00\x08\x00\x00\x00\x00\x00\x2b\x00\x05\x00\x26\x00\x07\x00\x08\x00\x0f\x00\x1c\x00\x2b\x00\x03\x00\x14\x00\x15\x00\x16\x00\x03\x00\x18\x00\x01\x00\x1a\x00\x1b\x00\x0e\x00\x0f\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x03\x00\x23\x00\x24\x00\x1c\x00\x1c\x00\x27\x00\x05\x00\x29\x00\x07\x00\x08\x00\x05\x00\x15\x00\x07\x00\x08\x00\x29\x00\x00\x00\x22\x00\x00\x00\x20\x00\x21\x00\x22\x00\x14\x00\x15\x00\x16\x00\x03\x00\x18\x00\x2e\x00\x1a\x00\x1b\x00\x20\x00\x02\x00\x22\x00\x1f\x00\x20\x00\x21\x00\x01\x00\x23\x00\x24\x00\x02\x00\x05\x00\x06\x00\x07\x00\x29\x00\x1c\x00\x01\x00\x0b\x00\x29\x00\x01\x00\x0e\x00\x0f\x00\x03\x00\x02\x00\x06\x00\x07\x00\x12\x00\x22\x00\x0d\x00\x0b\x00\x01\x00\x03\x00\x0e\x00\x0f\x00\x16\x00\x06\x00\x07\x00\x03\x00\x01\x00\x12\x00\x0b\x00\x03\x00\x24\x00\x0e\x00\x0f\x00\x01\x00\x03\x00\x11\x00\x02\x00\x13\x00\x06\x00\x20\x00\x03\x00\x22\x00\x24\x00\x0b\x00\x01\x00\x1f\x00\x0e\x00\x0f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x12\x00\x24\x00\x20\x00\x21\x00\x22\x00\x23\x00\x20\x00\x21\x00\x22\x00\x23\x00\x29\x00\x20\x00\x21\x00\x22\x00\x23\x00\x2e\x00\x24\x00\x20\x00\x1f\x00\x22\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x02\x00\x01\x00\x01\x00\x17\x00\x18\x00\x19\x00\x01\x00\x1b\x00\x17\x00\x18\x00\x19\x00\x00\x00\x1b\x00\x17\x00\x18\x00\x19\x00\x12\x00\x1b\x00\x0d\x00\x01\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x19\x00\x1a\x00\x00\x00\x10\x00\x01\x00\x19\x00\x1a\x00\x14\x00\x17\x00\x18\x00\x19\x00\x00\x00\x1f\x00\x17\x00\x18\x00\x19\x00\x0d\x00\x0a\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1d\x00\x1e\x00\x0a\x00\x01\x00\x0a\x00\x16\x00\x11\x00\x01\x00\x17\x00\x18\x00\x19\x00\x01\x00\x10\x00\x0b\x00\x18\x00\x19\x00\x0e\x00\x0f\x00\x01\x00\x0d\x00\x19\x00\x1d\x00\x1e\x00\x1d\x00\x1e\x00\x0f\x00\x10\x00\x01\x00\x0b\x00\x10\x00\x14\x00\x0e\x00\x0f\x00\x14\x00\x08\x00\x09\x00\x01\x00\x0b\x00\x05\x00\x0a\x00\x0e\x00\x0f\x00\x09\x00\x01\x00\x09\x00\x01\x00\x0b\x00\x01\x00\x09\x00\x0e\x00\x0f\x00\x09\x00\x01\x00\x0b\x00\x00\x00\x01\x00\x0e\x00\x0f\x00\x0e\x00\x0f\x00\x0e\x00\x0f\x00\x00\x00\x01\x00\x0d\x00\x01\x00\x0c\x00\x0d\x00\x01\x00\x11\x00\x22\x00\x13\x00\x05\x00\x25\x00\x0c\x00\x0d\x00\x28\x00\x0d\x00\x06\x00\x0e\x00\x0f\x00\x10\x00\x2a\x00\x05\x00\x0a\x00\x09\x00\x04\x00\x28\x00\x04\x00\x06\x00\x09\x00\x06\x00\x06\x00\x12\x00\x06\x00\x28\x00\x09\x00\x05\x00\x2a\x00\x06\x00\x2a\x00\x06\x00\x08\x00\x28\x00\x03\x00\x2e\x00\x28\x00\x04\x00\x01\x00\x29\x00\x29\x00\x29\x00\x02\x00\x02\x00\x01\x00\x03\x00\x24\x00\x2e\x00\x0a\x00\x2e\x00\x29\x00\x1e\x00\x06\x00\x2e\x00\x0a\x00\x05\x00\x08\x00\x01\x00\xff\xff\x0a\x00\x29\x00\x29\x00\x0a\x00\x0c\x00\xff\xff\x0c\x00\x28\x00\x0a\x00\x29\x00\x05\x00\x05\x00\x08\x00\x2a\x00\xff\xff\x2e\x00\x2e\x00\x08\x00\xff\xff\x2e\x00\xff\xff\x2e\x00\x2e\x00\x2e\x00\xff\xff\xff\xff\x2e\x00\xff\xff\x28\x00\xff\xff\x1e\x00\xff\xff\x2e\x00\xff\xff\xff\xff\xff\xff\x28\x00\xff\xff\xff\xff\xff\xff\x29\x00\x2e\x00\x28\x00\x2a\x00\x29\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x2b\x00\x69\x00\x53\x00\x2c\x00\xe0\x00\x22\x00\x55\x00\xe8\x00\x49\x00\xda\x00\xc9\x00\x4a\x00\x2d\x00\x62\x00\x2e\x00\x63\x00\x64\x00\x69\x00\x2f\x00\x22\x00\x30\x00\x69\x00\xa6\x00\x31\x00\x32\x00\xb6\xff\x22\x00\xb6\xff\xdb\x00\xb6\xff\xb6\xff\xb1\x00\x8b\x00\x33\x00\x25\x00\x22\x00\x22\x00\x25\x00\x34\x00\x39\x00\xd7\x00\xff\xff\xff\xff\x22\x00\x25\x00\x4b\x00\x34\x00\x4c\x00\x2b\x00\x25\x00\xe5\x00\x2c\x00\x22\x00\x25\x00\x87\x00\x2b\x00\xb8\x00\x25\x00\x2c\x00\x3a\x00\x2d\x00\x96\x00\x2e\x00\xb6\xff\x3d\x00\xff\xff\x2f\x00\x2d\x00\x30\x00\x2e\x00\x23\x00\x31\x00\x32\x00\x2f\x00\x58\x00\x30\x00\xc3\x00\x5f\x00\x31\x00\x32\x00\x62\x00\x33\x00\x63\x00\x64\x00\x3d\x00\x3d\x00\x34\x00\x62\x00\x33\x00\x63\x00\x9f\x00\x60\x00\xbe\x00\x34\x00\x25\x00\x6f\x00\x70\x00\x71\x00\x25\x00\x72\x00\x53\x00\x73\x00\x74\x00\xb2\x00\xb3\x00\xb4\x00\x75\x00\x76\x00\x77\x00\x25\x00\x78\x00\x79\x00\xd0\x00\x92\x00\x84\x00\x62\x00\x25\x00\x63\x00\x64\x00\x62\x00\x54\x00\x63\x00\x64\x00\x25\x00\x3d\x00\xaf\x00\xcd\x00\x26\x00\x35\x00\x28\x00\x6f\x00\x70\x00\x71\x00\x25\x00\x72\x00\xff\xff\x73\x00\x74\x00\xb8\x00\xcc\x00\x28\x00\x75\x00\x76\x00\x77\x00\x7e\x00\x78\x00\x79\x00\x5b\x00\x85\x00\x7f\x00\x86\x00\x25\x00\x3e\x00\x66\x00\x81\x00\x25\x00\x7e\x00\x6d\x00\x65\x00\x25\x00\x5b\x00\x7f\x00\xd8\x00\xea\x00\x34\x00\xde\x00\x81\x00\x7e\x00\x25\x00\x6d\x00\x65\x00\xce\x00\x7f\x00\x80\x00\x25\x00\xd1\x00\xc6\x00\x81\x00\x25\x00\x82\x00\x6d\x00\x65\x00\x7e\x00\x25\x00\x58\x00\x5b\x00\xc4\x00\x84\x00\xbc\x00\x25\x00\x28\x00\x82\x00\x81\x00\x8d\x00\x8e\x00\x6d\x00\x65\x00\x26\x00\x27\x00\x28\x00\xdd\x00\xa2\x00\x82\x00\x26\x00\x27\x00\x28\x00\xd5\x00\x26\x00\x27\x00\x28\x00\xb5\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\xff\xff\x82\x00\x36\x00\x8f\x00\x28\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x4c\x00\x41\x00\x42\x00\x43\x00\x5b\x00\x90\x00\x97\x00\x44\x00\x45\x00\x46\x00\x66\x00\xbf\x00\x44\x00\x45\x00\x46\x00\x98\x00\xa9\x00\x44\x00\x45\x00\x46\x00\x5c\x00\x47\x00\xcb\x00\x99\x00\x4d\x00\xa7\x00\x3f\x00\x4c\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x4c\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x4d\x00\xac\x00\xb4\x00\x55\x00\x66\x00\x4d\x00\x4e\x00\xa5\x00\xe6\x00\x45\x00\x46\x00\xb6\x00\x37\x00\xc0\x00\x45\x00\x46\x00\xd3\x00\x39\x00\xa8\x00\x45\x00\x46\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x3f\x00\x4c\x00\x41\x00\x42\x00\x43\x00\xdc\x00\x3c\x00\x39\x00\x5f\x00\x39\x00\x52\x00\x5d\x00\x66\x00\x51\x00\x45\x00\x46\x00\x5f\x00\x5e\x00\xd9\x00\x50\x00\x46\x00\x6d\x00\x65\x00\x5f\x00\xd4\x00\x4f\x00\xbd\x00\x3c\x00\x3b\x00\x3c\x00\x9c\x00\x55\x00\x5f\x00\x6c\x00\x55\x00\x9d\x00\x6d\x00\x65\x00\x56\x00\x7c\x00\x7d\x00\x5f\x00\x7b\x00\xc8\x00\x79\x00\x6d\x00\x65\x00\xc9\x00\x5f\x00\xd6\x00\x5f\x00\x7b\x00\x5f\x00\xed\x00\x6d\x00\x65\x00\x7a\x00\x66\x00\x7b\x00\x69\x00\x66\x00\x6d\x00\x65\x00\x95\x00\x65\x00\x64\x00\x65\x00\x69\x00\x66\x00\x9b\x00\x66\x00\x8c\x00\x6b\x00\xa0\x00\x58\x00\x94\x00\x59\x00\xa5\x00\x95\x00\x6a\x00\x6b\x00\x22\x00\x67\x00\xec\x00\xb2\x00\xb3\x00\xb4\x00\x4b\x00\xea\x00\xe4\x00\xe5\x00\xe8\x00\x22\x00\xe1\x00\xe2\x00\xbb\x00\xe3\x00\xba\x00\xbc\x00\xc2\x00\x22\x00\xc3\x00\x5b\x00\x4b\x00\xc6\x00\x4b\x00\xcb\x00\x58\x00\x22\x00\xd0\x00\xff\xff\x22\x00\xd3\x00\x89\x00\x25\x00\x25\x00\x25\x00\x8a\x00\x8c\x00\xa0\x00\x9b\x00\x92\x00\xff\xff\xa2\x00\xff\xff\x25\x00\x39\x00\xa1\x00\xff\xff\xa4\x00\xa5\x00\x58\x00\xab\x00\x00\x00\xac\x00\x25\x00\x25\x00\xae\x00\xaf\x00\x00\x00\xb1\x00\x22\x00\x8b\x00\x25\x00\x5b\x00\x5b\x00\x58\x00\x4b\x00\x00\x00\xff\xff\xff\xff\x58\x00\x00\x00\xff\xff\x00\x00\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\xff\xff\x00\x00\x22\x00\x00\x00\x39\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x25\x00\xff\xff\x22\x00\x4b\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (32, 134) [
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134)
	]

happy_n_terms = 47 :: Int
happy_n_nonterms = 37 :: Int

happyReduce_32 = happySpecReduce_1 0# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_33 = happySpecReduce_1 1# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn36
		 (Ident happy_var_1
	)}

happyReduce_34 = happySpecReduce_1 2# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn37
		 ((read happy_var_1) :: Integer
	)}

happyReduce_35 = happySpecReduce_1 3# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TC happy_var_1)) -> 
	happyIn38
		 ((read happy_var_1) :: Char
	)}

happyReduce_36 = happySpecReduce_1 4# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn39
		 ((read happy_var_1) :: Double
	)}

happyReduce_37 = happySpecReduce_1 5# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (LGr happy_var_1
	)}

happyReduce_38 = happySpecReduce_1 6# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (DefAll happy_var_1
	)}

happyReduce_39 = happySpecReduce_3 6# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (DefSome happy_var_1 happy_var_3
	)}}

happyReduce_40 = happySpecReduce_2 6# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (LDefView happy_var_2
	)}

happyReduce_41 = happySpecReduce_0 7# happyReduction_41
happyReduction_41  =  happyIn42
		 ([]
	)

happyReduce_42 = happySpecReduce_1 7# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 ((:[]) happy_var_1
	)}

happyReduce_43 = happySpecReduce_3 7# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_44 = happySpecReduce_1 8# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (Grammar happy_var_1
	)}

happyReduce_45 = happySpecReduce_0 9# happyReduction_45
happyReduction_45  =  happyIn44
		 ([]
	)

happyReduce_46 = happySpecReduce_1 9# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((:[]) happy_var_1
	)}

happyReduce_47 = happySpecReduce_3 9# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_0 10# happyReduction_48
happyReduction_48  =  happyIn45
		 ([]
	)

happyReduce_49 = happySpecReduce_2 10# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_50 = happyReduce 5# 11# happyReduction_50
happyReduction_50 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	case happyOut45 happy_x_5 of { happy_var_5 -> 
	happyIn46
		 (Rule happy_var_1 happy_var_3 (reverse happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_51 = happySpecReduce_2 11# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (Comment happy_var_2
	)}

happyReduce_52 = happySpecReduce_3 11# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (Comments happy_var_2 happy_var_3
	)}}

happyReduce_53 = happyReduce 6# 11# happyReduction_53
happyReduction_53 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	case happyOut45 happy_x_6 of { happy_var_6 -> 
	happyIn46
		 (Internal happy_var_2 happy_var_4 (reverse happy_var_6)
	) `HappyStk` happyRest}}}

happyReduce_54 = happySpecReduce_3 11# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut70 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (Token happy_var_2 happy_var_3
	)}}

happyReduce_55 = happyReduce 4# 11# happyReduction_55
happyReduction_55 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_3 of { happy_var_3 -> 
	case happyOut70 happy_x_4 of { happy_var_4 -> 
	happyIn46
		 (PosToken happy_var_3 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_56 = happySpecReduce_2 11# happyReduction_56
happyReduction_56 happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (Entryp happy_var_2
	)}

happyReduce_57 = happyReduce 4# 11# happyReduction_57
happyReduction_57 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut66 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	happyIn46
		 (Separator happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_58 = happyReduce 4# 11# happyReduction_58
happyReduction_58 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut66 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	happyIn46
		 (Terminator happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_59 = happySpecReduce_3 11# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (Coercions happy_var_2 happy_var_3
	)}}

happyReduce_60 = happyReduce 4# 11# happyReduction_60
happyReduction_60 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut64 happy_x_4 of { happy_var_4 -> 
	happyIn46
		 (Rules happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_61 = happyReduce 5# 11# happyReduction_61
happyReduction_61 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	case happyOut58 happy_x_5 of { happy_var_5 -> 
	happyIn46
		 (Function happy_var_2 (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_62 = happySpecReduce_2 11# happyReduction_62
happyReduction_62 happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (Layout happy_var_2
	)}

happyReduce_63 = happySpecReduce_3 11# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (LayoutStop happy_var_3
	)}

happyReduce_64 = happySpecReduce_2 11# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  happyIn46
		 (LayoutTop
	)

happyReduce_65 = happySpecReduce_1 12# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (Terminal happy_var_1
	)}

happyReduce_66 = happySpecReduce_1 12# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (NTerminal happy_var_1
	)}

happyReduce_67 = happySpecReduce_3 13# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_2 of { happy_var_2 -> 
	happyIn48
		 (ListCat happy_var_2
	)}

happyReduce_68 = happySpecReduce_1 13# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (IdCat happy_var_1
	)}

happyReduce_69 = happySpecReduce_1 14# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (LabNoP happy_var_1
	)}

happyReduce_70 = happySpecReduce_2 14# happyReduction_70
happyReduction_70 happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_2 of { happy_var_2 -> 
	happyIn49
		 (LabP happy_var_1 happy_var_2
	)}}

happyReduce_71 = happySpecReduce_3 14# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_2 of { happy_var_2 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (LabPF happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_72 = happySpecReduce_2 14# happyReduction_72
happyReduction_72 happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_2 of { happy_var_2 -> 
	happyIn49
		 (LabF happy_var_1 happy_var_2
	)}}

happyReduce_73 = happySpecReduce_1 15# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (Id happy_var_1
	)}

happyReduce_74 = happySpecReduce_1 15# happyReduction_74
happyReduction_74 happy_x_1
	 =  happyIn50
		 (Wild
	)

happyReduce_75 = happySpecReduce_2 15# happyReduction_75
happyReduction_75 happy_x_2
	happy_x_1
	 =  happyIn50
		 (ListE
	)

happyReduce_76 = happySpecReduce_3 15# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn50
		 (ListCons
	)

happyReduce_77 = happyReduce 5# 15# happyReduction_77
happyReduction_77 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyIn50
		 (ListOne
	) `HappyStk` happyRest

happyReduce_78 = happyReduce 9# 16# happyReduction_78
happyReduction_78 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_3 of { happy_var_3 -> 
	case happyOut53 happy_x_7 of { happy_var_7 -> 
	happyIn51
		 (ProfIt happy_var_3 happy_var_7
	) `HappyStk` happyRest}}

happyReduce_79 = happySpecReduce_3 17# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_2 of { happy_var_2 -> 
	happyIn52
		 (Ints happy_var_2
	)}

happyReduce_80 = happySpecReduce_0 18# happyReduction_80
happyReduction_80  =  happyIn53
		 ([]
	)

happyReduce_81 = happySpecReduce_1 18# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 ((:[]) happy_var_1
	)}

happyReduce_82 = happySpecReduce_3 18# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_83 = happySpecReduce_0 19# happyReduction_83
happyReduction_83  =  happyIn54
		 ([]
	)

happyReduce_84 = happySpecReduce_1 19# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 ((:[]) happy_var_1
	)}

happyReduce_85 = happySpecReduce_3 19# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_86 = happySpecReduce_1 20# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 ((:[]) happy_var_1
	)}

happyReduce_87 = happySpecReduce_2 20# happyReduction_87
happyReduction_87 happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_88 = happySpecReduce_1 21# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (Arg happy_var_1
	)}

happyReduce_89 = happySpecReduce_0 22# happyReduction_89
happyReduction_89  =  happyIn57
		 ([]
	)

happyReduce_90 = happySpecReduce_2 22# happyReduction_90
happyReduction_90 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	happyIn57
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_91 = happySpecReduce_3 23# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_3 of { happy_var_3 -> 
	happyIn58
		 (Cons happy_var_1 happy_var_3
	)}}

happyReduce_92 = happySpecReduce_1 23# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (happy_var_1
	)}

happyReduce_93 = happySpecReduce_2 24# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut61 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (App happy_var_1 happy_var_2
	)}}

happyReduce_94 = happySpecReduce_1 24# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (happy_var_1
	)}

happyReduce_95 = happySpecReduce_1 25# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (Var happy_var_1
	)}

happyReduce_96 = happySpecReduce_1 25# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (LitInt happy_var_1
	)}

happyReduce_97 = happySpecReduce_1 25# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (LitChar happy_var_1
	)}

happyReduce_98 = happySpecReduce_1 25# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (LitString happy_var_1
	)}

happyReduce_99 = happySpecReduce_1 25# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (LitDouble happy_var_1
	)}

happyReduce_100 = happySpecReduce_3 25# happyReduction_100
happyReduction_100 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_2 of { happy_var_2 -> 
	happyIn60
		 (List happy_var_2
	)}

happyReduce_101 = happySpecReduce_3 25# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn60
		 (happy_var_2
	)}

happyReduce_102 = happySpecReduce_1 26# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 ((:[]) happy_var_1
	)}

happyReduce_103 = happySpecReduce_2 26# happyReduction_103
happyReduction_103 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut61 happy_x_2 of { happy_var_2 -> 
	happyIn61
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_104 = happySpecReduce_0 27# happyReduction_104
happyReduction_104  =  happyIn62
		 ([]
	)

happyReduce_105 = happySpecReduce_1 27# happyReduction_105
happyReduction_105 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn62
		 ((:[]) happy_var_1
	)}

happyReduce_106 = happySpecReduce_3 27# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn62
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_107 = happySpecReduce_1 28# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn63
		 ((:[]) happy_var_1
	)}

happyReduce_108 = happySpecReduce_3 28# happyReduction_108
happyReduction_108 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut63 happy_x_3 of { happy_var_3 -> 
	happyIn63
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_109 = happySpecReduce_1 29# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut65 happy_x_1 of { happy_var_1 -> 
	happyIn64
		 ((:[]) happy_var_1
	)}

happyReduce_110 = happySpecReduce_3 29# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { happy_var_1 -> 
	case happyOut64 happy_x_3 of { happy_var_3 -> 
	happyIn64
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_111 = happySpecReduce_1 30# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn65
		 (RHS (reverse happy_var_1)
	)}

happyReduce_112 = happySpecReduce_1 31# happyReduction_112
happyReduction_112 happy_x_1
	 =  happyIn66
		 (MNonempty
	)

happyReduce_113 = happySpecReduce_0 31# happyReduction_113
happyReduction_113  =  happyIn66
		 (MEmpty
	)

happyReduce_114 = happySpecReduce_2 32# happyReduction_114
happyReduction_114 happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_2 of { happy_var_2 -> 
	happyIn67
		 (RSeq happy_var_1 happy_var_2
	)}}

happyReduce_115 = happySpecReduce_1 32# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut69 happy_x_1 of { happy_var_1 -> 
	happyIn67
		 (happy_var_1
	)}

happyReduce_116 = happySpecReduce_3 33# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut67 happy_x_3 of { happy_var_3 -> 
	happyIn68
		 (RAlt happy_var_1 happy_var_3
	)}}

happyReduce_117 = happySpecReduce_3 33# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { happy_var_1 -> 
	case happyOut67 happy_x_3 of { happy_var_3 -> 
	happyIn68
		 (RMinus happy_var_1 happy_var_3
	)}}

happyReduce_118 = happySpecReduce_1 33# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut67 happy_x_1 of { happy_var_1 -> 
	happyIn68
		 (happy_var_1
	)}

happyReduce_119 = happySpecReduce_2 34# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_1 of { happy_var_1 -> 
	happyIn69
		 (RStar happy_var_1
	)}

happyReduce_120 = happySpecReduce_2 34# happyReduction_120
happyReduction_120 happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_1 of { happy_var_1 -> 
	happyIn69
		 (RPlus happy_var_1
	)}

happyReduce_121 = happySpecReduce_2 34# happyReduction_121
happyReduction_121 happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_1 of { happy_var_1 -> 
	happyIn69
		 (ROpt happy_var_1
	)}

happyReduce_122 = happySpecReduce_1 34# happyReduction_122
happyReduction_122 happy_x_1
	 =  happyIn69
		 (REps
	)

happyReduce_123 = happySpecReduce_1 34# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn69
		 (RChar happy_var_1
	)}

happyReduce_124 = happySpecReduce_3 34# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn69
		 (RAlts happy_var_2
	)}

happyReduce_125 = happySpecReduce_3 34# happyReduction_125
happyReduction_125 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn69
		 (RSeqs happy_var_2
	)}

happyReduce_126 = happySpecReduce_1 34# happyReduction_126
happyReduction_126 happy_x_1
	 =  happyIn69
		 (RDigit
	)

happyReduce_127 = happySpecReduce_1 34# happyReduction_127
happyReduction_127 happy_x_1
	 =  happyIn69
		 (RLetter
	)

happyReduce_128 = happySpecReduce_1 34# happyReduction_128
happyReduction_128 happy_x_1
	 =  happyIn69
		 (RUpper
	)

happyReduce_129 = happySpecReduce_1 34# happyReduction_129
happyReduction_129 happy_x_1
	 =  happyIn69
		 (RLower
	)

happyReduce_130 = happySpecReduce_1 34# happyReduction_130
happyReduction_130 happy_x_1
	 =  happyIn69
		 (RAny
	)

happyReduce_131 = happySpecReduce_3 34# happyReduction_131
happyReduction_131 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut70 happy_x_2 of { happy_var_2 -> 
	happyIn69
		 (happy_var_2
	)}

happyReduce_132 = happySpecReduce_1 35# happyReduction_132
happyReduction_132 happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	happyIn70
		 (happy_var_1
	)}

happyReduce_133 = happySpecReduce_1 36# happyReduction_133
happyReduction_133 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn71
		 ((:[]) happy_var_1
	)}

happyReduce_134 = happySpecReduce_3 36# happyReduction_134
happyReduction_134 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut71 happy_x_3 of { happy_var_3 -> 
	happyIn71
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 46# (error "reading EOF!") action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS ":") -> cont 1#;
	PT _ (TS ";") -> cont 2#;
	PT _ (TS ".") -> cont 3#;
	PT _ (TS "::=") -> cont 4#;
	PT _ (TS "[") -> cont 5#;
	PT _ (TS "]") -> cont 6#;
	PT _ (TS "_") -> cont 7#;
	PT _ (TS "(") -> cont 8#;
	PT _ (TS ")") -> cont 9#;
	PT _ (TS ",") -> cont 10#;
	PT _ (TS "=") -> cont 11#;
	PT _ (TS "|") -> cont 12#;
	PT _ (TS "-") -> cont 13#;
	PT _ (TS "*") -> cont 14#;
	PT _ (TS "+") -> cont 15#;
	PT _ (TS "?") -> cont 16#;
	PT _ (TS "{") -> cont 17#;
	PT _ (TS "}") -> cont 18#;
	PT _ (TS "char") -> cont 19#;
	PT _ (TS "coercions") -> cont 20#;
	PT _ (TS "comment") -> cont 21#;
	PT _ (TS "define") -> cont 22#;
	PT _ (TS "digit") -> cont 23#;
	PT _ (TS "entrypoints") -> cont 24#;
	PT _ (TS "eps") -> cont 25#;
	PT _ (TS "internal") -> cont 26#;
	PT _ (TS "layout") -> cont 27#;
	PT _ (TS "letter") -> cont 28#;
	PT _ (TS "lower") -> cont 29#;
	PT _ (TS "nonempty") -> cont 30#;
	PT _ (TS "position") -> cont 31#;
	PT _ (TS "rules") -> cont 32#;
	PT _ (TS "separator") -> cont 33#;
	PT _ (TS "stop") -> cont 34#;
	PT _ (TS "terminator") -> cont 35#;
	PT _ (TS "token") -> cont 36#;
	PT _ (TS "toplevel") -> cont 37#;
	PT _ (TS "upper") -> cont 38#;
	PT _ (TS "views") -> cont 39#;
	PT _ (TL happy_dollar_dollar) -> cont 40#;
	PT _ (TV happy_dollar_dollar) -> cont 41#;
	PT _ (TI happy_dollar_dollar) -> cont 42#;
	PT _ (TC happy_dollar_dollar) -> cont 43#;
	PT _ (TD happy_dollar_dollar) -> cont 44#;
	_ -> cont 45#;
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
happyError' :: () => [Token] -> Err a
happyError' = happyError

pLGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut40 x))

pLDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut41 x))

pListLDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut42 x))

pGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut43 x))

pListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut44 x))

pListItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut45 x))

pDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut46 x))

pItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut47 x))

pCat tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut48 x))

pLabel tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut49 x))

pLabelId tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut50 x))

pProfItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut51 x))

pIntList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut52 x))

pListInteger tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut53 x))

pListIntList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut54 x))

pListProfItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut55 x))

pArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut56 x))

pListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut57 x))

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut58 x))

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut59 x))

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut60 x))

pListExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut61 x))

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut62 x))

pListString tks = happySomeParser where
  happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut63 x))

pListRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (happyOut64 x))

pRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (happyOut65 x))

pMinimumSize tks = happySomeParser where
  happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (happyOut66 x))

pReg2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (happyOut67 x))

pReg1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (happyOut68 x))

pReg3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (happyOut69 x))

pReg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (happyOut70 x))

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse 31# tks) (\x -> happyReturn (happyOut71 x))

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
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id: ParBNF.hs,v 1.14 2007/01/08 17:54:31 aarne Exp $


{-# LINE 28 "GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList






{-# LINE 49 "GenericTemplate.hs" #-}


{-# LINE 59 "GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st











indexShortOffAddr (HappyA# arr) off =
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





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
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


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
