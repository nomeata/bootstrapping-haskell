-- parser produced by Happy Version 1.5


module ParseIface ( parseIface, IfaceStuff(..) ) where

#include "HsVersions.h"

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsDecls		( HsIdInfo(..), HsStrictnessInfo(..) )
import HsTypes		( mkHsForAllTy )
import HsCore
import Literal
import BasicTypes	( IfaceFlavour(..), Fixity(..), FixityDirection(..), NewOrData(..), Version )
import HsPragmas	( noDataPragmas, noClassPragmas )
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind, mkTypeKind )
import IdInfo           ( ArgUsageInfo, FBTypeInfo, ArityInfo, exactArity )
import PrimRep		( decodePrimRep )
import Lex		

import RnMonad		( ImportVersion, LocalVersion, ParsedIface(..), WhatsImported(..),
			  RdrNamePragma, ExportItem, RdrAvailInfo, GenAvailInfo(..)
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import Name		( OccName(..), isTCOcc, Provenance, SYN_IE(Module) )
import SrcLoc		( SrcLoc )
import Maybes
import Outputable


data HappyAbsSyn 
	= HappyTerminal IfaceToken
	| HappyErrorToken Int
	| HappyAbsSyn1(IfaceStuff)
	| HappyAbsSyn2(ParsedIface)
	| HappyAbsSyn3([ImportVersion OccName])
	| HappyAbsSyn5(ImportVersion OccName)
	| HappyAbsSyn6(WhatsImported OccName)
	| HappyAbsSyn7([LocalVersion OccName])
	| HappyAbsSyn9(LocalVersion OccName)
	| HappyAbsSyn10([ExportItem])
	| HappyAbsSyn12(IfaceFlavour)
	| HappyAbsSyn13([RdrAvailInfo])
	| HappyAbsSyn14(RdrAvailInfo)
	| HappyAbsSyn15([OccName])
	| HappyAbsSyn16([Module])
	| HappyAbsSyn18([(OccName,Fixity)])
	| HappyAbsSyn20((OccName, Fixity))
	| HappyAbsSyn21([(Version, RdrNameHsDecl)])
	| HappyAbsSyn23(Version)
	| HappyAbsSyn24(RdrNameHsDecl)
	| HappyAbsSyn25(RdrNameContext)
	| HappyAbsSyn26([RdrNameSig])
	| HappyAbsSyn28(RdrNameSig)
	| HappyAbsSyn29([RdrNameConDecl] {- empty for handwritten abstract -})
	| HappyAbsSyn30([RdrNameConDecl])
	| HappyAbsSyn31(RdrNameConDecl)
	| HappyAbsSyn32([RdrNameConDecl] {- Empty if handwritten abstract -})
	| HappyAbsSyn33(Maybe [RdrName])
	| HappyAbsSyn34([RdrNameBangType])
	| HappyAbsSyn35(RdrNameBangType)
	| HappyAbsSyn36([([RdrName], RdrNameBangType)])
	| HappyAbsSyn37(([RdrName], RdrNameBangType))
	| HappyAbsSyn38(RdrNameHsType)
	| HappyAbsSyn39([HsTyVar RdrName])
	| HappyAbsSyn42((RdrName, [RdrNameHsType]))
	| HappyAbsSyn43([RdrNameHsType] 			{- Two or more -})
	| HappyAbsSyn46([RdrNameHsType] 	{-  Zero or more -})
	| HappyAbsSyn47(Module)
	| HappyAbsSyn48(OccName)
	| HappyAbsSyn53(RdrName)
	| HappyAbsSyn55([RdrName])
	| HappyAbsSyn64(HsTyVar RdrName)
	| HappyAbsSyn66(Kind)
	| HappyAbsSyn68([RdrNameInstDecl])
	| HappyAbsSyn70(RdrNameInstDecl)
	| HappyAbsSyn71([HsIdInfo RdrName])
	| HappyAbsSyn72(HsIdInfo RdrName)
	| HappyAbsSyn74(ArityInfo)
	| HappyAbsSyn75(HsStrictnessInfo RdrName)
	| HappyAbsSyn76(UfExpr RdrName)
	| HappyAbsSyn77([(UfBinder RdrName, UfExpr RdrName)])
	| HappyAbsSyn78([(Literal,UfExpr RdrName)])
	| HappyAbsSyn79([(RdrName, [RdrName], UfExpr RdrName)])
	| HappyAbsSyn80(UfDefault RdrName)
	| HappyAbsSyn81(UfArg RdrName)
	| HappyAbsSyn82([UfArg RdrName])
	| HappyAbsSyn84(Literal)
	| HappyAbsSyn85(UfBinder RdrName)
	| HappyAbsSyn86([UfBinder RdrName])
	| HappyAbsSyn89(FAST_STRING)
	| HappyAbsSyn90(Char)
	| HappyAbsSyn91(SrcLoc)
	| HappyAbsSyn92(())

type HappyReduction = 
	   Int# 
	-> (IfaceToken)
	-> HappyState (IfaceToken) ([HappyAbsSyn] -> IfM(IfaceStuff))
	-> [HappyState (IfaceToken) ([HappyAbsSyn] -> IfM(IfaceStuff))] 
	-> [HappyAbsSyn] 
	-> IfM(IfaceStuff)

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
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340,
 action_341,
 action_342,
 action_343,
 action_344,
 action_345,
 action_346,
 action_347,
 action_348,
 action_349,
 action_350,
 action_351,
 action_352,
 action_353,
 action_354,
 action_355,
 action_356,
 action_357,
 action_358,
 action_359,
 action_360,
 action_361,
 action_362,
 action_363,
 action_364,
 action_365,
 action_366,
 action_367,
 action_368,
 action_369,
 action_370,
 action_371,
 action_372,
 action_373,
 action_374,
 action_375,
 action_376,
 action_377,
 action_378,
 action_379,
 action_380,
 action_381,
 action_382,
 action_383,
 action_384,
 action_385,
 action_386,
 action_387,
 action_388,
 action_389,
 action_390,
 action_391,
 action_392,
 action_393,
 action_394,
 action_395,
 action_396,
 action_397,
 action_398,
 action_399,
 action_400,
 action_401,
 action_402 :: Int# -> HappyReduction

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
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
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
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176,
 happyReduce_177,
 happyReduce_178,
 happyReduce_179,
 happyReduce_180,
 happyReduce_181,
 happyReduce_182,
 happyReduce_183,
 happyReduce_184,
 happyReduce_185,
 happyReduce_186,
 happyReduce_187,
 happyReduce_188,
 happyReduce_189,
 happyReduce_190,
 happyReduce_191,
 happyReduce_192,
 happyReduce_193,
 happyReduce_194,
 happyReduce_195,
 happyReduce_196,
 happyReduce_197,
 happyReduce_198,
 happyReduce_199,
 happyReduce_200,
 happyReduce_201,
 happyReduce_202,
 happyReduce_203,
 happyReduce_204,
 happyReduce_205,
 happyReduce_206,
 happyReduce_207,
 happyReduce_208,
 happyReduce_209,
 happyReduce_210,
 happyReduce_211,
 happyReduce_212,
 happyReduce_213,
 happyReduce_214,
 happyReduce_215,
 happyReduce_216,
 happyReduce_217 :: HappyReduction

action_0 (93#) = happyShift action_12
action_0 (112#) = happyShift action_13
action_0 (120#) = happyShift action_14
action_0 (121#) = happyShift action_15
action_0 (122#) = happyShift action_16
action_0 (128#) = happyShift action_17
action_0 (129#) = happyShift action_18
action_0 (130#) = happyShift action_19
action_0 (131#) = happyShift action_20
action_0 (133#) = happyShift action_21
action_0 (135#) = happyShift action_22
action_0 (136#) = happyShift action_23
action_0 (138#) = happyShift action_24
action_0 (139#) = happyShift action_25
action_0 (140#) = happyShift action_26
action_0 (141#) = happyShift action_27
action_0 (1#) = happyGoto action_1
action_0 (2#) = happyGoto action_2
action_0 (38#) = happyGoto action_3
action_0 (44#) = happyGoto action_4
action_0 (45#) = happyGoto action_5
action_0 (49#) = happyGoto action_6
action_0 (60#) = happyGoto action_7
action_0 (62#) = happyGoto action_8
action_0 (71#) = happyGoto action_9
action_0 (72#) = happyGoto action_10
action_0 (75#) = happyGoto action_11
action_0 x = happyTcHack x happyReduce_148

action_1 (166#) = happyAccept
action_1 x = happyTcHack x happyFail

action_2 x = happyTcHack x happyReduce_1

action_3 x = happyTcHack x happyReduce_2

action_4 (120#) = happyShift action_14
action_4 (121#) = happyShift action_15
action_4 (122#) = happyShift action_16
action_4 (123#) = happyShift action_81
action_4 (128#) = happyShift action_17
action_4 (129#) = happyShift action_18
action_4 (130#) = happyShift action_19
action_4 (131#) = happyShift action_20
action_4 (133#) = happyShift action_21
action_4 (135#) = happyShift action_22
action_4 (45#) = happyGoto action_80
action_4 (49#) = happyGoto action_6
action_4 (60#) = happyGoto action_7
action_4 (62#) = happyGoto action_8
action_4 x = happyTcHack x happyReduce_78

action_5 x = happyTcHack x happyReduce_87

action_6 x = happyTcHack x happyReduce_126

action_7 x = happyTcHack x happyReduce_89

action_8 x = happyTcHack x happyReduce_90

action_9 x = happyTcHack x happyReduce_3

action_10 (136#) = happyShift action_23
action_10 (138#) = happyShift action_24
action_10 (139#) = happyShift action_25
action_10 (140#) = happyShift action_26
action_10 (141#) = happyShift action_27
action_10 (71#) = happyGoto action_79
action_10 (72#) = happyGoto action_10
action_10 (75#) = happyGoto action_11
action_10 x = happyTcHack x happyReduce_148

action_11 x = happyTcHack x happyReduce_151

action_12 (129#) = happyShift action_78
action_12 x = happyTcHack x happyFail

action_13 (121#) = happyShift action_77
action_13 (39#) = happyGoto action_76
action_13 x = happyTcHack x happyFail

action_14 (122#) = happyShift action_75
action_14 (129#) = happyShift action_18
action_14 (131#) = happyShift action_20
action_14 (133#) = happyShift action_21
action_14 (135#) = happyShift action_22
action_14 (49#) = happyGoto action_6
action_14 (60#) = happyGoto action_74
action_14 x = happyTcHack x happyFail

action_15 (112#) = happyShift action_13
action_15 (120#) = happyShift action_14
action_15 (121#) = happyShift action_15
action_15 (122#) = happyShift action_16
action_15 (128#) = happyShift action_17
action_15 (129#) = happyShift action_18
action_15 (130#) = happyShift action_19
action_15 (131#) = happyShift action_20
action_15 (133#) = happyShift action_21
action_15 (135#) = happyShift action_22
action_15 (38#) = happyGoto action_73
action_15 (44#) = happyGoto action_4
action_15 (45#) = happyGoto action_5
action_15 (49#) = happyGoto action_6
action_15 (60#) = happyGoto action_7
action_15 (62#) = happyGoto action_8
action_15 x = happyTcHack x happyFail

action_16 (112#) = happyShift action_13
action_16 (120#) = happyShift action_14
action_16 (121#) = happyShift action_15
action_16 (122#) = happyShift action_16
action_16 (123#) = happyShift action_72
action_16 (128#) = happyShift action_17
action_16 (129#) = happyShift action_18
action_16 (130#) = happyShift action_19
action_16 (131#) = happyShift action_20
action_16 (133#) = happyShift action_21
action_16 (135#) = happyShift action_22
action_16 (38#) = happyGoto action_70
action_16 (43#) = happyGoto action_71
action_16 (44#) = happyGoto action_4
action_16 (45#) = happyGoto action_5
action_16 (49#) = happyGoto action_6
action_16 (60#) = happyGoto action_7
action_16 (62#) = happyGoto action_8
action_16 x = happyTcHack x happyFail

action_17 x = happyTcHack x happyReduce_131

action_18 x = happyTcHack x happyReduce_101

action_19 x = happyTcHack x happyReduce_132

action_20 x = happyTcHack x happyReduce_102

action_21 x = happyTcHack x happyReduce_127

action_22 x = happyTcHack x happyReduce_128

action_23 (113#) = happyShift action_37
action_23 (128#) = happyShift action_39
action_23 (130#) = happyShift action_41
action_23 (132#) = happyShift action_43
action_23 (134#) = happyShift action_45
action_23 (48#) = happyGoto action_30
action_23 (53#) = happyGoto action_31
action_23 (54#) = happyGoto action_69
action_23 x = happyTcHack x happyReduce_159

action_24 (157#) = happyShift action_68
action_24 (74#) = happyGoto action_67
action_24 x = happyTcHack x happyFail

action_25 (113#) = happyShift action_37
action_25 (122#) = happyShift action_38
action_25 (128#) = happyShift action_39
action_25 (129#) = happyShift action_40
action_25 (130#) = happyShift action_41
action_25 (131#) = happyShift action_42
action_25 (132#) = happyShift action_43
action_25 (133#) = happyShift action_44
action_25 (134#) = happyShift action_45
action_25 (135#) = happyShift action_46
action_25 (142#) = happyShift action_47
action_25 (143#) = happyShift action_48
action_25 (144#) = happyShift action_49
action_25 (145#) = happyShift action_50
action_25 (146#) = happyShift action_51
action_25 (147#) = happyShift action_52
action_25 (150#) = happyShift action_53
action_25 (152#) = happyShift action_54
action_25 (153#) = happyShift action_55
action_25 (154#) = happyShift action_56
action_25 (155#) = happyShift action_57
action_25 (156#) = happyShift action_58
action_25 (157#) = happyShift action_59
action_25 (158#) = happyShift action_60
action_25 (159#) = happyShift action_61
action_25 (160#) = happyShift action_62
action_25 (161#) = happyShift action_63
action_25 (162#) = happyShift action_64
action_25 (163#) = happyShift action_65
action_25 (164#) = happyShift action_66
action_25 (48#) = happyGoto action_30
action_25 (53#) = happyGoto action_31
action_25 (54#) = happyGoto action_32
action_25 (57#) = happyGoto action_33
action_25 (58#) = happyGoto action_34
action_25 (76#) = happyGoto action_35
action_25 (84#) = happyGoto action_36
action_25 x = happyTcHack x happyFail

action_26 (121#) = happyShift action_29
action_26 (73#) = happyGoto action_28
action_26 x = happyTcHack x happyFail

action_27 x = happyTcHack x happyReduce_152

action_28 (120#) = happyShift action_14
action_28 (121#) = happyShift action_15
action_28 (122#) = happyShift action_16
action_28 (128#) = happyShift action_17
action_28 (129#) = happyShift action_18
action_28 (130#) = happyShift action_19
action_28 (131#) = happyShift action_20
action_28 (133#) = happyShift action_21
action_28 (135#) = happyShift action_22
action_28 (45#) = happyGoto action_89
action_28 (46#) = happyGoto action_130
action_28 (49#) = happyGoto action_6
action_28 (60#) = happyGoto action_7
action_28 (62#) = happyGoto action_8
action_28 x = happyTcHack x happyReduce_95

action_29 (128#) = happyShift action_17
action_29 (130#) = happyShift action_19
action_29 (62#) = happyGoto action_84
action_29 (64#) = happyGoto action_85
action_29 (65#) = happyGoto action_129
action_29 x = happyTcHack x happyReduce_137

action_30 x = happyTcHack x happyReduce_112

action_31 x = happyTcHack x happyReduce_113

action_32 x = happyTcHack x happyReduce_160

action_33 x = happyTcHack x happyReduce_121

action_34 (120#) = happyShift action_128
action_34 x = happyTcHack x happyReduce_161

action_35 (113#) = happyShift action_37
action_35 (128#) = happyShift action_39
action_35 (129#) = happyShift action_40
action_35 (130#) = happyShift action_41
action_35 (131#) = happyShift action_42
action_35 (132#) = happyShift action_43
action_35 (133#) = happyShift action_44
action_35 (134#) = happyShift action_45
action_35 (135#) = happyShift action_46
action_35 (151#) = happyShift action_127
action_35 (155#) = happyShift action_57
action_35 (156#) = happyShift action_58
action_35 (157#) = happyShift action_59
action_35 (158#) = happyShift action_60
action_35 (159#) = happyShift action_61
action_35 (160#) = happyShift action_62
action_35 (161#) = happyShift action_63
action_35 (162#) = happyShift action_64
action_35 (163#) = happyShift action_65
action_35 (164#) = happyShift action_66
action_35 (48#) = happyGoto action_30
action_35 (53#) = happyGoto action_31
action_35 (54#) = happyGoto action_123
action_35 (57#) = happyGoto action_33
action_35 (58#) = happyGoto action_124
action_35 (81#) = happyGoto action_125
action_35 (84#) = happyGoto action_126
action_35 x = happyTcHack x happyReduce_153

action_36 x = happyTcHack x happyReduce_162

action_37 x = happyTcHack x happyReduce_100

action_38 (113#) = happyShift action_37
action_38 (122#) = happyShift action_38
action_38 (128#) = happyShift action_39
action_38 (129#) = happyShift action_40
action_38 (130#) = happyShift action_41
action_38 (131#) = happyShift action_42
action_38 (132#) = happyShift action_43
action_38 (133#) = happyShift action_44
action_38 (134#) = happyShift action_45
action_38 (135#) = happyShift action_46
action_38 (142#) = happyShift action_47
action_38 (143#) = happyShift action_48
action_38 (144#) = happyShift action_49
action_38 (145#) = happyShift action_50
action_38 (146#) = happyShift action_51
action_38 (147#) = happyShift action_52
action_38 (150#) = happyShift action_53
action_38 (152#) = happyShift action_54
action_38 (153#) = happyShift action_55
action_38 (154#) = happyShift action_56
action_38 (155#) = happyShift action_57
action_38 (156#) = happyShift action_58
action_38 (157#) = happyShift action_59
action_38 (158#) = happyShift action_60
action_38 (159#) = happyShift action_61
action_38 (160#) = happyShift action_62
action_38 (161#) = happyShift action_63
action_38 (162#) = happyShift action_64
action_38 (163#) = happyShift action_65
action_38 (164#) = happyShift action_66
action_38 (48#) = happyGoto action_30
action_38 (53#) = happyGoto action_31
action_38 (54#) = happyGoto action_32
action_38 (57#) = happyGoto action_33
action_38 (58#) = happyGoto action_34
action_38 (76#) = happyGoto action_122
action_38 (84#) = happyGoto action_36
action_38 x = happyTcHack x happyFail

action_39 x = happyTcHack x happyReduce_98

action_40 x = happyTcHack x happyReduce_119

action_41 x = happyTcHack x happyReduce_99

action_42 x = happyTcHack x happyReduce_120

action_43 x = happyTcHack x happyReduce_114

action_44 x = happyTcHack x happyReduce_122

action_45 x = happyTcHack x happyReduce_115

action_46 x = happyTcHack x happyReduce_123

action_47 (113#) = happyShift action_37
action_47 (128#) = happyShift action_39
action_47 (130#) = happyShift action_41
action_47 (48#) = happyGoto action_30
action_47 (53#) = happyGoto action_119
action_47 (85#) = happyGoto action_120
action_47 (86#) = happyGoto action_121
action_47 x = happyTcHack x happyReduce_204

action_48 (128#) = happyShift action_17
action_48 (130#) = happyShift action_19
action_48 (62#) = happyGoto action_116
action_48 (87#) = happyGoto action_117
action_48 (88#) = happyGoto action_118
action_48 x = happyTcHack x happyReduce_208

action_49 (113#) = happyShift action_37
action_49 (122#) = happyShift action_38
action_49 (128#) = happyShift action_39
action_49 (129#) = happyShift action_40
action_49 (130#) = happyShift action_41
action_49 (131#) = happyShift action_42
action_49 (132#) = happyShift action_43
action_49 (133#) = happyShift action_44
action_49 (134#) = happyShift action_45
action_49 (135#) = happyShift action_46
action_49 (142#) = happyShift action_47
action_49 (143#) = happyShift action_48
action_49 (144#) = happyShift action_49
action_49 (145#) = happyShift action_50
action_49 (146#) = happyShift action_51
action_49 (147#) = happyShift action_52
action_49 (150#) = happyShift action_53
action_49 (152#) = happyShift action_54
action_49 (153#) = happyShift action_55
action_49 (154#) = happyShift action_56
action_49 (155#) = happyShift action_57
action_49 (156#) = happyShift action_58
action_49 (157#) = happyShift action_59
action_49 (158#) = happyShift action_60
action_49 (159#) = happyShift action_61
action_49 (160#) = happyShift action_62
action_49 (161#) = happyShift action_63
action_49 (162#) = happyShift action_64
action_49 (163#) = happyShift action_65
action_49 (164#) = happyShift action_66
action_49 (48#) = happyGoto action_30
action_49 (53#) = happyGoto action_31
action_49 (54#) = happyGoto action_32
action_49 (57#) = happyGoto action_33
action_49 (58#) = happyGoto action_34
action_49 (76#) = happyGoto action_115
action_49 (84#) = happyGoto action_36
action_49 x = happyTcHack x happyFail

action_50 (113#) = happyShift action_37
action_50 (122#) = happyShift action_38
action_50 (128#) = happyShift action_39
action_50 (129#) = happyShift action_40
action_50 (130#) = happyShift action_41
action_50 (131#) = happyShift action_42
action_50 (132#) = happyShift action_43
action_50 (133#) = happyShift action_44
action_50 (134#) = happyShift action_45
action_50 (135#) = happyShift action_46
action_50 (142#) = happyShift action_47
action_50 (143#) = happyShift action_48
action_50 (144#) = happyShift action_49
action_50 (145#) = happyShift action_50
action_50 (146#) = happyShift action_51
action_50 (147#) = happyShift action_52
action_50 (150#) = happyShift action_53
action_50 (152#) = happyShift action_54
action_50 (153#) = happyShift action_55
action_50 (154#) = happyShift action_56
action_50 (155#) = happyShift action_57
action_50 (156#) = happyShift action_58
action_50 (157#) = happyShift action_59
action_50 (158#) = happyShift action_60
action_50 (159#) = happyShift action_61
action_50 (160#) = happyShift action_62
action_50 (161#) = happyShift action_63
action_50 (162#) = happyShift action_64
action_50 (163#) = happyShift action_65
action_50 (164#) = happyShift action_66
action_50 (48#) = happyGoto action_30
action_50 (53#) = happyGoto action_31
action_50 (54#) = happyGoto action_32
action_50 (57#) = happyGoto action_33
action_50 (58#) = happyGoto action_34
action_50 (76#) = happyGoto action_114
action_50 (84#) = happyGoto action_36
action_50 x = happyTcHack x happyFail

action_51 (120#) = happyShift action_113
action_51 x = happyTcHack x happyFail

action_52 (120#) = happyShift action_112
action_52 x = happyTcHack x happyFail

action_53 (120#) = happyShift action_14
action_53 (121#) = happyShift action_15
action_53 (122#) = happyShift action_16
action_53 (128#) = happyShift action_17
action_53 (129#) = happyShift action_18
action_53 (130#) = happyShift action_19
action_53 (131#) = happyShift action_20
action_53 (133#) = happyShift action_21
action_53 (135#) = happyShift action_22
action_53 (45#) = happyGoto action_111
action_53 (49#) = happyGoto action_6
action_53 (60#) = happyGoto action_7
action_53 (62#) = happyGoto action_8
action_53 x = happyTcHack x happyFail

action_54 (128#) = happyShift action_108
action_54 (129#) = happyShift action_109
action_54 (156#) = happyShift action_110
action_54 (89#) = happyGoto action_107
action_54 x = happyTcHack x happyFail

action_55 (113#) = happyShift action_37
action_55 (122#) = happyShift action_38
action_55 (128#) = happyShift action_39
action_55 (129#) = happyShift action_40
action_55 (130#) = happyShift action_41
action_55 (131#) = happyShift action_42
action_55 (132#) = happyShift action_43
action_55 (133#) = happyShift action_44
action_55 (134#) = happyShift action_45
action_55 (135#) = happyShift action_46
action_55 (142#) = happyShift action_47
action_55 (143#) = happyShift action_48
action_55 (144#) = happyShift action_49
action_55 (145#) = happyShift action_50
action_55 (146#) = happyShift action_51
action_55 (147#) = happyShift action_52
action_55 (150#) = happyShift action_53
action_55 (152#) = happyShift action_54
action_55 (153#) = happyShift action_55
action_55 (154#) = happyShift action_56
action_55 (155#) = happyShift action_57
action_55 (156#) = happyShift action_58
action_55 (157#) = happyShift action_59
action_55 (158#) = happyShift action_60
action_55 (159#) = happyShift action_61
action_55 (160#) = happyShift action_62
action_55 (161#) = happyShift action_63
action_55 (162#) = happyShift action_64
action_55 (163#) = happyShift action_65
action_55 (164#) = happyShift action_66
action_55 (48#) = happyGoto action_30
action_55 (53#) = happyGoto action_31
action_55 (54#) = happyGoto action_32
action_55 (57#) = happyGoto action_33
action_55 (58#) = happyGoto action_34
action_55 (76#) = happyGoto action_106
action_55 (84#) = happyGoto action_36
action_55 x = happyTcHack x happyFail

action_56 (113#) = happyShift action_37
action_56 (122#) = happyShift action_38
action_56 (128#) = happyShift action_39
action_56 (129#) = happyShift action_40
action_56 (130#) = happyShift action_41
action_56 (131#) = happyShift action_42
action_56 (132#) = happyShift action_43
action_56 (133#) = happyShift action_44
action_56 (134#) = happyShift action_45
action_56 (135#) = happyShift action_46
action_56 (142#) = happyShift action_47
action_56 (143#) = happyShift action_48
action_56 (144#) = happyShift action_49
action_56 (145#) = happyShift action_50
action_56 (146#) = happyShift action_51
action_56 (147#) = happyShift action_52
action_56 (150#) = happyShift action_53
action_56 (152#) = happyShift action_54
action_56 (153#) = happyShift action_55
action_56 (154#) = happyShift action_56
action_56 (155#) = happyShift action_57
action_56 (156#) = happyShift action_58
action_56 (157#) = happyShift action_59
action_56 (158#) = happyShift action_60
action_56 (159#) = happyShift action_61
action_56 (160#) = happyShift action_62
action_56 (161#) = happyShift action_63
action_56 (162#) = happyShift action_64
action_56 (163#) = happyShift action_65
action_56 (164#) = happyShift action_66
action_56 (48#) = happyGoto action_30
action_56 (53#) = happyGoto action_31
action_56 (54#) = happyGoto action_32
action_56 (57#) = happyGoto action_33
action_56 (58#) = happyGoto action_34
action_56 (76#) = happyGoto action_105
action_56 (84#) = happyGoto action_36
action_56 x = happyTcHack x happyFail

action_57 x = happyTcHack x happyReduce_194

action_58 x = happyTcHack x happyReduce_195

action_59 x = happyTcHack x happyReduce_193

action_60 x = happyTcHack x happyReduce_197

action_61 (157#) = happyShift action_104
action_61 x = happyTcHack x happyFail

action_62 (158#) = happyShift action_103
action_62 x = happyTcHack x happyFail

action_63 (157#) = happyShift action_102
action_63 x = happyTcHack x happyFail

action_64 (157#) = happyShift action_101
action_64 x = happyTcHack x happyFail

action_65 (128#) = happyShift action_99
action_65 (129#) = happyShift action_100
action_65 (90#) = happyGoto action_98
action_65 x = happyTcHack x happyFail

action_66 (156#) = happyShift action_97
action_66 x = happyTcHack x happyFail

action_67 x = happyTcHack x happyReduce_150

action_68 x = happyTcHack x happyReduce_156

action_69 (120#) = happyShift action_96
action_69 x = happyTcHack x happyReduce_158

action_70 (116#) = happyShift action_94
action_70 (126#) = happyShift action_95
action_70 x = happyTcHack x happyFail

action_71 (126#) = happyShift action_93
action_71 x = happyTcHack x happyFail

action_72 (126#) = happyShift action_92
action_72 x = happyTcHack x happyFail

action_73 (125#) = happyShift action_91
action_73 x = happyTcHack x happyFail

action_74 (120#) = happyShift action_14
action_74 (121#) = happyShift action_15
action_74 (122#) = happyShift action_16
action_74 (128#) = happyShift action_17
action_74 (129#) = happyShift action_18
action_74 (130#) = happyShift action_19
action_74 (131#) = happyShift action_20
action_74 (133#) = happyShift action_21
action_74 (135#) = happyShift action_22
action_74 (45#) = happyGoto action_89
action_74 (46#) = happyGoto action_90
action_74 (49#) = happyGoto action_6
action_74 (60#) = happyGoto action_7
action_74 (62#) = happyGoto action_8
action_74 x = happyTcHack x happyReduce_95

action_75 (123#) = happyShift action_72
action_75 x = happyTcHack x happyFail

action_76 (120#) = happyShift action_88
action_76 (40#) = happyGoto action_87
action_76 x = happyTcHack x happyReduce_80

action_77 (128#) = happyShift action_17
action_77 (130#) = happyShift action_19
action_77 (62#) = happyGoto action_84
action_77 (64#) = happyGoto action_85
action_77 (65#) = happyGoto action_86
action_77 x = happyTcHack x happyReduce_137

action_78 (157#) = happyShift action_83
action_78 x = happyTcHack x happyFail

action_79 x = happyTcHack x happyReduce_149

action_80 x = happyTcHack x happyReduce_88

action_81 (112#) = happyShift action_13
action_81 (120#) = happyShift action_14
action_81 (121#) = happyShift action_15
action_81 (122#) = happyShift action_16
action_81 (128#) = happyShift action_17
action_81 (129#) = happyShift action_18
action_81 (130#) = happyShift action_19
action_81 (131#) = happyShift action_20
action_81 (133#) = happyShift action_21
action_81 (135#) = happyShift action_22
action_81 (38#) = happyGoto action_82
action_81 (44#) = happyGoto action_4
action_81 (45#) = happyGoto action_5
action_81 (49#) = happyGoto action_6
action_81 (60#) = happyGoto action_7
action_81 (62#) = happyGoto action_8
action_81 x = happyTcHack x happyFail

action_82 x = happyTcHack x happyReduce_77

action_83 (157#) = happyShift action_167
action_83 (92#) = happyGoto action_166
action_83 x = happyTcHack x happyReduce_216

action_84 (115#) = happyShift action_165
action_84 x = happyTcHack x happyReduce_136

action_85 (128#) = happyShift action_17
action_85 (130#) = happyShift action_19
action_85 (62#) = happyGoto action_84
action_85 (64#) = happyGoto action_85
action_85 (65#) = happyGoto action_164
action_85 x = happyTcHack x happyReduce_137

action_86 (125#) = happyShift action_163
action_86 x = happyTcHack x happyFail

action_87 (117#) = happyShift action_162
action_87 x = happyTcHack x happyFail

action_88 (122#) = happyShift action_75
action_88 (129#) = happyShift action_18
action_88 (131#) = happyShift action_20
action_88 (133#) = happyShift action_21
action_88 (135#) = happyShift action_22
action_88 (41#) = happyGoto action_159
action_88 (42#) = happyGoto action_160
action_88 (49#) = happyGoto action_6
action_88 (60#) = happyGoto action_161
action_88 x = happyTcHack x happyFail

action_89 (120#) = happyShift action_14
action_89 (121#) = happyShift action_15
action_89 (122#) = happyShift action_16
action_89 (128#) = happyShift action_17
action_89 (129#) = happyShift action_18
action_89 (130#) = happyShift action_19
action_89 (131#) = happyShift action_20
action_89 (133#) = happyShift action_21
action_89 (135#) = happyShift action_22
action_89 (45#) = happyGoto action_89
action_89 (46#) = happyGoto action_158
action_89 (49#) = happyGoto action_6
action_89 (60#) = happyGoto action_7
action_89 (62#) = happyGoto action_8
action_89 x = happyTcHack x happyReduce_95

action_90 (124#) = happyShift action_157
action_90 x = happyTcHack x happyFail

action_91 x = happyTcHack x happyReduce_92

action_92 x = happyTcHack x happyReduce_103

action_93 x = happyTcHack x happyReduce_91

action_94 (112#) = happyShift action_13
action_94 (120#) = happyShift action_14
action_94 (121#) = happyShift action_15
action_94 (122#) = happyShift action_16
action_94 (128#) = happyShift action_17
action_94 (129#) = happyShift action_18
action_94 (130#) = happyShift action_19
action_94 (131#) = happyShift action_20
action_94 (133#) = happyShift action_21
action_94 (135#) = happyShift action_22
action_94 (38#) = happyGoto action_155
action_94 (43#) = happyGoto action_156
action_94 (44#) = happyGoto action_4
action_94 (45#) = happyGoto action_5
action_94 (49#) = happyGoto action_6
action_94 (60#) = happyGoto action_7
action_94 (62#) = happyGoto action_8
action_94 x = happyTcHack x happyFail

action_95 x = happyTcHack x happyReduce_94

action_96 (129#) = happyShift action_40
action_96 (131#) = happyShift action_42
action_96 (133#) = happyShift action_44
action_96 (135#) = happyShift action_46
action_96 (57#) = happyGoto action_33
action_96 (58#) = happyGoto action_153
action_96 (59#) = happyGoto action_154
action_96 x = happyTcHack x happyReduce_124

action_97 x = happyTcHack x happyReduce_196

action_98 (156#) = happyShift action_152
action_98 x = happyTcHack x happyFail

action_99 x = happyTcHack x happyReduce_213

action_100 x = happyTcHack x happyReduce_214

action_101 x = happyTcHack x happyReduce_201

action_102 (157#) = happyShift action_151
action_102 x = happyTcHack x happyFail

action_103 x = happyTcHack x happyReduce_198

action_104 x = happyTcHack x happyReduce_199

action_105 (113#) = happyShift action_37
action_105 (128#) = happyShift action_39
action_105 (129#) = happyShift action_40
action_105 (130#) = happyShift action_41
action_105 (131#) = happyShift action_42
action_105 (132#) = happyShift action_43
action_105 (133#) = happyShift action_44
action_105 (134#) = happyShift action_45
action_105 (135#) = happyShift action_46
action_105 (151#) = happyShift action_127
action_105 (155#) = happyShift action_57
action_105 (156#) = happyShift action_58
action_105 (157#) = happyShift action_59
action_105 (158#) = happyShift action_60
action_105 (159#) = happyShift action_61
action_105 (160#) = happyShift action_62
action_105 (161#) = happyShift action_63
action_105 (162#) = happyShift action_64
action_105 (163#) = happyShift action_65
action_105 (164#) = happyShift action_66
action_105 (48#) = happyGoto action_30
action_105 (53#) = happyGoto action_31
action_105 (54#) = happyGoto action_123
action_105 (57#) = happyGoto action_33
action_105 (58#) = happyGoto action_124
action_105 (81#) = happyGoto action_125
action_105 (84#) = happyGoto action_126
action_105 x = happyTcHack x happyReduce_174

action_106 (113#) = happyShift action_37
action_106 (128#) = happyShift action_39
action_106 (129#) = happyShift action_40
action_106 (130#) = happyShift action_41
action_106 (131#) = happyShift action_42
action_106 (132#) = happyShift action_43
action_106 (133#) = happyShift action_44
action_106 (134#) = happyShift action_45
action_106 (135#) = happyShift action_46
action_106 (151#) = happyShift action_127
action_106 (155#) = happyShift action_57
action_106 (156#) = happyShift action_58
action_106 (157#) = happyShift action_59
action_106 (158#) = happyShift action_60
action_106 (159#) = happyShift action_61
action_106 (160#) = happyShift action_62
action_106 (161#) = happyShift action_63
action_106 (162#) = happyShift action_64
action_106 (163#) = happyShift action_65
action_106 (164#) = happyShift action_66
action_106 (48#) = happyGoto action_30
action_106 (53#) = happyGoto action_31
action_106 (54#) = happyGoto action_123
action_106 (57#) = happyGoto action_33
action_106 (58#) = happyGoto action_124
action_106 (81#) = happyGoto action_125
action_106 (84#) = happyGoto action_126
action_106 x = happyTcHack x happyReduce_176

action_107 (121#) = happyShift action_150
action_107 x = happyTcHack x happyFail

action_108 x = happyTcHack x happyReduce_211

action_109 x = happyTcHack x happyReduce_212

action_110 x = happyTcHack x happyReduce_210

action_111 (113#) = happyShift action_37
action_111 (122#) = happyShift action_38
action_111 (128#) = happyShift action_39
action_111 (129#) = happyShift action_40
action_111 (130#) = happyShift action_41
action_111 (131#) = happyShift action_42
action_111 (132#) = happyShift action_43
action_111 (133#) = happyShift action_44
action_111 (134#) = happyShift action_45
action_111 (135#) = happyShift action_46
action_111 (142#) = happyShift action_47
action_111 (143#) = happyShift action_48
action_111 (144#) = happyShift action_49
action_111 (145#) = happyShift action_50
action_111 (146#) = happyShift action_51
action_111 (147#) = happyShift action_52
action_111 (150#) = happyShift action_53
action_111 (152#) = happyShift action_54
action_111 (153#) = happyShift action_55
action_111 (154#) = happyShift action_56
action_111 (155#) = happyShift action_57
action_111 (156#) = happyShift action_58
action_111 (157#) = happyShift action_59
action_111 (158#) = happyShift action_60
action_111 (159#) = happyShift action_61
action_111 (160#) = happyShift action_62
action_111 (161#) = happyShift action_63
action_111 (162#) = happyShift action_64
action_111 (163#) = happyShift action_65
action_111 (164#) = happyShift action_66
action_111 (48#) = happyGoto action_30
action_111 (53#) = happyGoto action_31
action_111 (54#) = happyGoto action_32
action_111 (57#) = happyGoto action_33
action_111 (58#) = happyGoto action_34
action_111 (76#) = happyGoto action_149
action_111 (84#) = happyGoto action_36
action_111 x = happyTcHack x happyFail

action_112 (113#) = happyShift action_37
action_112 (128#) = happyShift action_39
action_112 (130#) = happyShift action_41
action_112 (48#) = happyGoto action_30
action_112 (53#) = happyGoto action_119
action_112 (77#) = happyGoto action_147
action_112 (85#) = happyGoto action_148
action_112 x = happyTcHack x happyReduce_177

action_113 (113#) = happyShift action_37
action_113 (128#) = happyShift action_39
action_113 (130#) = happyShift action_41
action_113 (48#) = happyGoto action_30
action_113 (53#) = happyGoto action_119
action_113 (85#) = happyGoto action_146
action_113 x = happyTcHack x happyFail

action_114 (113#) = happyShift action_37
action_114 (128#) = happyShift action_39
action_114 (129#) = happyShift action_40
action_114 (130#) = happyShift action_41
action_114 (131#) = happyShift action_42
action_114 (132#) = happyShift action_43
action_114 (133#) = happyShift action_44
action_114 (134#) = happyShift action_45
action_114 (135#) = happyShift action_46
action_114 (149#) = happyShift action_145
action_114 (151#) = happyShift action_127
action_114 (155#) = happyShift action_57
action_114 (156#) = happyShift action_58
action_114 (157#) = happyShift action_59
action_114 (158#) = happyShift action_60
action_114 (159#) = happyShift action_61
action_114 (160#) = happyShift action_62
action_114 (161#) = happyShift action_63
action_114 (162#) = happyShift action_64
action_114 (163#) = happyShift action_65
action_114 (164#) = happyShift action_66
action_114 (48#) = happyGoto action_30
action_114 (53#) = happyGoto action_31
action_114 (54#) = happyGoto action_123
action_114 (57#) = happyGoto action_33
action_114 (58#) = happyGoto action_124
action_114 (81#) = happyGoto action_125
action_114 (84#) = happyGoto action_126
action_114 x = happyTcHack x happyFail

action_115 (113#) = happyShift action_37
action_115 (128#) = happyShift action_39
action_115 (129#) = happyShift action_40
action_115 (130#) = happyShift action_41
action_115 (131#) = happyShift action_42
action_115 (132#) = happyShift action_43
action_115 (133#) = happyShift action_44
action_115 (134#) = happyShift action_45
action_115 (135#) = happyShift action_46
action_115 (149#) = happyShift action_144
action_115 (151#) = happyShift action_127
action_115 (155#) = happyShift action_57
action_115 (156#) = happyShift action_58
action_115 (157#) = happyShift action_59
action_115 (158#) = happyShift action_60
action_115 (159#) = happyShift action_61
action_115 (160#) = happyShift action_62
action_115 (161#) = happyShift action_63
action_115 (162#) = happyShift action_64
action_115 (163#) = happyShift action_65
action_115 (164#) = happyShift action_66
action_115 (48#) = happyGoto action_30
action_115 (53#) = happyGoto action_31
action_115 (54#) = happyGoto action_123
action_115 (57#) = happyGoto action_33
action_115 (58#) = happyGoto action_124
action_115 (81#) = happyGoto action_125
action_115 (84#) = happyGoto action_126
action_115 x = happyTcHack x happyFail

action_116 (115#) = happyShift action_143
action_116 x = happyTcHack x happyReduce_207

action_117 (128#) = happyShift action_17
action_117 (130#) = happyShift action_19
action_117 (62#) = happyGoto action_116
action_117 (87#) = happyGoto action_117
action_117 (88#) = happyGoto action_142
action_117 x = happyTcHack x happyReduce_208

action_118 (123#) = happyShift action_141
action_118 x = happyTcHack x happyFail

action_119 (115#) = happyShift action_140
action_119 x = happyTcHack x happyFail

action_120 (113#) = happyShift action_37
action_120 (128#) = happyShift action_39
action_120 (130#) = happyShift action_41
action_120 (48#) = happyGoto action_30
action_120 (53#) = happyGoto action_119
action_120 (85#) = happyGoto action_120
action_120 (86#) = happyGoto action_139
action_120 x = happyTcHack x happyReduce_204

action_121 (123#) = happyShift action_138
action_121 x = happyTcHack x happyFail

action_122 (113#) = happyShift action_37
action_122 (126#) = happyShift action_137
action_122 (128#) = happyShift action_39
action_122 (129#) = happyShift action_40
action_122 (130#) = happyShift action_41
action_122 (131#) = happyShift action_42
action_122 (132#) = happyShift action_43
action_122 (133#) = happyShift action_44
action_122 (134#) = happyShift action_45
action_122 (135#) = happyShift action_46
action_122 (151#) = happyShift action_127
action_122 (155#) = happyShift action_57
action_122 (156#) = happyShift action_58
action_122 (157#) = happyShift action_59
action_122 (158#) = happyShift action_60
action_122 (159#) = happyShift action_61
action_122 (160#) = happyShift action_62
action_122 (161#) = happyShift action_63
action_122 (162#) = happyShift action_64
action_122 (163#) = happyShift action_65
action_122 (164#) = happyShift action_66
action_122 (48#) = happyGoto action_30
action_122 (53#) = happyGoto action_31
action_122 (54#) = happyGoto action_123
action_122 (57#) = happyGoto action_33
action_122 (58#) = happyGoto action_124
action_122 (81#) = happyGoto action_125
action_122 (84#) = happyGoto action_126
action_122 x = happyTcHack x happyFail

action_123 x = happyTcHack x happyReduce_185

action_124 x = happyTcHack x happyReduce_186

action_125 x = happyTcHack x happyReduce_166

action_126 x = happyTcHack x happyReduce_187

action_127 (120#) = happyShift action_14
action_127 (121#) = happyShift action_15
action_127 (122#) = happyShift action_16
action_127 (128#) = happyShift action_17
action_127 (129#) = happyShift action_18
action_127 (130#) = happyShift action_19
action_127 (131#) = happyShift action_20
action_127 (133#) = happyShift action_21
action_127 (135#) = happyShift action_22
action_127 (45#) = happyGoto action_136
action_127 (49#) = happyGoto action_6
action_127 (60#) = happyGoto action_7
action_127 (62#) = happyGoto action_8
action_127 x = happyTcHack x happyFail

action_128 (113#) = happyShift action_37
action_128 (128#) = happyShift action_39
action_128 (129#) = happyShift action_40
action_128 (130#) = happyShift action_41
action_128 (131#) = happyShift action_42
action_128 (132#) = happyShift action_43
action_128 (133#) = happyShift action_44
action_128 (134#) = happyShift action_45
action_128 (135#) = happyShift action_46
action_128 (151#) = happyShift action_135
action_128 (155#) = happyShift action_57
action_128 (156#) = happyShift action_58
action_128 (157#) = happyShift action_59
action_128 (158#) = happyShift action_60
action_128 (159#) = happyShift action_61
action_128 (160#) = happyShift action_62
action_128 (161#) = happyShift action_63
action_128 (162#) = happyShift action_64
action_128 (163#) = happyShift action_65
action_128 (164#) = happyShift action_66
action_128 (48#) = happyGoto action_30
action_128 (53#) = happyGoto action_31
action_128 (54#) = happyGoto action_123
action_128 (57#) = happyGoto action_33
action_128 (58#) = happyGoto action_124
action_128 (81#) = happyGoto action_133
action_128 (83#) = happyGoto action_134
action_128 (84#) = happyGoto action_126
action_128 x = happyTcHack x happyReduce_190

action_129 (125#) = happyShift action_132
action_129 x = happyTcHack x happyFail

action_130 (119#) = happyShift action_131
action_130 x = happyTcHack x happyFail

action_131 (113#) = happyShift action_37
action_131 (122#) = happyShift action_38
action_131 (128#) = happyShift action_39
action_131 (129#) = happyShift action_40
action_131 (130#) = happyShift action_41
action_131 (131#) = happyShift action_42
action_131 (132#) = happyShift action_43
action_131 (133#) = happyShift action_44
action_131 (134#) = happyShift action_45
action_131 (135#) = happyShift action_46
action_131 (142#) = happyShift action_47
action_131 (143#) = happyShift action_48
action_131 (144#) = happyShift action_49
action_131 (145#) = happyShift action_50
action_131 (146#) = happyShift action_51
action_131 (147#) = happyShift action_52
action_131 (150#) = happyShift action_53
action_131 (152#) = happyShift action_54
action_131 (153#) = happyShift action_55
action_131 (154#) = happyShift action_56
action_131 (155#) = happyShift action_57
action_131 (156#) = happyShift action_58
action_131 (157#) = happyShift action_59
action_131 (158#) = happyShift action_60
action_131 (159#) = happyShift action_61
action_131 (160#) = happyShift action_62
action_131 (161#) = happyShift action_63
action_131 (162#) = happyShift action_64
action_131 (163#) = happyShift action_65
action_131 (164#) = happyShift action_66
action_131 (48#) = happyGoto action_30
action_131 (53#) = happyGoto action_31
action_131 (54#) = happyGoto action_32
action_131 (57#) = happyGoto action_33
action_131 (58#) = happyGoto action_34
action_131 (76#) = happyGoto action_192
action_131 (84#) = happyGoto action_36
action_131 x = happyTcHack x happyFail

action_132 x = happyTcHack x happyReduce_155

action_133 (113#) = happyShift action_37
action_133 (128#) = happyShift action_39
action_133 (129#) = happyShift action_40
action_133 (130#) = happyShift action_41
action_133 (131#) = happyShift action_42
action_133 (132#) = happyShift action_43
action_133 (133#) = happyShift action_44
action_133 (134#) = happyShift action_45
action_133 (135#) = happyShift action_46
action_133 (151#) = happyShift action_135
action_133 (155#) = happyShift action_57
action_133 (156#) = happyShift action_58
action_133 (157#) = happyShift action_59
action_133 (158#) = happyShift action_60
action_133 (159#) = happyShift action_61
action_133 (160#) = happyShift action_62
action_133 (161#) = happyShift action_63
action_133 (162#) = happyShift action_64
action_133 (163#) = happyShift action_65
action_133 (164#) = happyShift action_66
action_133 (48#) = happyGoto action_30
action_133 (53#) = happyGoto action_31
action_133 (54#) = happyGoto action_123
action_133 (57#) = happyGoto action_33
action_133 (58#) = happyGoto action_124
action_133 (81#) = happyGoto action_133
action_133 (83#) = happyGoto action_191
action_133 (84#) = happyGoto action_126
action_133 x = happyTcHack x happyReduce_190

action_134 (124#) = happyShift action_190
action_134 x = happyTcHack x happyFail

action_135 (120#) = happyShift action_14
action_135 (121#) = happyShift action_15
action_135 (122#) = happyShift action_16
action_135 (128#) = happyShift action_17
action_135 (129#) = happyShift action_18
action_135 (130#) = happyShift action_19
action_135 (131#) = happyShift action_20
action_135 (133#) = happyShift action_21
action_135 (135#) = happyShift action_22
action_135 (45#) = happyGoto action_189
action_135 (49#) = happyGoto action_6
action_135 (60#) = happyGoto action_7
action_135 (62#) = happyGoto action_8
action_135 x = happyTcHack x happyFail

action_136 x = happyTcHack x happyReduce_165

action_137 x = happyTcHack x happyReduce_163

action_138 (113#) = happyShift action_37
action_138 (122#) = happyShift action_38
action_138 (128#) = happyShift action_39
action_138 (129#) = happyShift action_40
action_138 (130#) = happyShift action_41
action_138 (131#) = happyShift action_42
action_138 (132#) = happyShift action_43
action_138 (133#) = happyShift action_44
action_138 (134#) = happyShift action_45
action_138 (135#) = happyShift action_46
action_138 (142#) = happyShift action_47
action_138 (143#) = happyShift action_48
action_138 (144#) = happyShift action_49
action_138 (145#) = happyShift action_50
action_138 (146#) = happyShift action_51
action_138 (147#) = happyShift action_52
action_138 (150#) = happyShift action_53
action_138 (152#) = happyShift action_54
action_138 (153#) = happyShift action_55
action_138 (154#) = happyShift action_56
action_138 (155#) = happyShift action_57
action_138 (156#) = happyShift action_58
action_138 (157#) = happyShift action_59
action_138 (158#) = happyShift action_60
action_138 (159#) = happyShift action_61
action_138 (160#) = happyShift action_62
action_138 (161#) = happyShift action_63
action_138 (162#) = happyShift action_64
action_138 (163#) = happyShift action_65
action_138 (164#) = happyShift action_66
action_138 (48#) = happyGoto action_30
action_138 (53#) = happyGoto action_31
action_138 (54#) = happyGoto action_32
action_138 (57#) = happyGoto action_33
action_138 (58#) = happyGoto action_34
action_138 (76#) = happyGoto action_188
action_138 (84#) = happyGoto action_36
action_138 x = happyTcHack x happyFail

action_139 x = happyTcHack x happyReduce_205

action_140 (120#) = happyShift action_14
action_140 (121#) = happyShift action_15
action_140 (122#) = happyShift action_16
action_140 (128#) = happyShift action_17
action_140 (129#) = happyShift action_18
action_140 (130#) = happyShift action_19
action_140 (131#) = happyShift action_20
action_140 (133#) = happyShift action_21
action_140 (135#) = happyShift action_22
action_140 (45#) = happyGoto action_187
action_140 (49#) = happyGoto action_6
action_140 (60#) = happyGoto action_7
action_140 (62#) = happyGoto action_8
action_140 x = happyTcHack x happyFail

action_141 (113#) = happyShift action_37
action_141 (122#) = happyShift action_38
action_141 (128#) = happyShift action_39
action_141 (129#) = happyShift action_40
action_141 (130#) = happyShift action_41
action_141 (131#) = happyShift action_42
action_141 (132#) = happyShift action_43
action_141 (133#) = happyShift action_44
action_141 (134#) = happyShift action_45
action_141 (135#) = happyShift action_46
action_141 (142#) = happyShift action_47
action_141 (143#) = happyShift action_48
action_141 (144#) = happyShift action_49
action_141 (145#) = happyShift action_50
action_141 (146#) = happyShift action_51
action_141 (147#) = happyShift action_52
action_141 (150#) = happyShift action_53
action_141 (152#) = happyShift action_54
action_141 (153#) = happyShift action_55
action_141 (154#) = happyShift action_56
action_141 (155#) = happyShift action_57
action_141 (156#) = happyShift action_58
action_141 (157#) = happyShift action_59
action_141 (158#) = happyShift action_60
action_141 (159#) = happyShift action_61
action_141 (160#) = happyShift action_62
action_141 (161#) = happyShift action_63
action_141 (162#) = happyShift action_64
action_141 (163#) = happyShift action_65
action_141 (164#) = happyShift action_66
action_141 (48#) = happyGoto action_30
action_141 (53#) = happyGoto action_31
action_141 (54#) = happyGoto action_32
action_141 (57#) = happyGoto action_33
action_141 (58#) = happyGoto action_34
action_141 (76#) = happyGoto action_186
action_141 (84#) = happyGoto action_36
action_141 x = happyTcHack x happyFail

action_142 x = happyTcHack x happyReduce_209

action_143 (122#) = happyShift action_171
action_143 (130#) = happyShift action_172
action_143 (67#) = happyGoto action_185
action_143 x = happyTcHack x happyFail

action_144 (120#) = happyShift action_184
action_144 x = happyTcHack x happyFail

action_145 (120#) = happyShift action_183
action_145 x = happyTcHack x happyFail

action_146 (119#) = happyShift action_182
action_146 x = happyTcHack x happyFail

action_147 (124#) = happyShift action_181
action_147 x = happyTcHack x happyFail

action_148 (119#) = happyShift action_180
action_148 x = happyTcHack x happyFail

action_149 (113#) = happyShift action_37
action_149 (128#) = happyShift action_39
action_149 (129#) = happyShift action_40
action_149 (130#) = happyShift action_41
action_149 (131#) = happyShift action_42
action_149 (132#) = happyShift action_43
action_149 (133#) = happyShift action_44
action_149 (134#) = happyShift action_45
action_149 (135#) = happyShift action_46
action_149 (151#) = happyShift action_127
action_149 (155#) = happyShift action_57
action_149 (156#) = happyShift action_58
action_149 (157#) = happyShift action_59
action_149 (158#) = happyShift action_60
action_149 (159#) = happyShift action_61
action_149 (160#) = happyShift action_62
action_149 (161#) = happyShift action_63
action_149 (162#) = happyShift action_64
action_149 (163#) = happyShift action_65
action_149 (164#) = happyShift action_66
action_149 (48#) = happyGoto action_30
action_149 (53#) = happyGoto action_31
action_149 (54#) = happyGoto action_123
action_149 (57#) = happyGoto action_33
action_149 (58#) = happyGoto action_124
action_149 (81#) = happyGoto action_125
action_149 (84#) = happyGoto action_126
action_149 x = happyTcHack x happyReduce_175

action_150 (120#) = happyShift action_14
action_150 (121#) = happyShift action_15
action_150 (122#) = happyShift action_16
action_150 (128#) = happyShift action_17
action_150 (129#) = happyShift action_18
action_150 (130#) = happyShift action_19
action_150 (131#) = happyShift action_20
action_150 (133#) = happyShift action_21
action_150 (135#) = happyShift action_22
action_150 (45#) = happyGoto action_179
action_150 (49#) = happyGoto action_6
action_150 (60#) = happyGoto action_7
action_150 (62#) = happyGoto action_8
action_150 x = happyTcHack x happyFail

action_151 x = happyTcHack x happyReduce_200

action_152 x = happyTcHack x happyReduce_202

action_153 (129#) = happyShift action_40
action_153 (131#) = happyShift action_42
action_153 (133#) = happyShift action_44
action_153 (135#) = happyShift action_46
action_153 (57#) = happyGoto action_33
action_153 (58#) = happyGoto action_153
action_153 (59#) = happyGoto action_178
action_153 x = happyTcHack x happyReduce_124

action_154 (124#) = happyShift action_177
action_154 x = happyTcHack x happyFail

action_155 (116#) = happyShift action_94
action_155 x = happyTcHack x happyReduce_85

action_156 x = happyTcHack x happyReduce_86

action_157 x = happyTcHack x happyReduce_93

action_158 x = happyTcHack x happyReduce_96

action_159 (124#) = happyShift action_176
action_159 x = happyTcHack x happyFail

action_160 (116#) = happyShift action_175
action_160 x = happyTcHack x happyReduce_82

action_161 (120#) = happyShift action_14
action_161 (121#) = happyShift action_15
action_161 (122#) = happyShift action_16
action_161 (128#) = happyShift action_17
action_161 (129#) = happyShift action_18
action_161 (130#) = happyShift action_19
action_161 (131#) = happyShift action_20
action_161 (133#) = happyShift action_21
action_161 (135#) = happyShift action_22
action_161 (45#) = happyGoto action_89
action_161 (46#) = happyGoto action_174
action_161 (49#) = happyGoto action_6
action_161 (60#) = happyGoto action_7
action_161 (62#) = happyGoto action_8
action_161 x = happyTcHack x happyReduce_95

action_162 (112#) = happyShift action_13
action_162 (120#) = happyShift action_14
action_162 (121#) = happyShift action_15
action_162 (122#) = happyShift action_16
action_162 (128#) = happyShift action_17
action_162 (129#) = happyShift action_18
action_162 (130#) = happyShift action_19
action_162 (131#) = happyShift action_20
action_162 (133#) = happyShift action_21
action_162 (135#) = happyShift action_22
action_162 (38#) = happyGoto action_173
action_162 (44#) = happyGoto action_4
action_162 (45#) = happyGoto action_5
action_162 (49#) = happyGoto action_6
action_162 (60#) = happyGoto action_7
action_162 (62#) = happyGoto action_8
action_162 x = happyTcHack x happyFail

action_163 x = happyTcHack x happyReduce_79

action_164 x = happyTcHack x happyReduce_138

action_165 (122#) = happyShift action_171
action_165 (130#) = happyShift action_172
action_165 (67#) = happyGoto action_170
action_165 x = happyTcHack x happyFail

action_166 (97#) = happyShift action_169
action_166 (16#) = happyGoto action_168
action_166 x = happyTcHack x happyReduce_29

action_167 x = happyTcHack x happyReduce_217

action_168 (94#) = happyShift action_209
action_168 (3#) = happyGoto action_208
action_168 x = happyTcHack x happyReduce_6

action_169 (129#) = happyShift action_207
action_169 (17#) = happyGoto action_205
action_169 (47#) = happyGoto action_206
action_169 x = happyTcHack x happyReduce_31

action_170 x = happyTcHack x happyReduce_135

action_171 (122#) = happyShift action_171
action_171 (130#) = happyShift action_172
action_171 (66#) = happyGoto action_203
action_171 (67#) = happyGoto action_204
action_171 x = happyTcHack x happyFail

action_172 x = happyTcHack x happyReduce_141

action_173 x = happyTcHack x happyReduce_76

action_174 x = happyTcHack x happyReduce_84

action_175 (122#) = happyShift action_75
action_175 (129#) = happyShift action_18
action_175 (131#) = happyShift action_20
action_175 (133#) = happyShift action_21
action_175 (135#) = happyShift action_22
action_175 (41#) = happyGoto action_202
action_175 (42#) = happyGoto action_160
action_175 (49#) = happyGoto action_6
action_175 (60#) = happyGoto action_161
action_175 x = happyTcHack x happyFail

action_176 x = happyTcHack x happyReduce_81

action_177 x = happyTcHack x happyReduce_157

action_178 x = happyTcHack x happyReduce_125

action_179 (120#) = happyShift action_14
action_179 (121#) = happyShift action_15
action_179 (122#) = happyShift action_16
action_179 (128#) = happyShift action_17
action_179 (129#) = happyShift action_18
action_179 (130#) = happyShift action_19
action_179 (131#) = happyShift action_20
action_179 (133#) = happyShift action_21
action_179 (135#) = happyShift action_22
action_179 (45#) = happyGoto action_89
action_179 (46#) = happyGoto action_201
action_179 (49#) = happyGoto action_6
action_179 (60#) = happyGoto action_7
action_179 (62#) = happyGoto action_8
action_179 x = happyTcHack x happyReduce_95

action_180 (113#) = happyShift action_37
action_180 (122#) = happyShift action_38
action_180 (128#) = happyShift action_39
action_180 (129#) = happyShift action_40
action_180 (130#) = happyShift action_41
action_180 (131#) = happyShift action_42
action_180 (132#) = happyShift action_43
action_180 (133#) = happyShift action_44
action_180 (134#) = happyShift action_45
action_180 (135#) = happyShift action_46
action_180 (142#) = happyShift action_47
action_180 (143#) = happyShift action_48
action_180 (144#) = happyShift action_49
action_180 (145#) = happyShift action_50
action_180 (146#) = happyShift action_51
action_180 (147#) = happyShift action_52
action_180 (150#) = happyShift action_53
action_180 (152#) = happyShift action_54
action_180 (153#) = happyShift action_55
action_180 (154#) = happyShift action_56
action_180 (155#) = happyShift action_57
action_180 (156#) = happyShift action_58
action_180 (157#) = happyShift action_59
action_180 (158#) = happyShift action_60
action_180 (159#) = happyShift action_61
action_180 (160#) = happyShift action_62
action_180 (161#) = happyShift action_63
action_180 (162#) = happyShift action_64
action_180 (163#) = happyShift action_65
action_180 (164#) = happyShift action_66
action_180 (48#) = happyGoto action_30
action_180 (53#) = happyGoto action_31
action_180 (54#) = happyGoto action_32
action_180 (57#) = happyGoto action_33
action_180 (58#) = happyGoto action_34
action_180 (76#) = happyGoto action_200
action_180 (84#) = happyGoto action_36
action_180 x = happyTcHack x happyFail

action_181 (148#) = happyShift action_199
action_181 x = happyTcHack x happyFail

action_182 (113#) = happyShift action_37
action_182 (122#) = happyShift action_38
action_182 (128#) = happyShift action_39
action_182 (129#) = happyShift action_40
action_182 (130#) = happyShift action_41
action_182 (131#) = happyShift action_42
action_182 (132#) = happyShift action_43
action_182 (133#) = happyShift action_44
action_182 (134#) = happyShift action_45
action_182 (135#) = happyShift action_46
action_182 (142#) = happyShift action_47
action_182 (143#) = happyShift action_48
action_182 (144#) = happyShift action_49
action_182 (145#) = happyShift action_50
action_182 (146#) = happyShift action_51
action_182 (147#) = happyShift action_52
action_182 (150#) = happyShift action_53
action_182 (152#) = happyShift action_54
action_182 (153#) = happyShift action_55
action_182 (154#) = happyShift action_56
action_182 (155#) = happyShift action_57
action_182 (156#) = happyShift action_58
action_182 (157#) = happyShift action_59
action_182 (158#) = happyShift action_60
action_182 (159#) = happyShift action_61
action_182 (160#) = happyShift action_62
action_182 (161#) = happyShift action_63
action_182 (162#) = happyShift action_64
action_182 (163#) = happyShift action_65
action_182 (164#) = happyShift action_66
action_182 (48#) = happyGoto action_30
action_182 (53#) = happyGoto action_31
action_182 (54#) = happyGoto action_32
action_182 (57#) = happyGoto action_33
action_182 (58#) = happyGoto action_34
action_182 (76#) = happyGoto action_198
action_182 (84#) = happyGoto action_36
action_182 x = happyTcHack x happyFail

action_183 (155#) = happyShift action_57
action_183 (156#) = happyShift action_58
action_183 (157#) = happyShift action_59
action_183 (158#) = happyShift action_60
action_183 (159#) = happyShift action_61
action_183 (160#) = happyShift action_62
action_183 (161#) = happyShift action_63
action_183 (162#) = happyShift action_64
action_183 (163#) = happyShift action_65
action_183 (164#) = happyShift action_66
action_183 (78#) = happyGoto action_196
action_183 (84#) = happyGoto action_197
action_183 x = happyTcHack x happyReduce_179

action_184 (129#) = happyShift action_40
action_184 (131#) = happyShift action_42
action_184 (133#) = happyShift action_44
action_184 (135#) = happyShift action_46
action_184 (57#) = happyGoto action_33
action_184 (58#) = happyGoto action_194
action_184 (79#) = happyGoto action_195
action_184 x = happyTcHack x happyReduce_181

action_185 x = happyTcHack x happyReduce_206

action_186 (113#) = happyShift action_37
action_186 (128#) = happyShift action_39
action_186 (129#) = happyShift action_40
action_186 (130#) = happyShift action_41
action_186 (131#) = happyShift action_42
action_186 (132#) = happyShift action_43
action_186 (133#) = happyShift action_44
action_186 (134#) = happyShift action_45
action_186 (135#) = happyShift action_46
action_186 (151#) = happyShift action_127
action_186 (155#) = happyShift action_57
action_186 (156#) = happyShift action_58
action_186 (157#) = happyShift action_59
action_186 (158#) = happyShift action_60
action_186 (159#) = happyShift action_61
action_186 (160#) = happyShift action_62
action_186 (161#) = happyShift action_63
action_186 (162#) = happyShift action_64
action_186 (163#) = happyShift action_65
action_186 (164#) = happyShift action_66
action_186 (48#) = happyGoto action_30
action_186 (53#) = happyGoto action_31
action_186 (54#) = happyGoto action_123
action_186 (57#) = happyGoto action_33
action_186 (58#) = happyGoto action_124
action_186 (81#) = happyGoto action_125
action_186 (84#) = happyGoto action_126
action_186 x = happyTcHack x happyReduce_168

action_187 x = happyTcHack x happyReduce_203

action_188 (113#) = happyShift action_37
action_188 (128#) = happyShift action_39
action_188 (129#) = happyShift action_40
action_188 (130#) = happyShift action_41
action_188 (131#) = happyShift action_42
action_188 (132#) = happyShift action_43
action_188 (133#) = happyShift action_44
action_188 (134#) = happyShift action_45
action_188 (135#) = happyShift action_46
action_188 (151#) = happyShift action_127
action_188 (155#) = happyShift action_57
action_188 (156#) = happyShift action_58
action_188 (157#) = happyShift action_59
action_188 (158#) = happyShift action_60
action_188 (159#) = happyShift action_61
action_188 (160#) = happyShift action_62
action_188 (161#) = happyShift action_63
action_188 (162#) = happyShift action_64
action_188 (163#) = happyShift action_65
action_188 (164#) = happyShift action_66
action_188 (48#) = happyGoto action_30
action_188 (53#) = happyGoto action_31
action_188 (54#) = happyGoto action_123
action_188 (57#) = happyGoto action_33
action_188 (58#) = happyGoto action_124
action_188 (81#) = happyGoto action_125
action_188 (84#) = happyGoto action_126
action_188 x = happyTcHack x happyReduce_167

action_189 (113#) = happyShift action_37
action_189 (128#) = happyShift action_39
action_189 (129#) = happyShift action_40
action_189 (130#) = happyShift action_41
action_189 (131#) = happyShift action_42
action_189 (132#) = happyShift action_43
action_189 (133#) = happyShift action_44
action_189 (134#) = happyShift action_45
action_189 (135#) = happyShift action_46
action_189 (151#) = happyShift action_135
action_189 (155#) = happyShift action_57
action_189 (156#) = happyShift action_58
action_189 (157#) = happyShift action_59
action_189 (158#) = happyShift action_60
action_189 (159#) = happyShift action_61
action_189 (160#) = happyShift action_62
action_189 (161#) = happyShift action_63
action_189 (162#) = happyShift action_64
action_189 (163#) = happyShift action_65
action_189 (164#) = happyShift action_66
action_189 (48#) = happyGoto action_30
action_189 (53#) = happyGoto action_31
action_189 (54#) = happyGoto action_123
action_189 (57#) = happyGoto action_33
action_189 (58#) = happyGoto action_124
action_189 (81#) = happyGoto action_133
action_189 (83#) = happyGoto action_193
action_189 (84#) = happyGoto action_126
action_189 x = happyTcHack x happyReduce_190

action_190 x = happyTcHack x happyReduce_164

action_191 x = happyTcHack x happyReduce_192

action_192 (113#) = happyShift action_37
action_192 (128#) = happyShift action_39
action_192 (129#) = happyShift action_40
action_192 (130#) = happyShift action_41
action_192 (131#) = happyShift action_42
action_192 (132#) = happyShift action_43
action_192 (133#) = happyShift action_44
action_192 (134#) = happyShift action_45
action_192 (135#) = happyShift action_46
action_192 (151#) = happyShift action_127
action_192 (155#) = happyShift action_57
action_192 (156#) = happyShift action_58
action_192 (157#) = happyShift action_59
action_192 (158#) = happyShift action_60
action_192 (159#) = happyShift action_61
action_192 (160#) = happyShift action_62
action_192 (161#) = happyShift action_63
action_192 (162#) = happyShift action_64
action_192 (163#) = happyShift action_65
action_192 (164#) = happyShift action_66
action_192 (48#) = happyGoto action_30
action_192 (53#) = happyGoto action_31
action_192 (54#) = happyGoto action_123
action_192 (57#) = happyGoto action_33
action_192 (58#) = happyGoto action_124
action_192 (81#) = happyGoto action_125
action_192 (84#) = happyGoto action_126
action_192 x = happyTcHack x happyReduce_154

action_193 x = happyTcHack x happyReduce_191

action_194 (113#) = happyShift action_37
action_194 (128#) = happyShift action_39
action_194 (130#) = happyShift action_41
action_194 (48#) = happyGoto action_30
action_194 (53#) = happyGoto action_226
action_194 (55#) = happyGoto action_227
action_194 x = happyTcHack x happyReduce_116

action_195 (113#) = happyShift action_37
action_195 (128#) = happyShift action_39
action_195 (130#) = happyShift action_41
action_195 (48#) = happyGoto action_30
action_195 (53#) = happyGoto action_223
action_195 (80#) = happyGoto action_225
action_195 x = happyTcHack x happyReduce_183

action_196 (113#) = happyShift action_37
action_196 (128#) = happyShift action_39
action_196 (130#) = happyShift action_41
action_196 (48#) = happyGoto action_30
action_196 (53#) = happyGoto action_223
action_196 (80#) = happyGoto action_224
action_196 x = happyTcHack x happyReduce_183

action_197 (123#) = happyShift action_222
action_197 x = happyTcHack x happyFail

action_198 (113#) = happyShift action_37
action_198 (124#) = happyShift action_221
action_198 (128#) = happyShift action_39
action_198 (129#) = happyShift action_40
action_198 (130#) = happyShift action_41
action_198 (131#) = happyShift action_42
action_198 (132#) = happyShift action_43
action_198 (133#) = happyShift action_44
action_198 (134#) = happyShift action_45
action_198 (135#) = happyShift action_46
action_198 (151#) = happyShift action_127
action_198 (155#) = happyShift action_57
action_198 (156#) = happyShift action_58
action_198 (157#) = happyShift action_59
action_198 (158#) = happyShift action_60
action_198 (159#) = happyShift action_61
action_198 (160#) = happyShift action_62
action_198 (161#) = happyShift action_63
action_198 (162#) = happyShift action_64
action_198 (163#) = happyShift action_65
action_198 (164#) = happyShift action_66
action_198 (48#) = happyGoto action_30
action_198 (53#) = happyGoto action_31
action_198 (54#) = happyGoto action_123
action_198 (57#) = happyGoto action_33
action_198 (58#) = happyGoto action_124
action_198 (81#) = happyGoto action_125
action_198 (84#) = happyGoto action_126
action_198 x = happyTcHack x happyFail

action_199 (113#) = happyShift action_37
action_199 (122#) = happyShift action_38
action_199 (128#) = happyShift action_39
action_199 (129#) = happyShift action_40
action_199 (130#) = happyShift action_41
action_199 (131#) = happyShift action_42
action_199 (132#) = happyShift action_43
action_199 (133#) = happyShift action_44
action_199 (134#) = happyShift action_45
action_199 (135#) = happyShift action_46
action_199 (142#) = happyShift action_47
action_199 (143#) = happyShift action_48
action_199 (144#) = happyShift action_49
action_199 (145#) = happyShift action_50
action_199 (146#) = happyShift action_51
action_199 (147#) = happyShift action_52
action_199 (150#) = happyShift action_53
action_199 (152#) = happyShift action_54
action_199 (153#) = happyShift action_55
action_199 (154#) = happyShift action_56
action_199 (155#) = happyShift action_57
action_199 (156#) = happyShift action_58
action_199 (157#) = happyShift action_59
action_199 (158#) = happyShift action_60
action_199 (159#) = happyShift action_61
action_199 (160#) = happyShift action_62
action_199 (161#) = happyShift action_63
action_199 (162#) = happyShift action_64
action_199 (163#) = happyShift action_65
action_199 (164#) = happyShift action_66
action_199 (48#) = happyGoto action_30
action_199 (53#) = happyGoto action_31
action_199 (54#) = happyGoto action_32
action_199 (57#) = happyGoto action_33
action_199 (58#) = happyGoto action_34
action_199 (76#) = happyGoto action_220
action_199 (84#) = happyGoto action_36
action_199 x = happyTcHack x happyFail

action_200 (113#) = happyShift action_37
action_200 (127#) = happyShift action_219
action_200 (128#) = happyShift action_39
action_200 (129#) = happyShift action_40
action_200 (130#) = happyShift action_41
action_200 (131#) = happyShift action_42
action_200 (132#) = happyShift action_43
action_200 (133#) = happyShift action_44
action_200 (134#) = happyShift action_45
action_200 (135#) = happyShift action_46
action_200 (151#) = happyShift action_127
action_200 (155#) = happyShift action_57
action_200 (156#) = happyShift action_58
action_200 (157#) = happyShift action_59
action_200 (158#) = happyShift action_60
action_200 (159#) = happyShift action_61
action_200 (160#) = happyShift action_62
action_200 (161#) = happyShift action_63
action_200 (162#) = happyShift action_64
action_200 (163#) = happyShift action_65
action_200 (164#) = happyShift action_66
action_200 (48#) = happyGoto action_30
action_200 (53#) = happyGoto action_31
action_200 (54#) = happyGoto action_123
action_200 (57#) = happyGoto action_33
action_200 (58#) = happyGoto action_124
action_200 (81#) = happyGoto action_125
action_200 (84#) = happyGoto action_126
action_200 x = happyTcHack x happyFail

action_201 (125#) = happyShift action_218
action_201 x = happyTcHack x happyFail

action_202 x = happyTcHack x happyReduce_83

action_203 (126#) = happyShift action_217
action_203 x = happyTcHack x happyFail

action_204 (123#) = happyShift action_216
action_204 x = happyTcHack x happyReduce_139

action_205 x = happyTcHack x happyReduce_30

action_206 (129#) = happyShift action_207
action_206 (17#) = happyGoto action_215
action_206 (47#) = happyGoto action_206
action_206 x = happyTcHack x happyReduce_31

action_207 x = happyTcHack x happyReduce_97

action_208 (96#) = happyShift action_214
action_208 (10#) = happyGoto action_213
action_208 x = happyTcHack x happyReduce_18

action_209 (129#) = happyShift action_207
action_209 (4#) = happyGoto action_210
action_209 (5#) = happyGoto action_211
action_209 (47#) = happyGoto action_212
action_209 x = happyTcHack x happyReduce_7

action_210 x = happyTcHack x happyReduce_5

action_211 (129#) = happyShift action_207
action_211 (4#) = happyGoto action_245
action_211 (5#) = happyGoto action_211
action_211 (47#) = happyGoto action_212
action_211 x = happyTcHack x happyReduce_7

action_212 (113#) = happyShift action_241
action_212 (12#) = happyGoto action_244
action_212 x = happyTcHack x happyReduce_21

action_213 (99#) = happyShift action_243
action_213 (18#) = happyGoto action_242
action_213 x = happyTcHack x happyReduce_33

action_214 (113#) = happyShift action_241
action_214 (129#) = happyReduce_21
action_214 (11#) = happyGoto action_239
action_214 (12#) = happyGoto action_240
action_214 x = happyTcHack x happyReduce_19

action_215 x = happyTcHack x happyReduce_32

action_216 (122#) = happyShift action_171
action_216 (130#) = happyShift action_172
action_216 (66#) = happyGoto action_238
action_216 (67#) = happyGoto action_204
action_216 x = happyTcHack x happyFail

action_217 x = happyTcHack x happyReduce_142

action_218 (113#) = happyShift action_37
action_218 (128#) = happyShift action_39
action_218 (129#) = happyShift action_40
action_218 (130#) = happyShift action_41
action_218 (131#) = happyShift action_42
action_218 (132#) = happyShift action_43
action_218 (133#) = happyShift action_44
action_218 (134#) = happyShift action_45
action_218 (135#) = happyShift action_46
action_218 (155#) = happyShift action_57
action_218 (156#) = happyShift action_58
action_218 (157#) = happyShift action_59
action_218 (158#) = happyShift action_60
action_218 (159#) = happyShift action_61
action_218 (160#) = happyShift action_62
action_218 (161#) = happyShift action_63
action_218 (162#) = happyShift action_64
action_218 (163#) = happyShift action_65
action_218 (164#) = happyShift action_66
action_218 (48#) = happyGoto action_30
action_218 (53#) = happyGoto action_31
action_218 (54#) = happyGoto action_123
action_218 (57#) = happyGoto action_33
action_218 (58#) = happyGoto action_124
action_218 (81#) = happyGoto action_236
action_218 (82#) = happyGoto action_237
action_218 (84#) = happyGoto action_126
action_218 x = happyTcHack x happyReduce_188

action_219 (113#) = happyShift action_37
action_219 (128#) = happyShift action_39
action_219 (130#) = happyShift action_41
action_219 (48#) = happyGoto action_30
action_219 (53#) = happyGoto action_119
action_219 (77#) = happyGoto action_235
action_219 (85#) = happyGoto action_148
action_219 x = happyTcHack x happyReduce_177

action_220 (113#) = happyShift action_37
action_220 (128#) = happyShift action_39
action_220 (129#) = happyShift action_40
action_220 (130#) = happyShift action_41
action_220 (131#) = happyShift action_42
action_220 (132#) = happyShift action_43
action_220 (133#) = happyShift action_44
action_220 (134#) = happyShift action_45
action_220 (135#) = happyShift action_46
action_220 (151#) = happyShift action_127
action_220 (155#) = happyShift action_57
action_220 (156#) = happyShift action_58
action_220 (157#) = happyShift action_59
action_220 (158#) = happyShift action_60
action_220 (159#) = happyShift action_61
action_220 (160#) = happyShift action_62
action_220 (161#) = happyShift action_63
action_220 (162#) = happyShift action_64
action_220 (163#) = happyShift action_65
action_220 (164#) = happyShift action_66
action_220 (48#) = happyGoto action_30
action_220 (53#) = happyGoto action_31
action_220 (54#) = happyGoto action_123
action_220 (57#) = happyGoto action_33
action_220 (58#) = happyGoto action_124
action_220 (81#) = happyGoto action_125
action_220 (84#) = happyGoto action_126
action_220 x = happyTcHack x happyReduce_172

action_221 (148#) = happyShift action_234
action_221 x = happyTcHack x happyFail

action_222 (113#) = happyShift action_37
action_222 (122#) = happyShift action_38
action_222 (128#) = happyShift action_39
action_222 (129#) = happyShift action_40
action_222 (130#) = happyShift action_41
action_222 (131#) = happyShift action_42
action_222 (132#) = happyShift action_43
action_222 (133#) = happyShift action_44
action_222 (134#) = happyShift action_45
action_222 (135#) = happyShift action_46
action_222 (142#) = happyShift action_47
action_222 (143#) = happyShift action_48
action_222 (144#) = happyShift action_49
action_222 (145#) = happyShift action_50
action_222 (146#) = happyShift action_51
action_222 (147#) = happyShift action_52
action_222 (150#) = happyShift action_53
action_222 (152#) = happyShift action_54
action_222 (153#) = happyShift action_55
action_222 (154#) = happyShift action_56
action_222 (155#) = happyShift action_57
action_222 (156#) = happyShift action_58
action_222 (157#) = happyShift action_59
action_222 (158#) = happyShift action_60
action_222 (159#) = happyShift action_61
action_222 (160#) = happyShift action_62
action_222 (161#) = happyShift action_63
action_222 (162#) = happyShift action_64
action_222 (163#) = happyShift action_65
action_222 (164#) = happyShift action_66
action_222 (48#) = happyGoto action_30
action_222 (53#) = happyGoto action_31
action_222 (54#) = happyGoto action_32
action_222 (57#) = happyGoto action_33
action_222 (58#) = happyGoto action_34
action_222 (76#) = happyGoto action_233
action_222 (84#) = happyGoto action_36
action_222 x = happyTcHack x happyFail

action_223 (123#) = happyShift action_232
action_223 x = happyTcHack x happyFail

action_224 (124#) = happyShift action_231
action_224 x = happyTcHack x happyFail

action_225 (124#) = happyShift action_230
action_225 x = happyTcHack x happyFail

action_226 (113#) = happyShift action_37
action_226 (128#) = happyShift action_39
action_226 (130#) = happyShift action_41
action_226 (48#) = happyGoto action_30
action_226 (53#) = happyGoto action_226
action_226 (55#) = happyGoto action_229
action_226 x = happyTcHack x happyReduce_116

action_227 (123#) = happyShift action_228
action_227 x = happyTcHack x happyFail

action_228 (113#) = happyShift action_37
action_228 (122#) = happyShift action_38
action_228 (128#) = happyShift action_39
action_228 (129#) = happyShift action_40
action_228 (130#) = happyShift action_41
action_228 (131#) = happyShift action_42
action_228 (132#) = happyShift action_43
action_228 (133#) = happyShift action_44
action_228 (134#) = happyShift action_45
action_228 (135#) = happyShift action_46
action_228 (142#) = happyShift action_47
action_228 (143#) = happyShift action_48
action_228 (144#) = happyShift action_49
action_228 (145#) = happyShift action_50
action_228 (146#) = happyShift action_51
action_228 (147#) = happyShift action_52
action_228 (150#) = happyShift action_53
action_228 (152#) = happyShift action_54
action_228 (153#) = happyShift action_55
action_228 (154#) = happyShift action_56
action_228 (155#) = happyShift action_57
action_228 (156#) = happyShift action_58
action_228 (157#) = happyShift action_59
action_228 (158#) = happyShift action_60
action_228 (159#) = happyShift action_61
action_228 (160#) = happyShift action_62
action_228 (161#) = happyShift action_63
action_228 (162#) = happyShift action_64
action_228 (163#) = happyShift action_65
action_228 (164#) = happyShift action_66
action_228 (48#) = happyGoto action_30
action_228 (53#) = happyGoto action_31
action_228 (54#) = happyGoto action_32
action_228 (57#) = happyGoto action_33
action_228 (58#) = happyGoto action_34
action_228 (76#) = happyGoto action_259
action_228 (84#) = happyGoto action_36
action_228 x = happyTcHack x happyFail

action_229 x = happyTcHack x happyReduce_117

action_230 x = happyTcHack x happyReduce_169

action_231 x = happyTcHack x happyReduce_170

action_232 (113#) = happyShift action_37
action_232 (122#) = happyShift action_38
action_232 (128#) = happyShift action_39
action_232 (129#) = happyShift action_40
action_232 (130#) = happyShift action_41
action_232 (131#) = happyShift action_42
action_232 (132#) = happyShift action_43
action_232 (133#) = happyShift action_44
action_232 (134#) = happyShift action_45
action_232 (135#) = happyShift action_46
action_232 (142#) = happyShift action_47
action_232 (143#) = happyShift action_48
action_232 (144#) = happyShift action_49
action_232 (145#) = happyShift action_50
action_232 (146#) = happyShift action_51
action_232 (147#) = happyShift action_52
action_232 (150#) = happyShift action_53
action_232 (152#) = happyShift action_54
action_232 (153#) = happyShift action_55
action_232 (154#) = happyShift action_56
action_232 (155#) = happyShift action_57
action_232 (156#) = happyShift action_58
action_232 (157#) = happyShift action_59
action_232 (158#) = happyShift action_60
action_232 (159#) = happyShift action_61
action_232 (160#) = happyShift action_62
action_232 (161#) = happyShift action_63
action_232 (162#) = happyShift action_64
action_232 (163#) = happyShift action_65
action_232 (164#) = happyShift action_66
action_232 (48#) = happyGoto action_30
action_232 (53#) = happyGoto action_31
action_232 (54#) = happyGoto action_32
action_232 (57#) = happyGoto action_33
action_232 (58#) = happyGoto action_34
action_232 (76#) = happyGoto action_258
action_232 (84#) = happyGoto action_36
action_232 x = happyTcHack x happyFail

action_233 (113#) = happyShift action_37
action_233 (127#) = happyShift action_257
action_233 (128#) = happyShift action_39
action_233 (129#) = happyShift action_40
action_233 (130#) = happyShift action_41
action_233 (131#) = happyShift action_42
action_233 (132#) = happyShift action_43
action_233 (133#) = happyShift action_44
action_233 (134#) = happyShift action_45
action_233 (135#) = happyShift action_46
action_233 (151#) = happyShift action_127
action_233 (155#) = happyShift action_57
action_233 (156#) = happyShift action_58
action_233 (157#) = happyShift action_59
action_233 (158#) = happyShift action_60
action_233 (159#) = happyShift action_61
action_233 (160#) = happyShift action_62
action_233 (161#) = happyShift action_63
action_233 (162#) = happyShift action_64
action_233 (163#) = happyShift action_65
action_233 (164#) = happyShift action_66
action_233 (48#) = happyGoto action_30
action_233 (53#) = happyGoto action_31
action_233 (54#) = happyGoto action_123
action_233 (57#) = happyGoto action_33
action_233 (58#) = happyGoto action_124
action_233 (81#) = happyGoto action_125
action_233 (84#) = happyGoto action_126
action_233 x = happyTcHack x happyFail

action_234 (113#) = happyShift action_37
action_234 (122#) = happyShift action_38
action_234 (128#) = happyShift action_39
action_234 (129#) = happyShift action_40
action_234 (130#) = happyShift action_41
action_234 (131#) = happyShift action_42
action_234 (132#) = happyShift action_43
action_234 (133#) = happyShift action_44
action_234 (134#) = happyShift action_45
action_234 (135#) = happyShift action_46
action_234 (142#) = happyShift action_47
action_234 (143#) = happyShift action_48
action_234 (144#) = happyShift action_49
action_234 (145#) = happyShift action_50
action_234 (146#) = happyShift action_51
action_234 (147#) = happyShift action_52
action_234 (150#) = happyShift action_53
action_234 (152#) = happyShift action_54
action_234 (153#) = happyShift action_55
action_234 (154#) = happyShift action_56
action_234 (155#) = happyShift action_57
action_234 (156#) = happyShift action_58
action_234 (157#) = happyShift action_59
action_234 (158#) = happyShift action_60
action_234 (159#) = happyShift action_61
action_234 (160#) = happyShift action_62
action_234 (161#) = happyShift action_63
action_234 (162#) = happyShift action_64
action_234 (163#) = happyShift action_65
action_234 (164#) = happyShift action_66
action_234 (48#) = happyGoto action_30
action_234 (53#) = happyGoto action_31
action_234 (54#) = happyGoto action_32
action_234 (57#) = happyGoto action_33
action_234 (58#) = happyGoto action_34
action_234 (76#) = happyGoto action_256
action_234 (84#) = happyGoto action_36
action_234 x = happyTcHack x happyFail

action_235 x = happyTcHack x happyReduce_178

action_236 (113#) = happyShift action_37
action_236 (128#) = happyShift action_39
action_236 (129#) = happyShift action_40
action_236 (130#) = happyShift action_41
action_236 (131#) = happyShift action_42
action_236 (132#) = happyShift action_43
action_236 (133#) = happyShift action_44
action_236 (134#) = happyShift action_45
action_236 (135#) = happyShift action_46
action_236 (155#) = happyShift action_57
action_236 (156#) = happyShift action_58
action_236 (157#) = happyShift action_59
action_236 (158#) = happyShift action_60
action_236 (159#) = happyShift action_61
action_236 (160#) = happyShift action_62
action_236 (161#) = happyShift action_63
action_236 (162#) = happyShift action_64
action_236 (163#) = happyShift action_65
action_236 (164#) = happyShift action_66
action_236 (48#) = happyGoto action_30
action_236 (53#) = happyGoto action_31
action_236 (54#) = happyGoto action_123
action_236 (57#) = happyGoto action_33
action_236 (58#) = happyGoto action_124
action_236 (81#) = happyGoto action_236
action_236 (82#) = happyGoto action_255
action_236 (84#) = happyGoto action_126
action_236 x = happyTcHack x happyReduce_188

action_237 x = happyTcHack x happyReduce_173

action_238 x = happyTcHack x happyReduce_140

action_239 x = happyTcHack x happyReduce_17

action_240 (129#) = happyShift action_207
action_240 (47#) = happyGoto action_254
action_240 x = happyTcHack x happyFail

action_241 x = happyTcHack x happyReduce_22

action_242 (98#) = happyShift action_253
action_242 (68#) = happyGoto action_252
action_242 x = happyTcHack x happyReduce_144

action_243 (109#) = happyShift action_249
action_243 (110#) = happyShift action_250
action_243 (111#) = happyShift action_251
action_243 (19#) = happyGoto action_247
action_243 (20#) = happyGoto action_248
action_243 x = happyTcHack x happyReduce_35

action_244 (157#) = happyShift action_246
action_244 x = happyTcHack x happyFail

action_245 x = happyTcHack x happyReduce_8

action_246 (115#) = happyShift action_278
action_246 x = happyTcHack x happyFail

action_247 x = happyTcHack x happyReduce_34

action_248 (109#) = happyShift action_249
action_248 (110#) = happyShift action_250
action_248 (111#) = happyShift action_251
action_248 (19#) = happyGoto action_277
action_248 (20#) = happyGoto action_248
action_248 x = happyTcHack x happyReduce_35

action_249 (157#) = happyShift action_276
action_249 x = happyTcHack x happyFail

action_250 (157#) = happyShift action_275
action_250 x = happyTcHack x happyFail

action_251 (157#) = happyShift action_274
action_251 x = happyTcHack x happyFail

action_252 (100#) = happyShift action_273
action_252 (21#) = happyGoto action_272
action_252 x = happyTcHack x happyReduce_40

action_253 (108#) = happyReduce_215
action_253 (69#) = happyGoto action_269
action_253 (70#) = happyGoto action_270
action_253 (91#) = happyGoto action_271
action_253 x = happyTcHack x happyReduce_145

action_254 (113#) = happyShift action_37
action_254 (122#) = happyShift action_75
action_254 (123#) = happyShift action_268
action_254 (128#) = happyShift action_39
action_254 (129#) = happyShift action_18
action_254 (130#) = happyShift action_41
action_254 (131#) = happyShift action_20
action_254 (13#) = happyGoto action_263
action_254 (14#) = happyGoto action_264
action_254 (48#) = happyGoto action_265
action_254 (49#) = happyGoto action_266
action_254 (50#) = happyGoto action_267
action_254 x = happyTcHack x happyReduce_23

action_255 x = happyTcHack x happyReduce_189

action_256 (113#) = happyShift action_37
action_256 (128#) = happyShift action_39
action_256 (129#) = happyShift action_40
action_256 (130#) = happyShift action_41
action_256 (131#) = happyShift action_42
action_256 (132#) = happyShift action_43
action_256 (133#) = happyShift action_44
action_256 (134#) = happyShift action_45
action_256 (135#) = happyShift action_46
action_256 (151#) = happyShift action_127
action_256 (155#) = happyShift action_57
action_256 (156#) = happyShift action_58
action_256 (157#) = happyShift action_59
action_256 (158#) = happyShift action_60
action_256 (159#) = happyShift action_61
action_256 (160#) = happyShift action_62
action_256 (161#) = happyShift action_63
action_256 (162#) = happyShift action_64
action_256 (163#) = happyShift action_65
action_256 (164#) = happyShift action_66
action_256 (48#) = happyGoto action_30
action_256 (53#) = happyGoto action_31
action_256 (54#) = happyGoto action_123
action_256 (57#) = happyGoto action_33
action_256 (58#) = happyGoto action_124
action_256 (81#) = happyGoto action_125
action_256 (84#) = happyGoto action_126
action_256 x = happyTcHack x happyReduce_171

action_257 (155#) = happyShift action_57
action_257 (156#) = happyShift action_58
action_257 (157#) = happyShift action_59
action_257 (158#) = happyShift action_60
action_257 (159#) = happyShift action_61
action_257 (160#) = happyShift action_62
action_257 (161#) = happyShift action_63
action_257 (162#) = happyShift action_64
action_257 (163#) = happyShift action_65
action_257 (164#) = happyShift action_66
action_257 (78#) = happyGoto action_262
action_257 (84#) = happyGoto action_197
action_257 x = happyTcHack x happyReduce_179

action_258 (113#) = happyShift action_37
action_258 (127#) = happyShift action_261
action_258 (128#) = happyShift action_39
action_258 (129#) = happyShift action_40
action_258 (130#) = happyShift action_41
action_258 (131#) = happyShift action_42
action_258 (132#) = happyShift action_43
action_258 (133#) = happyShift action_44
action_258 (134#) = happyShift action_45
action_258 (135#) = happyShift action_46
action_258 (151#) = happyShift action_127
action_258 (155#) = happyShift action_57
action_258 (156#) = happyShift action_58
action_258 (157#) = happyShift action_59
action_258 (158#) = happyShift action_60
action_258 (159#) = happyShift action_61
action_258 (160#) = happyShift action_62
action_258 (161#) = happyShift action_63
action_258 (162#) = happyShift action_64
action_258 (163#) = happyShift action_65
action_258 (164#) = happyShift action_66
action_258 (48#) = happyGoto action_30
action_258 (53#) = happyGoto action_31
action_258 (54#) = happyGoto action_123
action_258 (57#) = happyGoto action_33
action_258 (58#) = happyGoto action_124
action_258 (81#) = happyGoto action_125
action_258 (84#) = happyGoto action_126
action_258 x = happyTcHack x happyFail

action_259 (113#) = happyShift action_37
action_259 (127#) = happyShift action_260
action_259 (128#) = happyShift action_39
action_259 (129#) = happyShift action_40
action_259 (130#) = happyShift action_41
action_259 (131#) = happyShift action_42
action_259 (132#) = happyShift action_43
action_259 (133#) = happyShift action_44
action_259 (134#) = happyShift action_45
action_259 (135#) = happyShift action_46
action_259 (151#) = happyShift action_127
action_259 (155#) = happyShift action_57
action_259 (156#) = happyShift action_58
action_259 (157#) = happyShift action_59
action_259 (158#) = happyShift action_60
action_259 (159#) = happyShift action_61
action_259 (160#) = happyShift action_62
action_259 (161#) = happyShift action_63
action_259 (162#) = happyShift action_64
action_259 (163#) = happyShift action_65
action_259 (164#) = happyShift action_66
action_259 (48#) = happyGoto action_30
action_259 (53#) = happyGoto action_31
action_259 (54#) = happyGoto action_123
action_259 (57#) = happyGoto action_33
action_259 (58#) = happyGoto action_124
action_259 (81#) = happyGoto action_125
action_259 (84#) = happyGoto action_126
action_259 x = happyTcHack x happyFail

action_260 (129#) = happyShift action_40
action_260 (131#) = happyShift action_42
action_260 (133#) = happyShift action_44
action_260 (135#) = happyShift action_46
action_260 (57#) = happyGoto action_33
action_260 (58#) = happyGoto action_194
action_260 (79#) = happyGoto action_298
action_260 x = happyTcHack x happyReduce_181

action_261 x = happyTcHack x happyReduce_184

action_262 x = happyTcHack x happyReduce_180

action_263 (127#) = happyShift action_297
action_263 x = happyTcHack x happyFail

action_264 (113#) = happyShift action_37
action_264 (122#) = happyShift action_75
action_264 (123#) = happyShift action_268
action_264 (128#) = happyShift action_39
action_264 (129#) = happyShift action_18
action_264 (130#) = happyShift action_41
action_264 (131#) = happyShift action_20
action_264 (13#) = happyGoto action_296
action_264 (14#) = happyGoto action_264
action_264 (48#) = happyGoto action_265
action_264 (49#) = happyGoto action_266
action_264 (50#) = happyGoto action_267
action_264 x = happyTcHack x happyReduce_23

action_265 x = happyTcHack x happyReduce_104

action_266 x = happyTcHack x happyReduce_105

action_267 (114#) = happyShift action_294
action_267 (122#) = happyShift action_295
action_267 (15#) = happyGoto action_293
action_267 x = happyTcHack x happyReduce_25

action_268 x = happyTcHack x happyReduce_106

action_269 x = happyTcHack x happyReduce_143

action_270 (108#) = happyReduce_215
action_270 (69#) = happyGoto action_292
action_270 (70#) = happyGoto action_270
action_270 (91#) = happyGoto action_271
action_270 x = happyTcHack x happyReduce_145

action_271 (108#) = happyShift action_291
action_271 x = happyTcHack x happyFail

action_272 x = happyTcHack x happyReduce_4

action_273 (157#) = happyShift action_290
action_273 (22#) = happyGoto action_288
action_273 (23#) = happyGoto action_289
action_273 x = happyTcHack x happyReduce_42

action_274 (113#) = happyShift action_37
action_274 (128#) = happyShift action_39
action_274 (129#) = happyShift action_284
action_274 (130#) = happyShift action_41
action_274 (131#) = happyShift action_285
action_274 (48#) = happyGoto action_282
action_274 (51#) = happyGoto action_287
action_274 x = happyTcHack x happyFail

action_275 (113#) = happyShift action_37
action_275 (128#) = happyShift action_39
action_275 (129#) = happyShift action_284
action_275 (130#) = happyShift action_41
action_275 (131#) = happyShift action_285
action_275 (48#) = happyGoto action_282
action_275 (51#) = happyGoto action_286
action_275 x = happyTcHack x happyFail

action_276 (113#) = happyShift action_37
action_276 (128#) = happyShift action_39
action_276 (129#) = happyShift action_284
action_276 (130#) = happyShift action_41
action_276 (131#) = happyShift action_285
action_276 (48#) = happyGoto action_282
action_276 (51#) = happyGoto action_283
action_276 x = happyTcHack x happyFail

action_277 x = happyTcHack x happyReduce_36

action_278 (113#) = happyShift action_37
action_278 (122#) = happyShift action_75
action_278 (123#) = happyShift action_268
action_278 (128#) = happyShift action_39
action_278 (129#) = happyShift action_18
action_278 (130#) = happyShift action_41
action_278 (131#) = happyShift action_20
action_278 (6#) = happyGoto action_279
action_278 (9#) = happyGoto action_280
action_278 (48#) = happyGoto action_265
action_278 (49#) = happyGoto action_266
action_278 (50#) = happyGoto action_281
action_278 x = happyTcHack x happyReduce_10

action_279 (127#) = happyShift action_312
action_279 x = happyTcHack x happyFail

action_280 (113#) = happyShift action_37
action_280 (122#) = happyShift action_75
action_280 (123#) = happyShift action_268
action_280 (128#) = happyShift action_39
action_280 (129#) = happyShift action_18
action_280 (130#) = happyShift action_41
action_280 (131#) = happyShift action_20
action_280 (8#) = happyGoto action_310
action_280 (9#) = happyGoto action_311
action_280 (48#) = happyGoto action_265
action_280 (49#) = happyGoto action_266
action_280 (50#) = happyGoto action_281
action_280 x = happyTcHack x happyReduce_14

action_281 (157#) = happyShift action_309
action_281 x = happyTcHack x happyFail

action_282 x = happyTcHack x happyReduce_107

action_283 (127#) = happyShift action_308
action_283 x = happyTcHack x happyFail

action_284 x = happyTcHack x happyReduce_108

action_285 x = happyTcHack x happyReduce_109

action_286 (127#) = happyShift action_307
action_286 x = happyTcHack x happyFail

action_287 (127#) = happyShift action_306
action_287 x = happyTcHack x happyFail

action_288 x = happyTcHack x happyReduce_41

action_289 (24#) = happyGoto action_304
action_289 (91#) = happyGoto action_305
action_289 x = happyTcHack x happyReduce_215

action_290 x = happyTcHack x happyReduce_44

action_291 (112#) = happyShift action_13
action_291 (120#) = happyShift action_14
action_291 (121#) = happyShift action_15
action_291 (122#) = happyShift action_16
action_291 (128#) = happyShift action_17
action_291 (129#) = happyShift action_18
action_291 (130#) = happyShift action_19
action_291 (131#) = happyShift action_20
action_291 (133#) = happyShift action_21
action_291 (135#) = happyShift action_22
action_291 (38#) = happyGoto action_303
action_291 (44#) = happyGoto action_4
action_291 (45#) = happyGoto action_5
action_291 (49#) = happyGoto action_6
action_291 (60#) = happyGoto action_7
action_291 (62#) = happyGoto action_8
action_291 x = happyTcHack x happyFail

action_292 x = happyTcHack x happyReduce_146

action_293 x = happyTcHack x happyReduce_26

action_294 (122#) = happyShift action_295
action_294 (15#) = happyGoto action_302
action_294 x = happyTcHack x happyFail

action_295 (113#) = happyShift action_37
action_295 (128#) = happyShift action_39
action_295 (129#) = happyShift action_284
action_295 (130#) = happyShift action_41
action_295 (131#) = happyShift action_285
action_295 (48#) = happyGoto action_282
action_295 (51#) = happyGoto action_300
action_295 (52#) = happyGoto action_301
action_295 x = happyTcHack x happyFail

action_296 x = happyTcHack x happyReduce_24

action_297 (113#) = happyShift action_241
action_297 (129#) = happyReduce_21
action_297 (11#) = happyGoto action_299
action_297 (12#) = happyGoto action_240
action_297 x = happyTcHack x happyReduce_19

action_298 x = happyTcHack x happyReduce_182

action_299 x = happyTcHack x happyReduce_20

action_300 (113#) = happyShift action_37
action_300 (128#) = happyShift action_39
action_300 (129#) = happyShift action_284
action_300 (130#) = happyShift action_41
action_300 (131#) = happyShift action_285
action_300 (48#) = happyGoto action_282
action_300 (51#) = happyGoto action_300
action_300 (52#) = happyGoto action_322
action_300 x = happyTcHack x happyReduce_110

action_301 (126#) = happyShift action_321
action_301 x = happyTcHack x happyFail

action_302 x = happyTcHack x happyReduce_27

action_303 (119#) = happyShift action_320
action_303 x = happyTcHack x happyFail

action_304 (157#) = happyShift action_290
action_304 (22#) = happyGoto action_319
action_304 (23#) = happyGoto action_289
action_304 x = happyTcHack x happyReduce_42

action_305 (102#) = happyShift action_315
action_305 (103#) = happyShift action_316
action_305 (104#) = happyShift action_317
action_305 (106#) = happyShift action_318
action_305 (113#) = happyShift action_37
action_305 (128#) = happyShift action_39
action_305 (130#) = happyShift action_41
action_305 (48#) = happyGoto action_30
action_305 (53#) = happyGoto action_314
action_305 x = happyTcHack x happyFail

action_306 x = happyTcHack x happyReduce_39

action_307 x = happyTcHack x happyReduce_38

action_308 x = happyTcHack x happyReduce_37

action_309 x = happyTcHack x happyReduce_16

action_310 x = happyTcHack x happyReduce_11

action_311 (113#) = happyShift action_37
action_311 (122#) = happyShift action_75
action_311 (123#) = happyShift action_268
action_311 (128#) = happyShift action_39
action_311 (129#) = happyShift action_18
action_311 (130#) = happyShift action_41
action_311 (131#) = happyShift action_20
action_311 (8#) = happyGoto action_313
action_311 (9#) = happyGoto action_311
action_311 (48#) = happyGoto action_265
action_311 (49#) = happyGoto action_266
action_311 (50#) = happyGoto action_281
action_311 x = happyTcHack x happyReduce_14

action_312 x = happyTcHack x happyReduce_9

action_313 x = happyTcHack x happyReduce_15

action_314 (137#) = happyShift action_329
action_314 x = happyTcHack x happyFail

action_315 (120#) = happyShift action_325
action_315 (25#) = happyGoto action_328
action_315 x = happyTcHack x happyReduce_50

action_316 (122#) = happyShift action_75
action_316 (129#) = happyShift action_18
action_316 (131#) = happyShift action_20
action_316 (133#) = happyShift action_21
action_316 (135#) = happyShift action_22
action_316 (49#) = happyGoto action_6
action_316 (60#) = happyGoto action_327
action_316 x = happyTcHack x happyFail

action_317 (120#) = happyShift action_325
action_317 (25#) = happyGoto action_326
action_317 x = happyTcHack x happyReduce_50

action_318 (120#) = happyShift action_325
action_318 (25#) = happyGoto action_324
action_318 x = happyTcHack x happyReduce_50

action_319 x = happyTcHack x happyReduce_43

action_320 (113#) = happyShift action_37
action_320 (128#) = happyShift action_39
action_320 (130#) = happyShift action_41
action_320 (48#) = happyGoto action_30
action_320 (53#) = happyGoto action_323
action_320 x = happyTcHack x happyFail

action_321 x = happyTcHack x happyReduce_28

action_322 x = happyTcHack x happyReduce_111

action_323 (127#) = happyShift action_335
action_323 x = happyTcHack x happyFail

action_324 (122#) = happyShift action_75
action_324 (129#) = happyShift action_18
action_324 (131#) = happyShift action_20
action_324 (133#) = happyShift action_21
action_324 (135#) = happyShift action_22
action_324 (49#) = happyGoto action_6
action_324 (60#) = happyGoto action_334
action_324 x = happyTcHack x happyFail

action_325 (122#) = happyShift action_75
action_325 (129#) = happyShift action_18
action_325 (131#) = happyShift action_20
action_325 (133#) = happyShift action_21
action_325 (135#) = happyShift action_22
action_325 (41#) = happyGoto action_333
action_325 (42#) = happyGoto action_160
action_325 (49#) = happyGoto action_6
action_325 (60#) = happyGoto action_161
action_325 x = happyTcHack x happyFail

action_326 (122#) = happyShift action_75
action_326 (129#) = happyShift action_18
action_326 (131#) = happyShift action_20
action_326 (133#) = happyShift action_21
action_326 (135#) = happyShift action_22
action_326 (49#) = happyGoto action_6
action_326 (60#) = happyGoto action_332
action_326 x = happyTcHack x happyFail

action_327 (128#) = happyShift action_17
action_327 (130#) = happyShift action_19
action_327 (62#) = happyGoto action_84
action_327 (64#) = happyGoto action_85
action_327 (65#) = happyGoto action_331
action_327 x = happyTcHack x happyReduce_137

action_328 (122#) = happyShift action_75
action_328 (129#) = happyShift action_18
action_328 (131#) = happyShift action_20
action_328 (133#) = happyShift action_21
action_328 (135#) = happyShift action_22
action_328 (49#) = happyGoto action_6
action_328 (60#) = happyGoto action_330
action_328 x = happyTcHack x happyFail

action_329 x = happyTcHack x happyReduce_49

action_330 (128#) = happyShift action_17
action_330 (130#) = happyShift action_19
action_330 (62#) = happyGoto action_84
action_330 (64#) = happyGoto action_85
action_330 (65#) = happyGoto action_340
action_330 x = happyTcHack x happyReduce_137

action_331 (119#) = happyShift action_339
action_331 x = happyTcHack x happyFail

action_332 (128#) = happyShift action_17
action_332 (130#) = happyShift action_19
action_332 (62#) = happyGoto action_84
action_332 (64#) = happyGoto action_85
action_332 (65#) = happyGoto action_338
action_332 x = happyTcHack x happyReduce_137

action_333 (124#) = happyShift action_337
action_333 x = happyTcHack x happyFail

action_334 (128#) = happyShift action_17
action_334 (130#) = happyShift action_19
action_334 (62#) = happyGoto action_84
action_334 (64#) = happyGoto action_85
action_334 (65#) = happyGoto action_336
action_334 x = happyTcHack x happyReduce_137

action_335 x = happyTcHack x happyReduce_147

action_336 (107#) = happyShift action_348
action_336 (26#) = happyGoto action_347
action_336 x = happyTcHack x happyReduce_52

action_337 (117#) = happyShift action_346
action_337 x = happyTcHack x happyFail

action_338 (119#) = happyReduce_215
action_338 (32#) = happyGoto action_344
action_338 (91#) = happyGoto action_345
action_338 x = happyTcHack x happyReduce_64

action_339 (112#) = happyShift action_13
action_339 (120#) = happyShift action_14
action_339 (121#) = happyShift action_15
action_339 (122#) = happyShift action_16
action_339 (128#) = happyShift action_17
action_339 (129#) = happyShift action_18
action_339 (130#) = happyShift action_19
action_339 (131#) = happyShift action_20
action_339 (133#) = happyShift action_21
action_339 (135#) = happyShift action_22
action_339 (38#) = happyGoto action_343
action_339 (44#) = happyGoto action_4
action_339 (45#) = happyGoto action_5
action_339 (49#) = happyGoto action_6
action_339 (60#) = happyGoto action_7
action_339 (62#) = happyGoto action_8
action_339 x = happyTcHack x happyFail

action_340 (119#) = happyShift action_342
action_340 (29#) = happyGoto action_341
action_340 x = happyTcHack x happyReduce_58

action_341 (105#) = happyShift action_353
action_341 (33#) = happyGoto action_358
action_341 x = happyTcHack x happyReduce_66

action_342 (30#) = happyGoto action_355
action_342 (31#) = happyGoto action_356
action_342 (91#) = happyGoto action_357
action_342 x = happyTcHack x happyReduce_215

action_343 (127#) = happyShift action_354
action_343 x = happyTcHack x happyFail

action_344 (105#) = happyShift action_353
action_344 (33#) = happyGoto action_352
action_344 x = happyTcHack x happyReduce_66

action_345 (119#) = happyShift action_351
action_345 x = happyTcHack x happyFail

action_346 x = happyTcHack x happyReduce_51

action_347 (127#) = happyShift action_350
action_347 x = happyTcHack x happyFail

action_348 (120#) = happyShift action_349
action_348 x = happyTcHack x happyFail

action_349 (27#) = happyGoto action_365
action_349 (28#) = happyGoto action_366
action_349 (91#) = happyGoto action_367
action_349 x = happyTcHack x happyReduce_215

action_350 x = happyTcHack x happyReduce_48

action_351 (129#) = happyShift action_40
action_351 (131#) = happyShift action_42
action_351 (57#) = happyGoto action_364
action_351 x = happyTcHack x happyFail

action_352 (127#) = happyShift action_363
action_352 x = happyTcHack x happyFail

action_353 (122#) = happyShift action_362
action_353 x = happyTcHack x happyFail

action_354 x = happyTcHack x happyReduce_45

action_355 x = happyTcHack x happyReduce_59

action_356 (114#) = happyShift action_361
action_356 x = happyTcHack x happyReduce_60

action_357 (129#) = happyShift action_40
action_357 (131#) = happyShift action_42
action_357 (57#) = happyGoto action_360
action_357 x = happyTcHack x happyFail

action_358 (127#) = happyShift action_359
action_358 x = happyTcHack x happyFail

action_359 x = happyTcHack x happyReduce_46

action_360 (113#) = happyShift action_378
action_360 (120#) = happyShift action_379
action_360 (121#) = happyShift action_15
action_360 (122#) = happyShift action_16
action_360 (128#) = happyShift action_17
action_360 (129#) = happyShift action_18
action_360 (130#) = happyShift action_19
action_360 (131#) = happyShift action_20
action_360 (133#) = happyShift action_21
action_360 (135#) = happyShift action_22
action_360 (34#) = happyGoto action_375
action_360 (35#) = happyGoto action_376
action_360 (45#) = happyGoto action_377
action_360 (49#) = happyGoto action_6
action_360 (60#) = happyGoto action_7
action_360 (62#) = happyGoto action_8
action_360 x = happyTcHack x happyReduce_68

action_361 (30#) = happyGoto action_374
action_361 (31#) = happyGoto action_356
action_361 (91#) = happyGoto action_357
action_361 x = happyTcHack x happyReduce_215

action_362 (122#) = happyShift action_75
action_362 (129#) = happyShift action_18
action_362 (131#) = happyShift action_20
action_362 (133#) = happyShift action_21
action_362 (135#) = happyShift action_22
action_362 (49#) = happyGoto action_6
action_362 (60#) = happyGoto action_372
action_362 (61#) = happyGoto action_373
action_362 x = happyTcHack x happyFail

action_363 x = happyTcHack x happyReduce_47

action_364 (120#) = happyShift action_14
action_364 (121#) = happyShift action_15
action_364 (122#) = happyShift action_16
action_364 (128#) = happyShift action_17
action_364 (129#) = happyShift action_18
action_364 (130#) = happyShift action_19
action_364 (131#) = happyShift action_20
action_364 (133#) = happyShift action_21
action_364 (135#) = happyShift action_22
action_364 (45#) = happyGoto action_371
action_364 (49#) = happyGoto action_6
action_364 (60#) = happyGoto action_7
action_364 (62#) = happyGoto action_8
action_364 x = happyTcHack x happyFail

action_365 (124#) = happyShift action_370
action_365 x = happyTcHack x happyFail

action_366 (127#) = happyShift action_369
action_366 x = happyTcHack x happyReduce_54

action_367 (113#) = happyShift action_37
action_367 (128#) = happyShift action_39
action_367 (130#) = happyShift action_41
action_367 (48#) = happyGoto action_30
action_367 (53#) = happyGoto action_368
action_367 x = happyTcHack x happyFail

action_368 (115#) = happyShift action_389
action_368 (119#) = happyShift action_390
action_368 x = happyTcHack x happyFail

action_369 (27#) = happyGoto action_388
action_369 (28#) = happyGoto action_366
action_369 (91#) = happyGoto action_367
action_369 x = happyTcHack x happyReduce_215

action_370 x = happyTcHack x happyReduce_53

action_371 x = happyTcHack x happyReduce_65

action_372 (116#) = happyShift action_387
action_372 x = happyTcHack x happyReduce_129

action_373 (126#) = happyShift action_386
action_373 x = happyTcHack x happyFail

action_374 x = happyTcHack x happyReduce_61

action_375 x = happyTcHack x happyReduce_62

action_376 (113#) = happyShift action_378
action_376 (120#) = happyShift action_14
action_376 (121#) = happyShift action_15
action_376 (122#) = happyShift action_16
action_376 (128#) = happyShift action_17
action_376 (129#) = happyShift action_18
action_376 (130#) = happyShift action_19
action_376 (131#) = happyShift action_20
action_376 (133#) = happyShift action_21
action_376 (135#) = happyShift action_22
action_376 (34#) = happyGoto action_385
action_376 (35#) = happyGoto action_376
action_376 (45#) = happyGoto action_377
action_376 (49#) = happyGoto action_6
action_376 (60#) = happyGoto action_7
action_376 (62#) = happyGoto action_8
action_376 x = happyTcHack x happyReduce_68

action_377 x = happyTcHack x happyReduce_70

action_378 (120#) = happyShift action_14
action_378 (121#) = happyShift action_15
action_378 (122#) = happyShift action_16
action_378 (128#) = happyShift action_17
action_378 (129#) = happyShift action_18
action_378 (130#) = happyShift action_19
action_378 (131#) = happyShift action_20
action_378 (133#) = happyShift action_21
action_378 (135#) = happyShift action_22
action_378 (45#) = happyGoto action_384
action_378 (49#) = happyGoto action_6
action_378 (60#) = happyGoto action_7
action_378 (62#) = happyGoto action_8
action_378 x = happyTcHack x happyFail

action_379 (113#) = happyShift action_37
action_379 (122#) = happyShift action_75
action_379 (128#) = happyShift action_39
action_379 (129#) = happyShift action_18
action_379 (130#) = happyShift action_41
action_379 (131#) = happyShift action_20
action_379 (133#) = happyShift action_21
action_379 (135#) = happyShift action_22
action_379 (36#) = happyGoto action_380
action_379 (37#) = happyGoto action_381
action_379 (48#) = happyGoto action_30
action_379 (49#) = happyGoto action_6
action_379 (53#) = happyGoto action_382
action_379 (56#) = happyGoto action_383
action_379 (60#) = happyGoto action_74
action_379 x = happyTcHack x happyFail

action_380 (124#) = happyShift action_397
action_380 x = happyTcHack x happyFail

action_381 (116#) = happyShift action_396
action_381 x = happyTcHack x happyReduce_72

action_382 (113#) = happyShift action_37
action_382 (128#) = happyShift action_39
action_382 (130#) = happyShift action_41
action_382 (48#) = happyGoto action_30
action_382 (53#) = happyGoto action_226
action_382 (55#) = happyGoto action_395
action_382 x = happyTcHack x happyReduce_116

action_383 (115#) = happyShift action_394
action_383 x = happyTcHack x happyFail

action_384 x = happyTcHack x happyReduce_71

action_385 x = happyTcHack x happyReduce_69

action_386 x = happyTcHack x happyReduce_67

action_387 (122#) = happyShift action_75
action_387 (129#) = happyShift action_18
action_387 (131#) = happyShift action_20
action_387 (133#) = happyShift action_21
action_387 (135#) = happyShift action_22
action_387 (49#) = happyGoto action_6
action_387 (60#) = happyGoto action_372
action_387 (61#) = happyGoto action_393
action_387 x = happyTcHack x happyFail

action_388 x = happyTcHack x happyReduce_55

action_389 (112#) = happyShift action_13
action_389 (120#) = happyShift action_14
action_389 (121#) = happyShift action_15
action_389 (122#) = happyShift action_16
action_389 (128#) = happyShift action_17
action_389 (129#) = happyShift action_18
action_389 (130#) = happyShift action_19
action_389 (131#) = happyShift action_20
action_389 (133#) = happyShift action_21
action_389 (135#) = happyShift action_22
action_389 (38#) = happyGoto action_392
action_389 (44#) = happyGoto action_4
action_389 (45#) = happyGoto action_5
action_389 (49#) = happyGoto action_6
action_389 (60#) = happyGoto action_7
action_389 (62#) = happyGoto action_8
action_389 x = happyTcHack x happyFail

action_390 (115#) = happyShift action_391
action_390 x = happyTcHack x happyFail

action_391 (112#) = happyShift action_13
action_391 (120#) = happyShift action_14
action_391 (121#) = happyShift action_15
action_391 (122#) = happyShift action_16
action_391 (128#) = happyShift action_17
action_391 (129#) = happyShift action_18
action_391 (130#) = happyShift action_19
action_391 (131#) = happyShift action_20
action_391 (133#) = happyShift action_21
action_391 (135#) = happyShift action_22
action_391 (38#) = happyGoto action_401
action_391 (44#) = happyGoto action_4
action_391 (45#) = happyGoto action_5
action_391 (49#) = happyGoto action_6
action_391 (60#) = happyGoto action_7
action_391 (62#) = happyGoto action_8
action_391 x = happyTcHack x happyFail

action_392 x = happyTcHack x happyReduce_56

action_393 x = happyTcHack x happyReduce_130

action_394 (112#) = happyShift action_13
action_394 (113#) = happyShift action_400
action_394 (120#) = happyShift action_14
action_394 (121#) = happyShift action_15
action_394 (122#) = happyShift action_16
action_394 (128#) = happyShift action_17
action_394 (129#) = happyShift action_18
action_394 (130#) = happyShift action_19
action_394 (131#) = happyShift action_20
action_394 (133#) = happyShift action_21
action_394 (135#) = happyShift action_22
action_394 (38#) = happyGoto action_399
action_394 (44#) = happyGoto action_4
action_394 (45#) = happyGoto action_5
action_394 (49#) = happyGoto action_6
action_394 (60#) = happyGoto action_7
action_394 (62#) = happyGoto action_8
action_394 x = happyTcHack x happyFail

action_395 x = happyTcHack x happyReduce_118

action_396 (113#) = happyShift action_37
action_396 (128#) = happyShift action_39
action_396 (130#) = happyShift action_41
action_396 (36#) = happyGoto action_398
action_396 (37#) = happyGoto action_381
action_396 (48#) = happyGoto action_30
action_396 (53#) = happyGoto action_382
action_396 (56#) = happyGoto action_383
action_396 x = happyTcHack x happyFail

action_397 x = happyTcHack x happyReduce_63

action_398 x = happyTcHack x happyReduce_73

action_399 x = happyTcHack x happyReduce_74

action_400 (112#) = happyShift action_13
action_400 (120#) = happyShift action_14
action_400 (121#) = happyShift action_15
action_400 (122#) = happyShift action_16
action_400 (128#) = happyShift action_17
action_400 (129#) = happyShift action_18
action_400 (130#) = happyShift action_19
action_400 (131#) = happyShift action_20
action_400 (133#) = happyShift action_21
action_400 (135#) = happyShift action_22
action_400 (38#) = happyGoto action_402
action_400 (44#) = happyGoto action_4
action_400 (45#) = happyGoto action_5
action_400 (49#) = happyGoto action_6
action_400 (60#) = happyGoto action_7
action_400 (62#) = happyGoto action_8
action_400 x = happyTcHack x happyFail

action_401 x = happyTcHack x happyReduce_57

action_402 x = happyTcHack x happyReduce_75

happyReduce_1 = happySpecReduce_1 1# reduction where {
  reduction
	(HappyAbsSyn2  happy_var_1)
	 =  HappyAbsSyn1
		 (PIface  happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_2 = happySpecReduce_1 1# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn1
		 (PType   happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_3 = happySpecReduce_1 1# reduction where {
  reduction
	(HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn1
		 (PIdInfo happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_4 = happyReduce 10# 2# reduction where {
  reduction
	((HappyAbsSyn21  happy_var_10) :
	(HappyAbsSyn68  happy_var_9) :
	(HappyAbsSyn18  happy_var_8) :
	(HappyAbsSyn10  happy_var_7) :
	(HappyAbsSyn3  happy_var_6) :
	(HappyAbsSyn16  happy_var_5) :
	_ :
	(HappyTerminal (ITinteger  happy_var_3)) :
	(HappyTerminal (ITconid  	 happy_var_2)) :
	_ :
	happyRest)
	 = HappyAbsSyn2
		 (ParsedIface 
			happy_var_2 			-- Module name
			(fromInteger happy_var_3) 	-- Module version
			happy_var_6  		        -- Usages
			happy_var_7  		        -- Exports
			happy_var_5  		        -- Instance modules
			happy_var_8  		        -- Fixities
			happy_var_10  		        -- Decls
			happy_var_9 			-- Local instances
) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_5 = happySpecReduce_2 3# reduction where {
  reduction
	(HappyAbsSyn3  happy_var_2)
	_
	 =  HappyAbsSyn3
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_6 = happySpecReduce_0 3# reduction where {
  reduction
	 =  HappyAbsSyn3
		 ([])}

happyReduce_7 = happySpecReduce_0 4# reduction where {
  reduction
	 =  HappyAbsSyn3
		 ([])}

happyReduce_8 = happySpecReduce_2 4# reduction where {
  reduction
	(HappyAbsSyn3  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn3
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_9 = happyReduce 6# 5# reduction where {
  reduction
	(_ :
	(HappyAbsSyn6  happy_var_5) :
	_ :
	(HappyTerminal (ITinteger  happy_var_3)) :
	(HappyAbsSyn12  happy_var_2) :
	(HappyAbsSyn47  happy_var_1) :
	happyRest)
	 = HappyAbsSyn5
		 ((happy_var_1, happy_var_2, fromInteger happy_var_3, happy_var_5)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_10 = happySpecReduce_0 6# reduction where {
  reduction
	 =  HappyAbsSyn6
		 (Everything)}

happyReduce_11 = happySpecReduce_2 6# reduction where {
  reduction
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (Specifically (happy_var_1:happy_var_2));
  reduction _ _  = notHappyAtAll }

happyReduce_12 = happySpecReduce_2 7# reduction where {
  reduction
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_13 = happySpecReduce_0 7# reduction where {
  reduction
	 =  HappyAbsSyn7
		 ([])}

happyReduce_14 = happySpecReduce_0 8# reduction where {
  reduction
	 =  HappyAbsSyn7
		 ([])}

happyReduce_15 = happySpecReduce_2 8# reduction where {
  reduction
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_16 = happySpecReduce_2 9# reduction where {
  reduction
	(HappyTerminal (ITinteger  happy_var_2))
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn9
		 ((happy_var_1, fromInteger happy_var_2)
--------------------------------------------------------------------------
);
  reduction _ _  = notHappyAtAll }

happyReduce_17 = happySpecReduce_2 10# reduction where {
  reduction
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_18 = happySpecReduce_0 10# reduction where {
  reduction
	 =  HappyAbsSyn10
		 ([])}

happyReduce_19 = happySpecReduce_0 11# reduction where {
  reduction
	 =  HappyAbsSyn10
		 ([])}

happyReduce_20 = happyReduce 5# 11# reduction where {
  reduction
	((HappyAbsSyn10  happy_var_5) :
	_ :
	(HappyAbsSyn13  happy_var_3) :
	(HappyAbsSyn47  happy_var_2) :
	(HappyAbsSyn12  happy_var_1) :
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_2,happy_var_1,happy_var_3) : happy_var_5) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_21 = happySpecReduce_0 12# reduction where {
  reduction
	 =  HappyAbsSyn12
		 (HiFile)}

happyReduce_22 = happySpecReduce_1 12# reduction where {
  reduction
	_
	 =  HappyAbsSyn12
		 (HiBootFile)}

happyReduce_23 = happySpecReduce_0 13# reduction where {
  reduction
	 =  HappyAbsSyn13
		 ([])}

happyReduce_24 = happySpecReduce_2 13# reduction where {
  reduction
	(HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_25 = happySpecReduce_1 14# reduction where {
  reduction
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn14
		 (if isTCOcc happy_var_1 
							  then AvailTC happy_var_1 [happy_var_1]
							  else Avail happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_26 = happySpecReduce_2 14# reduction where {
  reduction
	(HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn14
		 (AvailTC happy_var_1 (happy_var_1:happy_var_2));
  reduction _ _  = notHappyAtAll }

happyReduce_27 = happySpecReduce_3 14# reduction where {
  reduction
	(HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn14
		 (AvailTC happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_28 = happySpecReduce_3 15# reduction where {
  reduction
	_
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
--------------------------------------------------------------------------
);
  reduction _ _ _  = notHappyAtAll }

happyReduce_29 = happySpecReduce_0 16# reduction where {
  reduction
	 =  HappyAbsSyn16
		 ([])}

happyReduce_30 = happySpecReduce_2 16# reduction where {
  reduction
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_31 = happySpecReduce_0 17# reduction where {
  reduction
	 =  HappyAbsSyn16
		 ([])}

happyReduce_32 = happySpecReduce_2 17# reduction where {
  reduction
	(HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_2
--------------------------------------------------------------------------
);
  reduction _ _  = notHappyAtAll }

happyReduce_33 = happySpecReduce_0 18# reduction where {
  reduction
	 =  HappyAbsSyn18
		 ([])}

happyReduce_34 = happySpecReduce_2 18# reduction where {
  reduction
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_35 = happySpecReduce_0 19# reduction where {
  reduction
	 =  HappyAbsSyn18
		 ([])}

happyReduce_36 = happySpecReduce_2 19# reduction where {
  reduction
	(HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_37 = happyReduce 4# 20# reduction where {
  reduction
	(_ :
	(HappyAbsSyn48  happy_var_3) :
	(HappyTerminal (ITinteger  happy_var_2)) :
	_ :
	happyRest)
	 = HappyAbsSyn20
		 ((happy_var_3, Fixity (fromInteger happy_var_2) InfixL)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_38 = happyReduce 4# 20# reduction where {
  reduction
	(_ :
	(HappyAbsSyn48  happy_var_3) :
	(HappyTerminal (ITinteger  happy_var_2)) :
	_ :
	happyRest)
	 = HappyAbsSyn20
		 ((happy_var_3, Fixity (fromInteger happy_var_2) InfixR)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_39 = happyReduce 4# 20# reduction where {
  reduction
	(_ :
	(HappyAbsSyn48  happy_var_3) :
	(HappyTerminal (ITinteger  happy_var_2)) :
	_ :
	happyRest)
	 = HappyAbsSyn20
		 ((happy_var_3, Fixity (fromInteger happy_var_2) InfixN)
--------------------------------------------------------------------------
) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_40 = happySpecReduce_0 21# reduction where {
  reduction
	 =  HappyAbsSyn21
		 ([])}

happyReduce_41 = happySpecReduce_2 21# reduction where {
  reduction
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_42 = happySpecReduce_0 22# reduction where {
  reduction
	 =  HappyAbsSyn21
		 ([])}

happyReduce_43 = happySpecReduce_3 22# reduction where {
  reduction
	(HappyAbsSyn21  happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn21
		 ((happy_var_1,happy_var_2) : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_44 = happySpecReduce_1 23# reduction where {
  reduction
	(HappyTerminal (ITinteger  happy_var_1))
	 =  HappyAbsSyn23
		 (fromInteger happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_45 = happyReduce 7# 24# reduction where {
  reduction
	(_ :
	(HappyAbsSyn38  happy_var_6) :
	_ :
	(HappyAbsSyn39  happy_var_4) :
	(HappyAbsSyn53  happy_var_3) :
	_ :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn24
		 (TyD (TySynonym happy_var_3 happy_var_4 happy_var_6 happy_var_1)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_46 = happyReduce 8# 24# reduction where {
  reduction
	(_ :
	(HappyAbsSyn33  happy_var_7) :
	(HappyAbsSyn29  happy_var_6) :
	(HappyAbsSyn39  happy_var_5) :
	(HappyAbsSyn53  happy_var_4) :
	(HappyAbsSyn25  happy_var_3) :
	_ :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn24
		 (TyD (TyData DataType happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 noDataPragmas happy_var_1)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_47 = happyReduce 8# 24# reduction where {
  reduction
	(_ :
	(HappyAbsSyn33  happy_var_7) :
	(HappyAbsSyn32  happy_var_6) :
	(HappyAbsSyn39  happy_var_5) :
	(HappyAbsSyn53  happy_var_4) :
	(HappyAbsSyn25  happy_var_3) :
	_ :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn24
		 (TyD (TyData NewType happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 noDataPragmas happy_var_1)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_48 = happyReduce 7# 24# reduction where {
  reduction
	(_ :
	(HappyAbsSyn26  happy_var_6) :
	(HappyAbsSyn39  happy_var_5) :
	(HappyAbsSyn53  happy_var_4) :
	(HappyAbsSyn25  happy_var_3) :
	_ :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn24
		 (ClD (mkClassDecl happy_var_3 happy_var_4 happy_var_5 happy_var_6 EmptyMonoBinds noClassPragmas happy_var_1)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_49 = happySpecReduce_3 24# reduction where {
  reduction
	(HappyTerminal happy_var_3)
	(HappyAbsSyn53  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn24
		 (
			 case happy_var_3 of
			    ITtysig sig idinfo_part ->	-- Parse type and idinfo lazily
				let info = 
				      case idinfo_part of
					Nothing -> []
					Just s  -> case parseIface s happy_var_1 of 
						     Succeeded (PIdInfo id_info) -> id_info
						     other ->  pprPanic "IdInfo parse failed"
							      	        (ppr happy_var_2)

				    tp = case parseIface sig happy_var_1 of
					    Succeeded (PType tp) -> tp
					    other -> pprPanic "Id type parse failed"
							      (ppr happy_var_2)
				 in
  			 	 SigD (IfaceSig happy_var_2 tp info happy_var_1));
  reduction _ _ _  = notHappyAtAll }

happyReduce_50 = happySpecReduce_0 25# reduction where {
  reduction
	 =  HappyAbsSyn25
		 ([])}

happyReduce_51 = happyReduce 4# 25# reduction where {
  reduction
	(_ :
	_ :
	(HappyAbsSyn25  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn25
		 (happy_var_2) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_52 = happySpecReduce_0 26# reduction where {
  reduction
	 =  HappyAbsSyn26
		 ([])}

happyReduce_53 = happyReduce 4# 26# reduction where {
  reduction
	(_ :
	(HappyAbsSyn26  happy_var_3) :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn26
		 (happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_54 = happySpecReduce_1 27# reduction where {
  reduction
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn26
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_55 = happySpecReduce_3 27# reduction where {
  reduction
	(HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_56 = happyReduce 4# 28# reduction where {
  reduction
	((HappyAbsSyn38  happy_var_4) :
	_ :
	(HappyAbsSyn53  happy_var_2) :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn28
		 (ClassOpSig happy_var_2 Nothing happy_var_4 happy_var_1) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_57 = happyReduce 5# 28# reduction where {
  reduction
	((HappyAbsSyn38  happy_var_5) :
	_ :
	_ :
	(HappyAbsSyn53  happy_var_2) :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn28
		 (ClassOpSig happy_var_2 
								(Just (error "Un-filled-in default method"))
								happy_var_5 happy_var_1) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_58 = happySpecReduce_0 29# reduction where {
  reduction
	 =  HappyAbsSyn29
		 ([])}

happyReduce_59 = happySpecReduce_2 29# reduction where {
  reduction
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_60 = happySpecReduce_1 30# reduction where {
  reduction
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_61 = happySpecReduce_3 30# reduction where {
  reduction
	(HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_62 = happySpecReduce_3 31# reduction where {
  reduction
	(HappyAbsSyn34  happy_var_3)
	(HappyAbsSyn53  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn31
		 (ConDecl happy_var_2 [] (VanillaCon happy_var_3) happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_63 = happyReduce 5# 31# reduction where {
  reduction
	(_ :
	(HappyAbsSyn36  happy_var_4) :
	_ :
	(HappyAbsSyn53  happy_var_2) :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn31
		 (ConDecl happy_var_2 [] (RecCon happy_var_4)     happy_var_1) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_64 = happySpecReduce_0 32# reduction where {
  reduction
	 =  HappyAbsSyn32
		 ([])}

happyReduce_65 = happyReduce 4# 32# reduction where {
  reduction
	((HappyAbsSyn38  happy_var_4) :
	(HappyAbsSyn53  happy_var_3) :
	_ :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn32
		 ([ConDecl happy_var_3 [] (NewCon happy_var_4) happy_var_1]) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_66 = happySpecReduce_0 33# reduction where {
  reduction
	 =  HappyAbsSyn33
		 (Nothing)}

happyReduce_67 = happyReduce 4# 33# reduction where {
  reduction
	(_ :
	(HappyAbsSyn55  happy_var_3) :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn33
		 (Just happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_68 = happySpecReduce_0 34# reduction where {
  reduction
	 =  HappyAbsSyn34
		 ([])}

happyReduce_69 = happySpecReduce_2 34# reduction where {
  reduction
	(HappyAbsSyn34  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_70 = happySpecReduce_1 35# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn35
		 (Unbanged happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_71 = happySpecReduce_2 35# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (Banged   happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_72 = happySpecReduce_1 36# reduction where {
  reduction
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_73 = happySpecReduce_3 36# reduction where {
  reduction
	(HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_74 = happySpecReduce_3 37# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn37
		 ((happy_var_1, Unbanged happy_var_3));
  reduction _ _ _  = notHappyAtAll }

happyReduce_75 = happyReduce 4# 37# reduction where {
  reduction
	((HappyAbsSyn38  happy_var_4) :
	_ :
	_ :
	(HappyAbsSyn55  happy_var_1) :
	happyRest)
	 = HappyAbsSyn37
		 ((happy_var_1, Banged   happy_var_4)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_76 = happyReduce 5# 38# reduction where {
  reduction
	((HappyAbsSyn38  happy_var_5) :
	_ :
	(HappyAbsSyn25  happy_var_3) :
	(HappyAbsSyn39  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn38
		 (mkHsForAllTy happy_var_2 happy_var_3 happy_var_5) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_77 = happySpecReduce_3 38# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (MonoFunTy happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_78 = happySpecReduce_1 38# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_79 = happySpecReduce_3 39# reduction where {
  reduction
	_
	(HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_80 = happySpecReduce_0 40# reduction where {
  reduction
	 =  HappyAbsSyn25
		 ([])}

happyReduce_81 = happySpecReduce_3 40# reduction where {
  reduction
	_
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_82 = happySpecReduce_1 41# reduction where {
  reduction
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_83 = happySpecReduce_3 41# reduction where {
  reduction
	(HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_84 = happySpecReduce_2 42# reduction where {
  reduction
	(HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn42
		 ((happy_var_1, happy_var_2));
  reduction _ _  = notHappyAtAll }

happyReduce_85 = happySpecReduce_3 43# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn43
		 ([happy_var_1,happy_var_3]);
  reduction _ _ _  = notHappyAtAll }

happyReduce_86 = happySpecReduce_3 43# reduction where {
  reduction
	(HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_87 = happySpecReduce_1 44# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_88 = happySpecReduce_2 44# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_2)
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (MonoTyApp happy_var_1 happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_89 = happySpecReduce_1 45# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn38
		 (MonoTyVar happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_90 = happySpecReduce_1 45# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn38
		 (MonoTyVar happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_91 = happySpecReduce_3 45# reduction where {
  reduction
	_
	(HappyAbsSyn43  happy_var_2)
	_
	 =  HappyAbsSyn38
		 (MonoTupleTy dummyRdrTcName happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_92 = happySpecReduce_3 45# reduction where {
  reduction
	_
	(HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn38
		 (MonoListTy  dummyRdrTcName happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_93 = happyReduce 4# 45# reduction where {
  reduction
	(_ :
	(HappyAbsSyn46  happy_var_3) :
	(HappyAbsSyn53  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn38
		 (MonoDictTy happy_var_2 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_94 = happySpecReduce_3 45# reduction where {
  reduction
	_
	(HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn38
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_95 = happySpecReduce_0 46# reduction where {
  reduction
	 =  HappyAbsSyn46
		 ([])}

happyReduce_96 = happySpecReduce_2 46# reduction where {
  reduction
	(HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_97 = happySpecReduce_1 47# reduction where {
  reduction
	(HappyTerminal (ITconid  	 happy_var_1))
	 =  HappyAbsSyn47
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_98 = happySpecReduce_1 48# reduction where {
  reduction
	(HappyTerminal (ITvarid  	 happy_var_1))
	 =  HappyAbsSyn48
		 (VarOcc happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_99 = happySpecReduce_1 48# reduction where {
  reduction
	(HappyTerminal (ITvarsym 	 happy_var_1))
	 =  HappyAbsSyn48
		 (VarOcc happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_100 = happySpecReduce_1 48# reduction where {
  reduction
	_
	 =  HappyAbsSyn48
		 (VarOcc SLIT("!") {-sigh, double-sigh-})}

happyReduce_101 = happySpecReduce_1 49# reduction where {
  reduction
	(HappyTerminal (ITconid  	 happy_var_1))
	 =  HappyAbsSyn48
		 (TCOcc happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_102 = happySpecReduce_1 49# reduction where {
  reduction
	(HappyTerminal (ITconsym 	 happy_var_1))
	 =  HappyAbsSyn48
		 (TCOcc happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_103 = happySpecReduce_3 49# reduction where {
  reduction
	_
	_
	_
	 =  HappyAbsSyn48
		 (TCOcc SLIT("->"))}

happyReduce_104 = happySpecReduce_1 50# reduction where {
  reduction
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_105 = happySpecReduce_1 50# reduction where {
  reduction
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_106 = happySpecReduce_1 50# reduction where {
  reduction
	_
	 =  HappyAbsSyn48
		 (TCOcc SLIT("->") {- Allow un-paren'd arrow -})}

happyReduce_107 = happySpecReduce_1 51# reduction where {
  reduction
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_108 = happySpecReduce_1 51# reduction where {
  reduction
	(HappyTerminal (ITconid  	 happy_var_1))
	 =  HappyAbsSyn48
		 (VarOcc happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_109 = happySpecReduce_1 51# reduction where {
  reduction
	(HappyTerminal (ITconsym 	 happy_var_1))
	 =  HappyAbsSyn48
		 (VarOcc happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_110 = happySpecReduce_1 52# reduction where {
  reduction
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_111 = happySpecReduce_2 52# reduction where {
  reduction
	(HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_112 = happySpecReduce_1 53# reduction where {
  reduction
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn53
		 (Unqual happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_113 = happySpecReduce_1 54# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_114 = happySpecReduce_1 54# reduction where {
  reduction
	(HappyTerminal (ITqvarid   happy_var_1))
	 =  HappyAbsSyn53
		 (lexVarQual happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_115 = happySpecReduce_1 54# reduction where {
  reduction
	(HappyTerminal (ITqvarsym  happy_var_1))
	 =  HappyAbsSyn53
		 (lexVarQual happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_116 = happySpecReduce_0 55# reduction where {
  reduction
	 =  HappyAbsSyn55
		 ([])}

happyReduce_117 = happySpecReduce_2 55# reduction where {
  reduction
	(HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_118 = happySpecReduce_2 56# reduction where {
  reduction
	(HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_119 = happySpecReduce_1 57# reduction where {
  reduction
	(HappyTerminal (ITconid  	 happy_var_1))
	 =  HappyAbsSyn53
		 (Unqual (VarOcc happy_var_1));
  reduction _  = notHappyAtAll }

happyReduce_120 = happySpecReduce_1 57# reduction where {
  reduction
	(HappyTerminal (ITconsym 	 happy_var_1))
	 =  HappyAbsSyn53
		 (Unqual (VarOcc happy_var_1));
  reduction _  = notHappyAtAll }

happyReduce_121 = happySpecReduce_1 58# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_122 = happySpecReduce_1 58# reduction where {
  reduction
	(HappyTerminal (ITqconid   happy_var_1))
	 =  HappyAbsSyn53
		 (lexVarQual happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_123 = happySpecReduce_1 58# reduction where {
  reduction
	(HappyTerminal (ITqconsym  happy_var_1))
	 =  HappyAbsSyn53
		 (lexVarQual happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_124 = happySpecReduce_0 59# reduction where {
  reduction
	 =  HappyAbsSyn55
		 ([])}

happyReduce_125 = happySpecReduce_2 59# reduction where {
  reduction
	(HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_126 = happySpecReduce_1 60# reduction where {
  reduction
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn53
		 (Unqual happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_127 = happySpecReduce_1 60# reduction where {
  reduction
	(HappyTerminal (ITqconid   happy_var_1))
	 =  HappyAbsSyn53
		 (lexTcQual happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_128 = happySpecReduce_1 60# reduction where {
  reduction
	(HappyTerminal (ITqconsym  happy_var_1))
	 =  HappyAbsSyn53
		 (lexTcQual happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_129 = happySpecReduce_1 61# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn55
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_130 = happySpecReduce_3 61# reduction where {
  reduction
	(HappyAbsSyn55  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_131 = happySpecReduce_1 62# reduction where {
  reduction
	(HappyTerminal (ITvarid  	 happy_var_1))
	 =  HappyAbsSyn53
		 (Unqual (TvOcc happy_var_1));
  reduction _  = notHappyAtAll }

happyReduce_132 = happySpecReduce_1 62# reduction where {
  reduction
	(HappyTerminal (ITvarsym 	 happy_var_1))
	 =  HappyAbsSyn53
		 (Unqual (TvOcc happy_var_1) {- Allow t2 as a tyvar -});
  reduction _  = notHappyAtAll }

happyReduce_133 = happySpecReduce_0 63# reduction where {
  reduction
	 =  HappyAbsSyn55
		 ([])}

happyReduce_134 = happySpecReduce_2 63# reduction where {
  reduction
	(HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_135 = happySpecReduce_3 64# reduction where {
  reduction
	(HappyAbsSyn66  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn64
		 (IfaceTyVar happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_136 = happySpecReduce_1 64# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn64
		 (UserTyVar happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_137 = happySpecReduce_0 65# reduction where {
  reduction
	 =  HappyAbsSyn39
		 ([])}

happyReduce_138 = happySpecReduce_2 65# reduction where {
  reduction
	(HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_139 = happySpecReduce_1 66# reduction where {
  reduction
	(HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn66
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_140 = happySpecReduce_3 66# reduction where {
  reduction
	(HappyAbsSyn66  happy_var_3)
	_
	(HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn66
		 (mkArrowKind happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_141 = happySpecReduce_1 67# reduction where {
  reduction
	(HappyTerminal (ITvarsym 	 happy_var_1))
	 =  HappyAbsSyn66
		 (if happy_var_1 == SLIT("*") then
						mkBoxedTypeKind
					  else if happy_var_1 == SLIT("**") then
						mkTypeKind
					  else panic "ParseInterface: akind"
);
  reduction _  = notHappyAtAll }

happyReduce_142 = happySpecReduce_3 67# reduction where {
  reduction
	_
	(HappyAbsSyn66  happy_var_2)
	_
	 =  HappyAbsSyn66
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_143 = happySpecReduce_2 68# reduction where {
  reduction
	(HappyAbsSyn68  happy_var_2)
	_
	 =  HappyAbsSyn68
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_144 = happySpecReduce_0 68# reduction where {
  reduction
	 =  HappyAbsSyn68
		 ([])}

happyReduce_145 = happySpecReduce_0 69# reduction where {
  reduction
	 =  HappyAbsSyn68
		 ([])}

happyReduce_146 = happySpecReduce_2 69# reduction where {
  reduction
	(HappyAbsSyn68  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn68
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_147 = happyReduce 6# 70# reduction where {
  reduction
	(_ :
	(HappyAbsSyn53  happy_var_5) :
	_ :
	(HappyAbsSyn38  happy_var_3) :
	_ :
	(HappyAbsSyn91  happy_var_1) :
	happyRest)
	 = HappyAbsSyn70
		 (InstDecl happy_var_3
				   EmptyMonoBinds	{- No bindings -}
				   []    		{- No user pragmas -}
				   (Just happy_var_5)		{- Dfun id -}
				   happy_var_1
) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_148 = happySpecReduce_0 71# reduction where {
  reduction
	 =  HappyAbsSyn71
		 ([])}

happyReduce_149 = happySpecReduce_2 71# reduction where {
  reduction
	(HappyAbsSyn71  happy_var_2)
	(HappyAbsSyn72  happy_var_1)
	 =  HappyAbsSyn71
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_150 = happySpecReduce_2 72# reduction where {
  reduction
	(HappyAbsSyn74  happy_var_2)
	_
	 =  HappyAbsSyn72
		 (HsArity happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_151 = happySpecReduce_1 72# reduction where {
  reduction
	(HappyAbsSyn75  happy_var_1)
	 =  HappyAbsSyn72
		 (HsStrictness happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_152 = happySpecReduce_1 72# reduction where {
  reduction
	_
	 =  HappyAbsSyn72
		 (HsStrictness HsBottom)}

happyReduce_153 = happySpecReduce_2 72# reduction where {
  reduction
	(HappyAbsSyn76  happy_var_2)
	(HappyTerminal (ITunfold happy_var_1))
	 =  HappyAbsSyn72
		 (HsUnfold happy_var_1 happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_154 = happyReduce 5# 72# reduction where {
  reduction
	((HappyAbsSyn76  happy_var_5) :
	_ :
	(HappyAbsSyn46  happy_var_3) :
	(HappyAbsSyn39  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn72
		 (HsSpecialise happy_var_2 happy_var_3 happy_var_5) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_155 = happySpecReduce_3 73# reduction where {
  reduction
	_
	(HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_156 = happySpecReduce_1 74# reduction where {
  reduction
	(HappyTerminal (ITinteger  happy_var_1))
	 =  HappyAbsSyn74
		 (exactArity (fromInteger happy_var_1));
  reduction _  = notHappyAtAll }

happyReduce_157 = happyReduce 5# 75# reduction where {
  reduction
	(_ :
	(HappyAbsSyn55  happy_var_4) :
	_ :
	(HappyAbsSyn53  happy_var_2) :
	(HappyTerminal (ITstrict happy_var_1)) :
	happyRest)
	 = HappyAbsSyn75
		 (HsStrictnessInfo happy_var_1 (Just (happy_var_2,happy_var_4))) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_158 = happySpecReduce_2 75# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_2)
	(HappyTerminal (ITstrict happy_var_1))
	 =  HappyAbsSyn75
		 (HsStrictnessInfo happy_var_1 (Just (happy_var_2,[])));
  reduction _ _  = notHappyAtAll }

happyReduce_159 = happySpecReduce_1 75# reduction where {
  reduction
	(HappyTerminal (ITstrict happy_var_1))
	 =  HappyAbsSyn75
		 (HsStrictnessInfo happy_var_1 Nothing);
  reduction _  = notHappyAtAll }

happyReduce_160 = happySpecReduce_1 76# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn76
		 (UfVar happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_161 = happySpecReduce_1 76# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn76
		 (UfVar happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_162 = happySpecReduce_1 76# reduction where {
  reduction
	(HappyAbsSyn84  happy_var_1)
	 =  HappyAbsSyn76
		 (UfLit happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_163 = happySpecReduce_3 76# reduction where {
  reduction
	_
	(HappyAbsSyn76  happy_var_2)
	_
	 =  HappyAbsSyn76
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_164 = happyReduce 4# 76# reduction where {
  reduction
	(_ :
	(HappyAbsSyn82  happy_var_3) :
	_ :
	(HappyAbsSyn53  happy_var_1) :
	happyRest)
	 = HappyAbsSyn76
		 (UfCon happy_var_1 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_165 = happySpecReduce_3 76# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn76
		 (UfApp happy_var_1 (UfTyArg happy_var_3));
  reduction _ _ _  = notHappyAtAll }

happyReduce_166 = happySpecReduce_2 76# reduction where {
  reduction
	(HappyAbsSyn81  happy_var_2)
	(HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn76
		 (UfApp happy_var_1 happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_167 = happyReduce 4# 76# reduction where {
  reduction
	((HappyAbsSyn76  happy_var_4) :
	_ :
	(HappyAbsSyn86  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn76
		 (foldr UfLam happy_var_4 happy_var_2) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_168 = happyReduce 4# 76# reduction where {
  reduction
	((HappyAbsSyn76  happy_var_4) :
	_ :
	(HappyAbsSyn86  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn76
		 (foldr UfLam happy_var_4 happy_var_2) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_169 = happyReduce 7# 76# reduction where {
  reduction
	(_ :
	(HappyAbsSyn80  happy_var_6) :
	(HappyAbsSyn79  happy_var_5) :
	_ :
	_ :
	(HappyAbsSyn76  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn76
		 (UfCase happy_var_2 (UfAlgAlts  happy_var_5 happy_var_6)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_170 = happyReduce 7# 76# reduction where {
  reduction
	(_ :
	(HappyAbsSyn80  happy_var_6) :
	(HappyAbsSyn78  happy_var_5) :
	_ :
	_ :
	(HappyAbsSyn76  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn76
		 (UfCase happy_var_2 (UfPrimAlts happy_var_5 happy_var_6)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_171 = happyReduce 8# 76# reduction where {
  reduction
	((HappyAbsSyn76  happy_var_8) :
	_ :
	_ :
	(HappyAbsSyn76  happy_var_5) :
	_ :
	(HappyAbsSyn85  happy_var_3) :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn76
		 (UfLet (UfNonRec happy_var_3 happy_var_5) happy_var_8) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_172 = happyReduce 6# 76# reduction where {
  reduction
	((HappyAbsSyn76  happy_var_6) :
	_ :
	_ :
	(HappyAbsSyn77  happy_var_3) :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn76
		 (UfLet (UfRec happy_var_3) happy_var_6) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_173 = happyReduce 7# 76# reduction where {
  reduction
	((HappyAbsSyn82  happy_var_7) :
	_ :
	(HappyAbsSyn46  happy_var_5) :
	(HappyAbsSyn38  happy_var_4) :
	_ :
	(HappyAbsSyn89  happy_var_2) :
	(HappyTerminal (ITccall happy_var_1)) :
	happyRest)
	 = HappyAbsSyn76
		 (let
									(is_casm, may_gc) = happy_var_1
								  in
								  UfPrim (UfCCallOp happy_var_2 is_casm may_gc happy_var_5 happy_var_4)
									 happy_var_7
) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_174 = happySpecReduce_2 76# reduction where {
  reduction
	(HappyAbsSyn76  happy_var_2)
	_
	 =  HappyAbsSyn76
		 (UfNote UfInlineCall happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_175 = happySpecReduce_3 76# reduction where {
  reduction
	(HappyAbsSyn76  happy_var_3)
	(HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn76
		 (UfNote (UfCoerce happy_var_2) happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_176 = happySpecReduce_2 76# reduction where {
  reduction
	(HappyAbsSyn76  happy_var_2)
	(HappyTerminal (ITscc happy_var_1))
	 =  HappyAbsSyn76
		 (UfNote (UfSCC happy_var_1) happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_177 = happySpecReduce_0 77# reduction where {
  reduction
	 =  HappyAbsSyn77
		 ([])}

happyReduce_178 = happyReduce 5# 77# reduction where {
  reduction
	((HappyAbsSyn77  happy_var_5) :
	_ :
	(HappyAbsSyn76  happy_var_3) :
	_ :
	(HappyAbsSyn85  happy_var_1) :
	happyRest)
	 = HappyAbsSyn77
		 ((happy_var_1,happy_var_3) : happy_var_5) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_179 = happySpecReduce_0 78# reduction where {
  reduction
	 =  HappyAbsSyn78
		 ([])}

happyReduce_180 = happyReduce 5# 78# reduction where {
  reduction
	((HappyAbsSyn78  happy_var_5) :
	_ :
	(HappyAbsSyn76  happy_var_3) :
	_ :
	(HappyAbsSyn84  happy_var_1) :
	happyRest)
	 = HappyAbsSyn78
		 ((happy_var_1,happy_var_3) : happy_var_5) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_181 = happySpecReduce_0 79# reduction where {
  reduction
	 =  HappyAbsSyn79
		 ([])}

happyReduce_182 = happyReduce 6# 79# reduction where {
  reduction
	((HappyAbsSyn79  happy_var_6) :
	_ :
	(HappyAbsSyn76  happy_var_4) :
	_ :
	(HappyAbsSyn55  happy_var_2) :
	(HappyAbsSyn53  happy_var_1) :
	happyRest)
	 = HappyAbsSyn79
		 ((happy_var_1,happy_var_2,happy_var_4) : happy_var_6) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_183 = happySpecReduce_0 80# reduction where {
  reduction
	 =  HappyAbsSyn80
		 (UfNoDefault)}

happyReduce_184 = happyReduce 4# 80# reduction where {
  reduction
	(_ :
	(HappyAbsSyn76  happy_var_3) :
	_ :
	(HappyAbsSyn53  happy_var_1) :
	happyRest)
	 = HappyAbsSyn80
		 (UfBindDefault happy_var_1 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_185 = happySpecReduce_1 81# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn81
		 (UfVarArg happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_186 = happySpecReduce_1 81# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn81
		 (UfVarArg happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_187 = happySpecReduce_1 81# reduction where {
  reduction
	(HappyAbsSyn84  happy_var_1)
	 =  HappyAbsSyn81
		 (UfLitArg happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_188 = happySpecReduce_0 82# reduction where {
  reduction
	 =  HappyAbsSyn82
		 ([])}

happyReduce_189 = happySpecReduce_2 82# reduction where {
  reduction
	(HappyAbsSyn82  happy_var_2)
	(HappyAbsSyn81  happy_var_1)
	 =  HappyAbsSyn82
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_190 = happySpecReduce_0 83# reduction where {
  reduction
	 =  HappyAbsSyn82
		 ([])}

happyReduce_191 = happySpecReduce_3 83# reduction where {
  reduction
	(HappyAbsSyn82  happy_var_3)
	(HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn82
		 (UfTyArg happy_var_2 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_192 = happySpecReduce_2 83# reduction where {
  reduction
	(HappyAbsSyn82  happy_var_2)
	(HappyAbsSyn81  happy_var_1)
	 =  HappyAbsSyn82
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_193 = happySpecReduce_1 84# reduction where {
  reduction
	(HappyTerminal (ITinteger  happy_var_1))
	 =  HappyAbsSyn84
		 (MachInt happy_var_1 True);
  reduction _  = notHappyAtAll }

happyReduce_194 = happySpecReduce_1 84# reduction where {
  reduction
	(HappyTerminal (ITchar happy_var_1))
	 =  HappyAbsSyn84
		 (MachChar happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_195 = happySpecReduce_1 84# reduction where {
  reduction
	(HappyTerminal (ITstring happy_var_1))
	 =  HappyAbsSyn84
		 (MachStr happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_196 = happySpecReduce_2 84# reduction where {
  reduction
	(HappyTerminal (ITstring happy_var_2))
	_
	 =  HappyAbsSyn84
		 (NoRepStr happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_197 = happySpecReduce_1 84# reduction where {
  reduction
	(HappyTerminal (ITrational happy_var_1))
	 =  HappyAbsSyn84
		 (MachDouble happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_198 = happySpecReduce_2 84# reduction where {
  reduction
	(HappyTerminal (ITrational happy_var_2))
	_
	 =  HappyAbsSyn84
		 (MachFloat happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_199 = happySpecReduce_2 84# reduction where {
  reduction
	(HappyTerminal (ITinteger  happy_var_2))
	_
	 =  HappyAbsSyn84
		 (NoRepInteger  happy_var_2 (panic "NoRepInteger type") 
							-- The type checker will add the types
);
  reduction _ _  = notHappyAtAll }

happyReduce_200 = happySpecReduce_3 84# reduction where {
  reduction
	(HappyTerminal (ITinteger  happy_var_3))
	(HappyTerminal (ITinteger  happy_var_2))
	_
	 =  HappyAbsSyn84
		 (NoRepRational (happy_var_2 % happy_var_3) 
								(panic "NoRepRational type")
									-- The type checker will add the type
);
  reduction _ _ _  = notHappyAtAll }

happyReduce_201 = happySpecReduce_2 84# reduction where {
  reduction
	(HappyTerminal (ITinteger  happy_var_2))
	_
	 =  HappyAbsSyn84
		 (MachAddr happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_202 = happySpecReduce_3 84# reduction where {
  reduction
	(HappyTerminal (ITstring happy_var_3))
	(HappyAbsSyn90  happy_var_2)
	_
	 =  HappyAbsSyn84
		 (MachLitLit happy_var_3 (decodePrimRep happy_var_2));
  reduction _ _ _  = notHappyAtAll }

happyReduce_203 = happySpecReduce_3 85# reduction where {
  reduction
	(HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn85
		 (UfValBinder happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_204 = happySpecReduce_0 86# reduction where {
  reduction
	 =  HappyAbsSyn86
		 ([])}

happyReduce_205 = happySpecReduce_2 86# reduction where {
  reduction
	(HappyAbsSyn86  happy_var_2)
	(HappyAbsSyn85  happy_var_1)
	 =  HappyAbsSyn86
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_206 = happySpecReduce_3 87# reduction where {
  reduction
	(HappyAbsSyn66  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn85
		 (UfTyBinder happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_207 = happySpecReduce_1 87# reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn85
		 (UfTyBinder happy_var_1 mkBoxedTypeKind);
  reduction _  = notHappyAtAll }

happyReduce_208 = happySpecReduce_0 88# reduction where {
  reduction
	 =  HappyAbsSyn86
		 ([])}

happyReduce_209 = happySpecReduce_2 88# reduction where {
  reduction
	(HappyAbsSyn86  happy_var_2)
	(HappyAbsSyn85  happy_var_1)
	 =  HappyAbsSyn86
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_210 = happySpecReduce_1 89# reduction where {
  reduction
	(HappyTerminal (ITstring happy_var_1))
	 =  HappyAbsSyn89
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_211 = happySpecReduce_1 89# reduction where {
  reduction
	(HappyTerminal (ITvarid  	 happy_var_1))
	 =  HappyAbsSyn89
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_212 = happySpecReduce_1 89# reduction where {
  reduction
	(HappyTerminal (ITconid  	 happy_var_1))
	 =  HappyAbsSyn89
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_213 = happySpecReduce_1 90# reduction where {
  reduction
	(HappyTerminal (ITvarid  	 happy_var_1))
	 =  HappyAbsSyn90
		 (head (_UNPK_ happy_var_1));
  reduction _  = notHappyAtAll }

happyReduce_214 = happySpecReduce_1 90# reduction where {
  reduction
	(HappyTerminal (ITconid  	 happy_var_1))
	 =  HappyAbsSyn90
		 (head (_UNPK_ happy_var_1));
  reduction _  = notHappyAtAll }

happyReduce_215 = happyMonadReduce 0# 91# HappyAbsSyn91 reduction where {
  reduction
	(happyRest)
	 =  getSrcLocIf}

happyReduce_216 = happyMonadReduce 0# 92# HappyAbsSyn92 reduction where {
  reduction
	(happyRest)
	 =  checkVersion Nothing}

happyReduce_217 = happyMonadReduce 1# 92# HappyAbsSyn92 reduction where {
  reduction
	((HappyTerminal (ITinteger  happy_var_1)) :
	happyRest)
	 =  checkVersion (Just (fromInteger happy_var_1));
  reduction _ = notHappyAtAll }

happyNewToken action sts stk
	= lexIface(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	ITeof -> action 166# 166# (error "reading EOF!") (HappyState action) sts stk;
	ITinterface -> cont 93#;
	ITusages -> cont 94#;
	ITversions -> cont 95#;
	ITexports -> cont 96#;
	ITinstance_modules -> cont 97#;
	ITinstances -> cont 98#;
	ITfixities -> cont 99#;
	ITdeclarations -> cont 100#;
	ITpragmas -> cont 101#;
	ITdata -> cont 102#;
	ITtype -> cont 103#;
	ITnewtype -> cont 104#;
	ITderiving -> cont 105#;
	ITclass -> cont 106#;
	ITwhere -> cont 107#;
	ITinstance -> cont 108#;
	ITinfixl -> cont 109#;
	ITinfixr -> cont 110#;
	ITinfix -> cont 111#;
	ITforall -> cont 112#;
	ITbang -> cont 113#;
	ITvbar -> cont 114#;
	ITdcolon -> cont 115#;
	ITcomma -> cont 116#;
	ITdarrow -> cont 117#;
	ITdotdot -> cont 118#;
	ITequal -> cont 119#;
	ITocurly -> cont 120#;
	ITobrack -> cont 121#;
	IToparen -> cont 122#;
	ITrarrow -> cont 123#;
	ITccurly -> cont 124#;
	ITcbrack -> cont 125#;
	ITcparen -> cont 126#;
	ITsemi -> cont 127#;
	ITvarid  	 happy_dollar_dollar -> cont 128#;
	ITconid  	 happy_dollar_dollar -> cont 129#;
	ITvarsym 	 happy_dollar_dollar -> cont 130#;
	ITconsym 	 happy_dollar_dollar -> cont 131#;
	ITqvarid   happy_dollar_dollar -> cont 132#;
	ITqconid   happy_dollar_dollar -> cont 133#;
	ITqvarsym  happy_dollar_dollar -> cont 134#;
	ITqconsym  happy_dollar_dollar -> cont 135#;
	ITstrict happy_dollar_dollar -> cont 136#;
	ITtysig _ _ -> cont 137#;
	ITarity -> cont 138#;
	ITunfold happy_dollar_dollar -> cont 139#;
	ITspecialise -> cont 140#;
	ITbottom -> cont 141#;
	ITlam -> cont 142#;
	ITbiglam -> cont 143#;
	ITcase -> cont 144#;
	ITprim_case -> cont 145#;
	ITlet -> cont 146#;
	ITletrec -> cont 147#;
	ITin -> cont 148#;
	ITof -> cont 149#;
	ITcoerce -> cont 150#;
	ITatsign -> cont 151#;
	ITccall happy_dollar_dollar -> cont 152#;
	ITscc happy_dollar_dollar -> cont 153#;
	ITinline -> cont 154#;
	ITchar happy_dollar_dollar -> cont 155#;
	ITstring happy_dollar_dollar -> cont 156#;
	ITinteger  happy_dollar_dollar -> cont 157#;
	ITrational happy_dollar_dollar -> cont 158#;
	ITinteger_lit -> cont 159#;
	ITfloat_lit -> cont 160#;
	ITrational_lit -> cont 161#;
	ITaddr_lit -> cont 162#;
	ITlit_lit -> cont 163#;
	ITstring_lit -> cont 164#;
	ITunknown happy_dollar_dollar -> cont 165#;
	})

happyThen = thenIf
happyReturn = returnIf
parseIface = happyParse



data IfaceStuff = PIface 	ParsedIface
		| PIdInfo	[HsIdInfo RdrName]
		| PType		RdrNameHsType

-- $Id: HappyTemplate-ghc,v 1.10 1997/12/04 15:07:25 simonm Exp $

{-
	The stack is in the following order throughout the parse:

	i	current token number
	j	another copy of this to avoid messing with the stack
	tk	current token semantic value
	st	current state
	sts	state stack
	stk	semantic stack
-}

-----------------------------------------------------------------------------

happyParse = happyNewToken action_0 [] []

-- All this HappyState stuff is simply because we can't have recursive
-- types in Haskell without an intervening data structure.

newtype HappyState b c = HappyState
	(Int# ->			-- token number
	 Int# ->			-- token number (yes, again)
	 b -> 				-- token semantic value
	 HappyState b c ->		-- current state
	 [HappyState b c] ->		-- state stack
	 c)

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts [ HappyAbsSyn1 ans ] = happyReturn ans
happyAccept j tk st sts _ = happyTcHack j (notHappyAtAll (-1))

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (-1#) tk st sts stk@(HappyErrorToken (I# i) : _) =
--     _trace "shifting the error token" $
     new_state i i tk (HappyState new_state) (st:sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (st:sts) (HappyTerminal tk:stk)

-----------------------------------------------------------------------------
-- Reducing

-- happyReduce is specialised for the common cases.

-- don't allow reductions when we're in error recovery, because this can
-- lead to an infinite loop.

happySpecReduce_0 i fn (-1#) tk _ sts stk
     = case sts of
	st@(HappyState action):sts -> action (-1#) (-1#) tk st sts stk
	_ -> happyError
happySpecReduce_0 i fn j tk st@(HappyState action) sts stk
     = action i j tk st (st:sts) (fn : stk)

happySpecReduce_1 i fn (-1#) tk _ (st@(HappyState action):sts) stk
     = action (-1#) (-1#) tk st sts stk
happySpecReduce_1 i fn j tk _ sts@(st@(HappyState action):_) (v1:stk')
     = action i j tk st sts (fn v1 : stk')
happySpecReduce_1 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_2 i fn (-1#) tk _ (st@(HappyState action):sts) stk
     = action (-1#) (-1#) tk st sts stk
happySpecReduce_2 i fn j tk _ (_:sts@(st@(HappyState action):_)) (v1:v2:stk')
     = action i j tk st sts (fn v1 v2 : stk')
happySpecReduce_2 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_3 i fn (-1#) tk _ (st@(HappyState action):sts) stk
     = action (-1#) (-1#) tk st sts stk
happySpecReduce_3 i fn j tk _ (_:_:sts@(st@(HappyState action):_)) 
	(v1:v2:v3:stk')
     = action i j tk st sts (fn v1 v2 v3 : stk')
happySpecReduce_3 _ _ _ _ _ _ _
     = notHappyAtAll

happyReduce k i fn (-1#) tk _ (st@(HappyState action):sts) stk
     = action (-1#) (-1#) tk st sts stk
happyReduce k i fn j tk st sts stk = action i j tk st' sts' (fn stk)
       where sts'@(st'@(HappyState action):_) = drop (I# k) (st:sts)

happyMonadReduce k i c fn (-1#) tk _ sts stk
      = case sts of
	     (st@(HappyState action):sts) -> action (-1#) (-1#) tk st sts stk
	     [] -> happyError
happyMonadReduce k i c fn j tk st sts stk =
	happyThen (fn stk) (\r -> action i j tk st' sts' (c r : stk'))
       where sts'@(st'@(HappyState action):_) = drop (I# k) (st:sts)
	     stk' = drop (I# k) stk

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto action j tk st = action j j tk (HappyState action)

-----------------------------------------------------------------------------
-- Error recovery (-1 is the error token)

-- fail if we are in recovery and no more states to discard
happyFail  (-1#) tk st' [] stk = happyError

-- discard a state
happyFail  (-1#) tk st' (st@(HappyState action):sts) stk =
--	_trace "discarding state" $
	action (-1#) (-1#) tk st sts stk

-- Enter error recovery: generate an error token,
-- 			 save the old token and carry on.
happyFail  i tk st@(HappyState action) sts stk =
--	_trace "entering error recovery" $
	action (-1#) (-1#) tk st sts (HappyErrorToken (I# i) : stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-- end of Happy Template.

