
/* -----------------------------------------------------------------------------
 * $Id: Bytecodes.h,v 1.15 2000/04/11 20:44:19 panne Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Bytecode definitions.
 *
 * ---------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Instructions
 *
 * Notes:
 * o INTERNAL_ERROR is never generated by the compiler and usually
 *   indicates an error in the heap.
 *   PANIC is generated by the compiler whenever it tests an "irrefutable"
 *   pattern which fails.  If we don't see too many of these, we could
 *   optimise out the redundant test.
 *
 * o If you add any new instructions, you have to check that each enumeration
 *   has at most 256 entries in it --- some of the lists are very close to
 *   overflowing.
 * ------------------------------------------------------------------------*/

#define INSTRLIST             \
    Ins(i_INTERNAL_ERROR),    \
    Ins(i_PANIC),             \
    Ins(i_STK_CHECK),         \
    Ins(i_STK_CHECK_big),     \
    Ins(i_ARG_CHECK),         \
    Ins(i_ALLOC_AP),          \
    Ins(i_ALLOC_PAP),         \
    Ins(i_ALLOC_CONSTR),      \
    Ins(i_ALLOC_CONSTR_big),  \
    Ins(i_MKAP),              \
    Ins(i_MKAP_big),          \
    Ins(i_MKPAP),             \
    Ins(i_PACK),              \
    Ins(i_PACK_big),          \
    Ins(i_SLIDE),             \
    Ins(i_SLIDE_big),         \
    Ins(i_TEST),              \
    Ins(i_UNPACK),            \
    Ins(i_VAR),               \
    Ins(i_VAR_big),           \
    Ins(i_CONST),             \
    Ins(i_CONST_big),         \
    Ins(i_ENTER),             \
    Ins(i_RETADDR),           \
    Ins(i_RETADDR_big),       \
    Ins(i_VOID),              \
    Ins(i_VAR_INT),           \
    Ins(i_VAR_INT_big),       \
    Ins(i_CONST_INT),         \
    Ins(i_CONST_INT_big),     \
    Ins(i_PACK_INT),          \
    Ins(i_UNPACK_INT),        \
    Ins(i_TEST_INT),          \
    Ins(i_CONST_INTEGER),     \
    Ins(i_CONST_INTEGER_big), \
    Ins(i_VAR_WORD),          \
    Ins(i_VAR_WORD_big),      \
    Ins(i_CONST_WORD),        \
    Ins(i_PACK_WORD),         \
    Ins(i_UNPACK_WORD),       \
    Ins(i_VAR_ADDR),          \
    Ins(i_VAR_ADDR_big),      \
    Ins(i_CONST_ADDR),        \
    Ins(i_CONST_ADDR_big),    \
    Ins(i_PACK_ADDR),         \
    Ins(i_UNPACK_ADDR),       \
    Ins(i_VAR_CHAR),          \
    Ins(i_VAR_CHAR_big),      \
    Ins(i_CONST_CHAR),        \
    Ins(i_CONST_CHAR_big),    \
    Ins(i_PACK_CHAR),         \
    Ins(i_UNPACK_CHAR),       \
    Ins(i_VAR_FLOAT),         \
    Ins(i_VAR_FLOAT_big),     \
    Ins(i_CONST_FLOAT),       \
    Ins(i_CONST_FLOAT_big),   \
    Ins(i_PACK_FLOAT),        \
    Ins(i_UNPACK_FLOAT),      \
    Ins(i_VAR_DOUBLE),        \
    Ins(i_VAR_DOUBLE_big),    \
    Ins(i_CONST_DOUBLE),      \
    Ins(i_CONST_DOUBLE_big),  \
    Ins(i_PACK_DOUBLE),       \
    Ins(i_UNPACK_DOUBLE),     \
    Ins(i_VAR_STABLE),        \
    Ins(i_VAR_STABLE_big),    \
    Ins(i_PACK_STABLE),       \
    Ins(i_UNPACK_STABLE),     \
    Ins(i_PRIMOP1),           \
    Ins(i_PRIMOP2),           \
    Ins(i_RV),                \
    Ins(i_RVE),               \
    Ins(i_SE),                \
    Ins(i_VV)

#define BIGGEST_OPCODE ((int)(i_VV))

#define Ins(x) x
typedef enum { INSTRLIST } Instr;
#undef Ins



typedef enum
    { i_INTERNAL_ERROR1  /* Instruction 0 raises an internal error */

    , i_pushseqframe
    , i_pushcatchframe

    /* Char# operations */
    , i_gtChar
    , i_geChar
    , i_eqChar
    , i_neChar
    , i_ltChar
    , i_leChar
    , i_charToInt
    , i_intToChar

    /* Int# operations */
    , i_gtInt
    , i_geInt
    , i_eqInt
    , i_neInt
    , i_ltInt
    , i_leInt
    , i_minInt
    , i_maxInt
    , i_plusInt
    , i_minusInt
    , i_timesInt
    , i_quotInt
    , i_remInt
    , i_quotRemInt
    , i_negateInt
    , i_andInt
    , i_orInt
    , i_xorInt
    , i_notInt
    , i_shiftLInt
    , i_shiftRAInt
    , i_shiftRLInt

    /* Word# operations */
    , i_gtWord
    , i_geWord
    , i_eqWord
    , i_neWord
    , i_ltWord
    , i_leWord
    , i_minWord
    , i_maxWord
    , i_plusWord
    , i_minusWord
    , i_timesWord
    , i_quotWord
    , i_remWord
    , i_quotRemWord
    , i_negateWord
    , i_andWord
    , i_orWord
    , i_xorWord
    , i_notWord
    , i_shiftLWord
    , i_shiftRAWord
    , i_shiftRLWord
    , i_intToWord
    , i_wordToInt

    /* Addr# operations */
    , i_gtAddr
    , i_geAddr
    , i_eqAddr
    , i_neAddr
    , i_ltAddr
    , i_leAddr
    , i_intToAddr
    , i_addrToInt

    /* Stable# operations */
    , i_intToStable
    , i_stableToInt

    /* Stateless Addr operations */
    , i_indexCharOffAddr
    , i_indexIntOffAddr
    , i_indexWordOffAddr
    , i_indexAddrOffAddr
    , i_indexFloatOffAddr
    , i_indexDoubleOffAddr
    , i_indexStableOffAddr

    , i_readCharOffAddr
    , i_readIntOffAddr
    , i_readWordOffAddr
    , i_readAddrOffAddr
    , i_readFloatOffAddr
    , i_readDoubleOffAddr
    , i_readStableOffAddr

    , i_writeCharOffAddr
    , i_writeIntOffAddr
    , i_writeWordOffAddr
    , i_writeAddrOffAddr
    , i_writeFloatOffAddr
    , i_writeDoubleOffAddr
    , i_writeStableOffAddr

    /* Integer operations */
    , i_compareInteger
    , i_negateInteger
    , i_plusInteger
    , i_minusInteger
    , i_timesInteger
    , i_quotRemInteger
    , i_divModInteger
    , i_integerToInt
    , i_intToInteger
    , i_integerToWord
    , i_wordToInteger
    , i_integerToFloat
    , i_floatToInteger
    , i_integerToDouble
    , i_doubleToInteger

    /* Float# operations */
    , i_gtFloat
    , i_geFloat
    , i_eqFloat
    , i_neFloat
    , i_ltFloat
    , i_leFloat
    , i_minFloat
    , i_maxFloat
    , i_radixFloat
    , i_digitsFloat
    , i_minExpFloat
    , i_maxExpFloat
    , i_plusFloat
    , i_minusFloat
    , i_timesFloat
    , i_divideFloat
    , i_negateFloat
    , i_floatToInt
    , i_intToFloat
    , i_expFloat
    , i_logFloat
    , i_sqrtFloat
    , i_sinFloat
    , i_cosFloat
    , i_tanFloat
    , i_asinFloat
    , i_acosFloat
    , i_atanFloat
    , i_sinhFloat
    , i_coshFloat
    , i_tanhFloat
    , i_powerFloat
    , i_decodeFloatZ
    , i_encodeFloatZ
    , i_isNaNFloat
    , i_isInfiniteFloat
    , i_isDenormalizedFloat
    , i_isNegativeZeroFloat
    , i_isIEEEFloat

    /* Double# operations */
    , i_gtDouble
    , i_geDouble
    , i_eqDouble
    , i_neDouble
    , i_ltDouble
    , i_leDouble
    , i_minDouble
    , i_maxDouble
    , i_radixDouble
    , i_digitsDouble
    , i_minExpDouble
    , i_maxExpDouble
    , i_plusDouble
    , i_minusDouble
    , i_timesDouble
    , i_divideDouble
    , i_negateDouble
    , i_doubleToInt
    , i_intToDouble
    , i_doubleToFloat
    , i_floatToDouble
    , i_expDouble
    , i_logDouble
    , i_sqrtDouble
    , i_sinDouble
    , i_cosDouble
    , i_tanDouble
    , i_asinDouble
    , i_acosDouble
    , i_atanDouble
    , i_sinhDouble
    , i_coshDouble
    , i_tanhDouble
    , i_powerDouble
    , i_decodeDoubleZ
    , i_encodeDoubleZ
    , i_isNaNDouble
    , i_isInfiniteDouble
    , i_isDenormalizedDouble
    , i_isNegativeZeroDouble
    , i_isIEEEDouble

    /* If you add a new primop to this table, check you don't
     * overflow the 256 limit.  That is MAX_Primop1 <= 255.
     * Current value (30/10/98) = 0xc8
     */
    , MAX_Primop1 = i_isIEEEDouble
} Primop1;


typedef enum
    { i_INTERNAL_ERROR2  /* Instruction 0 raises an internal error */

    , i_raise       

    /* Ref operations */
    , i_newRef
    , i_writeRef
    , i_readRef
    , i_sameRef

    /* Prim[Mutable]Array operations */
    , i_sameMutableArray
    , i_unsafeFreezeArray

    , i_newArray
    , i_writeArray
    , i_readArray
    , i_indexArray
    , i_sizeArray
    , i_sizeMutableArray

    /* Prim[Mutable]ByteArray operations */
    , i_sameMutableByteArray
    , i_unsafeFreezeByteArray
    , i_newByteArray

    , i_writeCharArray
    , i_readCharArray
    , i_indexCharArray

    , i_writeIntArray
    , i_readIntArray
    , i_indexIntArray

    /* {write,read,index}IntegerArray not provided */

    , i_writeWordArray
    , i_readWordArray
    , i_indexWordArray
    , i_writeAddrArray
    , i_readAddrArray
    , i_indexAddrArray
    , i_writeFloatArray
    , i_readFloatArray
    , i_indexFloatArray
    , i_writeDoubleArray
    , i_readDoubleArray
    , i_indexDoubleArray

#if 0
#ifdef PROVIDE_STABLE
    , i_writeStableArray
    , i_readStableArray
    , i_indexStableArray
#endif
#endif

    /* {write,read,index}ForeignObjArray not provided */

#ifdef PROVIDE_PTREQUALITY
    , i_reallyUnsafePtrEquality
#endif
#ifdef PROVIDE_COERCE
    , i_unsafeCoerce
#endif

#ifdef PROVIDE_FOREIGN
    /* ForeignObj# operations */
    , i_mkForeignObj

    , indexCharOffForeignObj
    , indexIntOffForeignObj
    , indexInt64OffForeignObj
    , indexWordOffForeignObj
    , indexAddrOffForeignObj
    , indexFloatOffForeignObj
    , indexDoubleOffForeignObj
    , indexStablePtrOffForeignObj
#endif
#ifdef PROVIDE_WEAK
    /* Weak# operations */
    , i_makeWeak
    , i_deRefWeak
#endif 
    /* StablePtr# operations */
    , i_makeStablePtr
    , i_deRefStablePtr
    , i_freeStablePtr

    /* foreign export dynamic support */
    , i_createAdjThunkARCH

    /* misc handy hacks */
    , i_getArgc
    , i_getArgv

#ifdef PROVIDE_CONCURRENT
    /* Concurrency operations */
    , i_forkIO
    , i_killThread
    , i_raiseInThread
    , i_delay
    , i_waitRead
    , i_waitWrite
    , i_yield
    , i_getThreadId
    , i_cmpThreadIds
#endif
    , i_sameMVar
    , i_newMVar
    , i_takeMVar
    , i_putMVar


    /* CCall! */
    , i_ccall_ccall_Id
    , i_ccall_ccall_IO
    , i_ccall_stdcall_Id
    , i_ccall_stdcall_IO

    /* If you add a new primop to this table, check you don't
     * overflow the 256 limit.  That is MAX_Primop2 <= 255.
     * Current value (30/10/98) = 0x42
     */
    , MAX_Primop2 = i_ccall_stdcall_IO
} Primop2;

typedef unsigned int InstrPtr; /* offset of instruction within BCO */

/*-------------------------------------------------------------------------*/