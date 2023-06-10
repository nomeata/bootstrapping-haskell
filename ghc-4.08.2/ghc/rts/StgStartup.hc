/* -----------------------------------------------------------------------------
 * $Id: StgStartup.hc,v 1.13 2000/04/26 13:50:27 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Code for starting, stopping and restarting threads.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "StgRun.h" /* StgReturn */
#include "StgStartup.h"

/*
 * This module contains the two entry points and the final exit point
 * to/from the Haskell world.  We can enter either by:
 *
 *   a) returning to the address on the top of the stack, or
 *   b) entering the closure on the top of the stack
 *
 * the function stg_stop_thread_entry is the final exit for a
 * thread: it is the last return address on the stack.  It returns
 * to the scheduler marking the thread as finished.
 */

#define CHECK_SENSIBLE_REGS() \
    ASSERT(Hp != (P_)0);			\
    ASSERT(Sp != (P_)0);			\
    ASSERT(Su != (StgUpdateFrame *)0);		\
    ASSERT(SpLim != (P_)0);			\
    ASSERT(HpLim != (P_)0);			\
    ASSERT(Sp <= (P_)Su);			\
    ASSERT(SpLim - RESERVED_STACK_WORDS <= Sp); \
    ASSERT(HpLim >= Hp);

/* -----------------------------------------------------------------------------
   Returning from the STG world.

   This is a polymorphic return address, meaning that any old constructor
   can be returned, we don't care (actually, it's probably going to be
   an IOok constructor, which will indirect through the vector table
   slot 0).
   -------------------------------------------------------------------------- */

EXTFUN(stg_stop_thread_entry);

#ifdef PROFILING
#define STOP_THREAD_BITMAP 1
#else
#define STOP_THREAD_BITMAP 0
#endif

/* VEC_POLY_INFO expects to see these names - but they should all be the same. */
#define stg_stop_thread_0_entry stg_stop_thread_entry 
#define stg_stop_thread_1_entry stg_stop_thread_entry 
#define stg_stop_thread_2_entry stg_stop_thread_entry 
#define stg_stop_thread_3_entry stg_stop_thread_entry 
#define stg_stop_thread_4_entry stg_stop_thread_entry 
#define stg_stop_thread_5_entry stg_stop_thread_entry 
#define stg_stop_thread_6_entry stg_stop_thread_entry 
#define stg_stop_thread_7_entry stg_stop_thread_entry 

VEC_POLY_INFO_TABLE(stg_stop_thread,STOP_THREAD_BITMAP,0,0,0,STOP_FRAME,,EF_);

STGFUN(stg_stop_thread_entry)
{
    FB_

    /* 
     * The final exit.
     *
     * The top-top-level closures (e.g., "main") are of type "IO a".
     * When entered, they perform an IO action and return an 'a' in R1.
     *
     * We save R1 on top of the stack where the scheduler can find it,
     * tidy up the registers and return to the scheduler.
    */

    /* Move Sp to the last word on the stack, and Su to just past the end
     * of the stack.  We then place the return value at the top of the stack.
     */
    Sp += sizeofW(StgStopFrame) - 1;
    Su = (StgUpdateFrame *)(Sp+1);  
    Sp[0] = R1.w;

    CurrentTSO->what_next = ThreadComplete;

    SaveThreadState();	/* inline! */

    /* R1 contains the return value of the thread */
    R1.p = (P_)ThreadFinished;

    JMP_(StgReturn);
    FE_
}

/* -----------------------------------------------------------------------------
   Start a thread from the scheduler by returning to the address on
   the top of the stack  (and popping the address).  This is used for
   returning to the slow entry point of a function after a garbage collection
   or re-schedule.  The slow entry point expects the stack to contain the
   pending arguments only.
   -------------------------------------------------------------------------- */

STGFUN(stg_returnToStackTop)
{
  FB_
  LoadThreadState();
  CHECK_SENSIBLE_REGS();
  Sp++;
  JMP_(ENTRY_CODE(Sp[-1]));
  FE_
}

/* -----------------------------------------------------------------------------
   Start a thread from the scheduler by entering the closure pointed
   to by the word on the top of the stack.
   -------------------------------------------------------------------------- */

STGFUN(stg_enterStackTop)
{
  FB_
  LoadThreadState();
  CHECK_SENSIBLE_REGS();
  /* don't count this enter for ticky-ticky profiling */
  R1.p = (P_)Sp[0];
  Sp++;
  JMP_(GET_ENTRY(R1.cl));
  FE_
}

  
/* -----------------------------------------------------------------------------
   Special STG entry points for module registration.

   This stuff is problematic for Hugs, because it introduces a
   dependency between the RTS and the program (ie. __init_PrelMain).  So
   we currently disable module initialisation for Hugs.
   -------------------------------------------------------------------------- */

#ifndef INTERPRETER 

extern F_ *init_stack;

STGFUN(stg_init_ret)
{
  FB_
  JMP_(StgReturn);
  FE_
}

/* On entry to stg_init:
 *    init_stack[0] = &stg_init_ret;
 *    init_stack[1] = __init_Something;
 */
STGFUN(stg_init)
{
  FB_
  Sp = BaseReg->rSp;
  JMP_(POP_INIT_STACK());
  FE_
}

/* PrelGHC doesn't really exist... */

START_MOD_INIT(__init_PrelGHC);
END_MOD_INIT();

#endif /* !INTERPRETER */
