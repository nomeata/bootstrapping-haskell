/* -----------------------------------------------------------------------------
 * $Id: Main.c,v 1.24 2000/04/26 09:28:18 simonmar Exp $
 *
 * (c) The GHC Team 1998-2000
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#define COMPILING_RTS_MAIN

#include "Rts.h"
#include "RtsAPI.h"
#include "SchedAPI.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Prelude.h"

#ifdef DEBUG
# include "Printer.h"   /* for printing        */
#endif

#ifdef INTERPRETER
# include "Assembler.h"
#endif

#ifdef PAR
# include "ParInit.h"
# include "Parallel.h"
# include "LLC.h"
#endif

#if defined(GRAN) || defined(PAR)
# include "GranSimRts.h"
#endif

#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

EXTFUN(__init_PrelMain);

/* Hack: we assume that we're building a batch-mode system unless 
 * INTERPRETER is set
 */
#ifndef INTERPRETER /* Hack */
int main(int argc, char *argv[])
{
    int exit_status;
    SchedulerStatus status;
    /* all GranSim/GUM init is done in startupHaskell; sets IAmMainThread! */

    startupHaskell(argc,argv,__init_PrelMain);

    /* kick off the computation by creating the main thread with a pointer
       to mainIO_closure representing the computation of the overall program;
       then enter the scheduler with this thread and off we go;
      
       the same for GranSim (we have only one instance of this code)

       in a parallel setup, where we have many instances of this code
       running on different PEs, we should do this only for the main PE
       (IAmMainThread is set in startupHaskell) 
    */

#  if defined(PAR)

#   if defined(DEBUG)
    { /* a wait loop to allow attachment of gdb to UNIX threads */
      nat i, j, s;

      for (i=0, s=0; i<(nat)RtsFlags.ParFlags.wait; i++)
	for (j=0; j<1000000; j++) 
	  s += j % 65536;
    }
    IF_PAR_DEBUG(verbose,
		 belch("Passed wait loop"));
#   endif

    if (IAmMainThread == rtsTrue) {
      fprintf(stderr, "Main Thread Started ...\n");

      /* ToDo: Dump event for the main thread */
      status = rts_evalIO((HaskellObj)mainIO_closure, NULL);
    } else {
      /* Just to show we're alive */
      IF_PAR_DEBUG(verbose,
		   fprintf(stderr, "== [%x] Non-Main PE enters scheduler via taskStart() without work ...\n",
			   mytid));
     
      /* all non-main threads enter the scheduler without work */
      status = rts_evalNothing((StgClosure*)NULL);
    }

#  elif defined(GRAN)

    /* ToDo: Dump event for the main thread */
    status = rts_evalIO(mainIO_closure, NULL);

#  else /* !PAR && !GRAN */

    /* ToDo: want to start with a larger stack size */
    status = rts_evalIO((HaskellObj)mainIO_closure, NULL);

#  endif /* !PAR && !GRAN */

    // ToDo: update for parallel execution
    /* check the status of the entire Haskell computation */
    switch (status) {
    case Deadlock:
      prog_belch("no threads to run:  infinite loop or deadlock?");
      exit_status = EXIT_DEADLOCK;
      break;
    case Killed:
      prog_belch("main thread exited (uncaught exception)");
      exit_status = EXIT_KILLED;
      break;
    case Interrupted:
      prog_belch("interrupted");
      exit_status = EXIT_INTERRUPTED;
      break;
    case Success:
      exit_status = EXIT_SUCCESS;
      break;
    default:
      barf("main thread completed with invalid status");
    }
    shutdownHaskellAndExit(exit_status);
}
# endif /* BATCH_MODE */
