/* -----------------------------------------------------------------------------
 * $Id: Ticky.c,v 1.12 1999/11/11 17:50:48 simonpj Exp $
 *
 * (c) The AQUA project, Glasgow University, 1992-1997
 * (c) The GHC Team, 1998-1999
 *
 * Ticky-ticky profiling
 *-------------------------------------------------------------------------- */

#if defined(TICKY_TICKY)

#define TICKY_C			/* define those variables */
#include "Rts.h"
#include "RtsFlags.h"
#include "Ticky.h"

/* -----------------------------------------------------------------------------
   Print out all the counters
   -------------------------------------------------------------------------- */

static void printRegisteredCounterInfo (FILE *); /* fwd decl */

#define INTAVG(a,b) ((b == 0) ? 0.0 : ((double) (a) / (double) (b)))
#define PC(a)	    (100.0 * a)

#define AVG(thing) \
	StgDouble avg##thing  = INTAVG(tot##thing,ctr##thing)

void
PrintTickyInfo(void)
{
  unsigned long i;
  unsigned long tot_allocs = /* total number of things allocated */
	ALLOC_FUN_ctr + ALLOC_SE_THK_ctr + ALLOC_UP_THK_ctr + ALLOC_CON_ctr + ALLOC_TUP_ctr +
    	ALLOC_TSO_ctr +
#ifdef PAR
	ALLOC_FMBQ_ctr + ALLOC_FME_ctr + ALLOC_BF_ctr +
#endif
	ALLOC_BH_ctr  + ALLOC_UPD_PAP_ctr + ALLOC_PRIM_ctr;

  unsigned long tot_adm_wds = /* total number of admin words allocated */
	ALLOC_FUN_adm + ALLOC_THK_adm + ALLOC_CON_adm + ALLOC_TUP_adm +
    	ALLOC_TSO_adm +
#ifdef PAR
	ALLOC_FMBQ_adm + ALLOC_FME_adm + ALLOC_BF_adm +
#endif
	ALLOC_BH_adm  + ALLOC_UPD_PAP_adm + ALLOC_PRIM_adm;

  unsigned long tot_gds_wds = /* total number of words of ``good stuff'' allocated */
	ALLOC_FUN_gds + ALLOC_THK_gds + ALLOC_CON_gds + ALLOC_TUP_gds +
    	ALLOC_TSO_gds +
#ifdef PAR
	ALLOC_FMBQ_gds + ALLOC_FME_gds + ALLOC_BF_gds +
#endif

	ALLOC_BH_gds  + ALLOC_UPD_PAP_gds + ALLOC_PRIM_gds;

  unsigned long tot_slp_wds = /* total number of ``slop'' words allocated */
	ALLOC_FUN_slp + ALLOC_THK_slp + ALLOC_CON_slp + ALLOC_TUP_slp +
    	ALLOC_TSO_slp +
#ifdef PAR
	ALLOC_FMBQ_slp + ALLOC_FME_slp + ALLOC_BF_slp +
#endif
	ALLOC_BH_slp  + ALLOC_UPD_PAP_slp + ALLOC_PRIM_slp;

  unsigned long tot_wds = /* total words */
	tot_adm_wds + tot_gds_wds + tot_slp_wds;

  unsigned long tot_enters =
	ENT_CON_ctr + ENT_FUN_DIRECT_ctr +
	ENT_IND_ctr + ENT_PERM_IND_ctr + ENT_PAP_ctr + ENT_THK_ctr;
  unsigned long jump_direct_enters =
	tot_enters - ENT_VIA_NODE_ctr;
  unsigned long bypass_enters =
	ENT_FUN_DIRECT_ctr -
	(ENT_FUN_STD_ctr - UPD_PAP_IN_NEW_ctr);

  unsigned long tot_returns =
	RET_NEW_ctr + RET_OLD_ctr + RET_UNBOXED_TUP_ctr +
        RET_SEMI_IN_HEAP_ctr + RET_SEMI_BY_DEFAULT_ctr/*?*/;

  unsigned long tot_returns_of_new = RET_NEW_ctr;

  unsigned long con_updates = UPD_CON_IN_NEW_ctr + UPD_CON_IN_PLACE_ctr;
  unsigned long pap_updates = UPD_PAP_IN_NEW_ctr + UPD_PAP_IN_PLACE_ctr;

  unsigned long tot_updates = UPD_SQUEEZED_ctr + pap_updates + con_updates;

  unsigned long tot_new_updates   = UPD_NEW_IND_ctr + UPD_NEW_PERM_IND_ctr;
  unsigned long tot_old_updates   = UPD_OLD_IND_ctr + UPD_OLD_PERM_IND_ctr;
  unsigned long tot_gengc_updates = tot_new_updates + tot_old_updates;

  FILE *tf = RtsFlags.TickyFlags.tickyFile;

  fprintf(tf,"\n\nALLOCATIONS: %ld (%ld words total: %ld admin, %ld goods, %ld slop)\n",
	  tot_allocs, tot_wds, tot_adm_wds, tot_gds_wds, tot_slp_wds);
  fprintf(tf,"\t\t\t\ttotal words:\t    2     3     4     5    6+\n");

#define ALLOC_HISTO_MAGIC(categ) \
	(PC(INTAVG(ALLOC_##categ##_hst[0], ALLOC_##categ##_ctr))), \
	(PC(INTAVG(ALLOC_##categ##_hst[1], ALLOC_##categ##_ctr))), \
	(PC(INTAVG(ALLOC_##categ##_hst[2], ALLOC_##categ##_ctr))), \
	(PC(INTAVG(ALLOC_##categ##_hst[3], ALLOC_##categ##_ctr))), \
	(PC(INTAVG(ALLOC_##categ##_hst[4], ALLOC_##categ##_ctr)))

  fprintf(tf,"%7ld (%5.1f%%) function values",
	ALLOC_FUN_ctr,
	PC(INTAVG(ALLOC_FUN_ctr, tot_allocs)));
  if (ALLOC_FUN_ctr != 0)
      fprintf(tf,"\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(FUN));

  fprintf(tf,"\n%7ld (%5.1f%%) thunks",
	ALLOC_SE_THK_ctr + ALLOC_UP_THK_ctr,
	PC(INTAVG(ALLOC_SE_THK_ctr + ALLOC_UP_THK_ctr, tot_allocs)));

#define ALLOC_THK_ctr (ALLOC_UP_THK_ctr + ALLOC_SE_THK_ctr)
  /* hack to make ALLOC_HISTO_MAGIC still work for THK */
  if ((ALLOC_SE_THK_ctr + ALLOC_UP_THK_ctr) != 0)
      fprintf(tf,"\t\t\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(THK));
#undef ALLOC_THK_ctr

  fprintf(tf,"\n%7ld (%5.1f%%) data values",
	ALLOC_CON_ctr,
	PC(INTAVG(ALLOC_CON_ctr, tot_allocs)));
  if (ALLOC_CON_ctr != 0)
      fprintf(tf,"\t\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(CON));

  fprintf(tf,"\n%7ld (%5.1f%%) big tuples",
	ALLOC_TUP_ctr,
	PC(INTAVG(ALLOC_TUP_ctr, tot_allocs)));
  if (ALLOC_TUP_ctr != 0)
      fprintf(tf,"\t\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(TUP));

  fprintf(tf,"\n%7ld (%5.1f%%) black holes",
	ALLOC_BH_ctr,
	PC(INTAVG(ALLOC_BH_ctr, tot_allocs)));
  if (ALLOC_BH_ctr != 0)
      fprintf(tf,"\t\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(BH));

  fprintf(tf,"\n%7ld (%5.1f%%) prim things",
	ALLOC_PRIM_ctr,
	PC(INTAVG(ALLOC_PRIM_ctr, tot_allocs)));
  if (ALLOC_PRIM_ctr != 0)
      fprintf(tf,"\t\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(PRIM));

  fprintf(tf,"\n%7ld (%5.1f%%) partial applications",
	ALLOC_UPD_PAP_ctr,
	PC(INTAVG(ALLOC_UPD_PAP_ctr, tot_allocs)));
  if (ALLOC_UPD_PAP_ctr != 0)
      fprintf(tf,"\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(UPD_PAP));

  fprintf(tf,"\n%7ld (%5.1f%%) thread state objects",
	ALLOC_TSO_ctr,
	PC(INTAVG(ALLOC_TSO_ctr, tot_allocs)));
  if (ALLOC_TSO_ctr != 0)
      fprintf(tf,"\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(TSO));
#ifdef PAR
  fprintf(tf,"\n%7ld (%5.1f%%) thread state objects",
	ALLOC_FMBQ_ctr,
	PC(INTAVG(ALLOC_FMBQ_ctr, tot_allocs)));
  if (ALLOC_FMBQ_ctr != 0)
      fprintf(tf,"\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(FMBQ));
  fprintf(tf,"\n%7ld (%5.1f%%) thread state objects",
	ALLOC_FME_ctr,
	PC(INTAVG(ALLOC_FME_ctr, tot_allocs)));
  if (ALLOC_FME_ctr != 0)
      fprintf(tf,"\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(FME));
  fprintf(tf,"\n%7ld (%5.1f%%) thread state objects",
	ALLOC_BF_ctr,
	PC(INTAVG(ALLOC_BF_ctr, tot_allocs)));
  if (ALLOC_BF_ctr != 0)
      fprintf(tf,"\t\t%5.1f %5.1f %5.1f %5.1f %5.1f", ALLOC_HISTO_MAGIC(BF));
#endif
  fprintf(tf,"\n");

  fprintf(tf,"\nTotal storage-manager allocations: %ld (%ld words)\n\t[%ld words lost to speculative heap-checks]\n", ALLOC_HEAP_ctr, ALLOC_HEAP_tot, ALLOC_HEAP_tot - tot_wds);

  fprintf(tf,"\nSTACK USAGE:\n"); /* NB: some bits are direction sensitive */

  fprintf(tf,"\nENTERS: %ld  of which %ld (%.1f%%) direct to the entry code\n\t\t  [the rest indirected via Node's info ptr]\n",
	tot_enters,
	jump_direct_enters,
	PC(INTAVG(jump_direct_enters,tot_enters)));
  fprintf(tf,"%7ld (%5.1f%%) thunks\n",
	ENT_THK_ctr,
	PC(INTAVG(ENT_THK_ctr,tot_enters)));
  fprintf(tf,"%7ld (%5.1f%%) data values\n",
	ENT_CON_ctr,
	PC(INTAVG(ENT_CON_ctr,tot_enters)));
  fprintf(tf,"%7ld (%5.1f%%) function values\n\t\t  [of which %ld (%.1f%%) bypassed arg-satisfaction chk]\n",
	ENT_FUN_DIRECT_ctr,
	PC(INTAVG(ENT_FUN_DIRECT_ctr,tot_enters)),
	bypass_enters,
	PC(INTAVG(bypass_enters,ENT_FUN_DIRECT_ctr)));
  fprintf(tf,"%7ld (%5.1f%%) partial applications\n",
	ENT_PAP_ctr,
	PC(INTAVG(ENT_PAP_ctr,tot_enters)));
  fprintf(tf,"%7ld (%5.1f%%) normal indirections\n",
	ENT_IND_ctr,
	PC(INTAVG(ENT_IND_ctr,tot_enters)));
  fprintf(tf,"%7ld (%5.1f%%) permanent indirections\n",
	ENT_PERM_IND_ctr,
	PC(INTAVG(ENT_PERM_IND_ctr,tot_enters)));

  fprintf(tf,"\nRETURNS: %ld\n", tot_returns);
  fprintf(tf,"%7ld (%5.1f%%) from entering a new constructor\n\t\t  [the rest from entering an existing constructor]\n",
	tot_returns_of_new,
	PC(INTAVG(tot_returns_of_new,tot_returns)));
  fprintf(tf,"%7ld (%5.1f%%) vectored [the rest unvectored]\n",
	VEC_RETURN_ctr,
	PC(INTAVG(VEC_RETURN_ctr,tot_returns)));

  fprintf(tf, "\nRET_NEW:         %7ld: ", RET_NEW_ctr);
  for (i = 0; i < 9; i++) { fprintf(tf, "%5.1f%%",
				PC(INTAVG(RET_NEW_hst[i],RET_NEW_ctr))); }
  fprintf(tf, "\n");
  fprintf(tf, "RET_OLD:         %7ld: ", RET_OLD_ctr);
  for (i = 0; i < 9; i++) { fprintf(tf, "%5.1f%%",
				PC(INTAVG(RET_OLD_hst[i],RET_OLD_ctr))); }
  fprintf(tf, "\n");
  fprintf(tf, "RET_UNBOXED_TUP: %7ld: ", RET_UNBOXED_TUP_ctr);
  for (i = 0; i < 9; i++) { fprintf(tf, "%5.1f%%",
				    PC(INTAVG(RET_UNBOXED_TUP_hst[i],
					      RET_UNBOXED_TUP_ctr))); }
  fprintf(tf, "\n");
  fprintf(tf, "\nRET_VEC_RETURN : %7ld: ", VEC_RETURN_ctr);
  for (i = 0; i < 9; i++) { fprintf(tf, "%5.1f%%",
				PC(INTAVG(RET_VEC_RETURN_hst[i],VEC_RETURN_ctr))); }
  fprintf(tf, "\n");

  fprintf(tf,"\nUPDATE FRAMES: %ld (%ld omitted from thunks)",
	UPDF_PUSHED_ctr,
	UPDF_OMITTED_ctr);

  fprintf(tf,"\nSEQ FRAMES:    %ld", SEQF_PUSHED_ctr);

  fprintf(tf,"\nCATCH FRAMES:  %ld", CATCHF_PUSHED_ctr);

  if (UPDF_RCC_PUSHED_ctr != 0)
     fprintf(tf,"%7ld restore cost centre frames (%ld omitted)\n",
	UPDF_RCC_PUSHED_ctr,
	UPDF_RCC_OMITTED_ctr);

  fprintf(tf,"\nUPDATES: %ld\n", tot_updates);
  fprintf(tf,"%7ld (%5.1f%%) data values\n\t\t  [%ld in place, %ld allocated new space]\n",
	con_updates,
	PC(INTAVG(con_updates,tot_updates)),
	UPD_CON_IN_PLACE_ctr, UPD_CON_IN_NEW_ctr);
  fprintf(tf,"%7ld (%5.1f%%) partial applications\n\t\t  [%ld in place, %ld allocated new space]\n",
	pap_updates,
	PC(INTAVG(pap_updates,tot_updates)),
	UPD_PAP_IN_PLACE_ctr, UPD_PAP_IN_NEW_ctr);
  fprintf(tf,"%7ld (%5.1f%%) updates by squeezing\n",
	UPD_SQUEEZED_ctr,
	PC(INTAVG(UPD_SQUEEZED_ctr, tot_updates)));

  fprintf(tf, "\nUPD_CON_IN_NEW:   %7ld: ", UPD_CON_IN_NEW_ctr);
  for (i = 0; i < 9; i++) { fprintf(tf, "%7ld", UPD_CON_IN_NEW_hst[i]); }
  fprintf(tf, "\n");
  fprintf(tf, "UPD_CON_IN_PLACE: %7ld: ", UPD_CON_IN_PLACE_ctr);
  for (i = 0; i < 9; i++) { fprintf(tf, "%7ld", UPD_CON_IN_PLACE_hst[i]); }
  fprintf(tf, "\n");
  fprintf(tf, "UPD_PAP_IN_NEW:   %7ld: ", UPD_PAP_IN_NEW_ctr);
  for (i = 0; i < 9; i++) { fprintf(tf, "%7ld", UPD_PAP_IN_NEW_hst[i]); }
  fprintf(tf, "\n");

  if (tot_gengc_updates != 0) {
      fprintf(tf,"\nNEW GEN UPDATES: %9ld (%5.1f%%)\n",
	      tot_new_updates,
	      PC(INTAVG(tot_new_updates,tot_gengc_updates)));
      fprintf(tf,"OLD GEN UPDATES: %9ld (%5.1f%%)\n",
	      tot_old_updates,
	      PC(INTAVG(tot_old_updates,tot_gengc_updates)));
  }

  fprintf(tf,"\nTotal bytes copied during GC: %ld\n",
	  GC_WORDS_COPIED_ctr * sizeof(W_));

  printRegisteredCounterInfo(tf);

  fprintf(tf,"\n**************************************************\n");

  /* here, we print out all the raw numbers; these are really
    more useful when we want to snag them for subsequent
    rdb-etc processing. WDP 95/11
  */

#define PR_CTR(ctr) \
  do { fprintf(tf,"%7ld " #ctr "\n", ctr); } while(0)
/* COND_PR_CTR takes a boolean; if false then msg is the printname rather than ctr */
#define COND_PR_CTR(ctr,b,msg) \
    if (b) { fprintf(tf,"%7ld " #ctr "\n", ctr); } else { fprintf(tf,"%7ld " msg "\n", ctr); }
#define PR_HST(hst,i) \
  do { fprintf(tf,"%7ld " #hst "_" #i "\n", hst[i]); } while(0)

  PR_CTR(ALLOC_HEAP_ctr);
  PR_CTR(ALLOC_HEAP_tot);

  PR_CTR(ALLOC_FUN_ctr);
  PR_CTR(ALLOC_FUN_adm);
  PR_CTR(ALLOC_FUN_gds);
  PR_CTR(ALLOC_FUN_slp);
  PR_HST(ALLOC_FUN_hst,0);
  PR_HST(ALLOC_FUN_hst,1);
  PR_HST(ALLOC_FUN_hst,2);
  PR_HST(ALLOC_FUN_hst,3);
  PR_HST(ALLOC_FUN_hst,4);
  PR_CTR(ALLOC_UP_THK_ctr);
  PR_CTR(ALLOC_SE_THK_ctr);
  PR_CTR(ALLOC_THK_adm);
  PR_CTR(ALLOC_THK_gds);
  PR_CTR(ALLOC_THK_slp);
  PR_HST(ALLOC_THK_hst,0);
  PR_HST(ALLOC_THK_hst,1);
  PR_HST(ALLOC_THK_hst,2);
  PR_HST(ALLOC_THK_hst,3);
  PR_HST(ALLOC_THK_hst,4);
  PR_CTR(ALLOC_CON_ctr);
  PR_CTR(ALLOC_CON_adm);
  PR_CTR(ALLOC_CON_gds);
  PR_CTR(ALLOC_CON_slp);
  PR_HST(ALLOC_CON_hst,0);
  PR_HST(ALLOC_CON_hst,1);
  PR_HST(ALLOC_CON_hst,2);
  PR_HST(ALLOC_CON_hst,3);
  PR_HST(ALLOC_CON_hst,4);
  PR_CTR(ALLOC_TUP_ctr);
  PR_CTR(ALLOC_TUP_adm);
  PR_CTR(ALLOC_TUP_gds);
  PR_CTR(ALLOC_TUP_slp);
  PR_HST(ALLOC_TUP_hst,0);
  PR_HST(ALLOC_TUP_hst,1);
  PR_HST(ALLOC_TUP_hst,2);
  PR_HST(ALLOC_TUP_hst,3);
  PR_HST(ALLOC_TUP_hst,4);
  PR_CTR(ALLOC_BH_ctr);
  PR_CTR(ALLOC_BH_adm);
  PR_CTR(ALLOC_BH_gds);
  PR_CTR(ALLOC_BH_slp);
  PR_HST(ALLOC_BH_hst,0);
  PR_HST(ALLOC_BH_hst,1);
  PR_HST(ALLOC_BH_hst,2);
  PR_HST(ALLOC_BH_hst,3);
  PR_HST(ALLOC_BH_hst,4);
  PR_CTR(ALLOC_PRIM_ctr);
  PR_CTR(ALLOC_PRIM_adm);
  PR_CTR(ALLOC_PRIM_gds);
  PR_CTR(ALLOC_PRIM_slp);
  PR_HST(ALLOC_PRIM_hst,0);
  PR_HST(ALLOC_PRIM_hst,1);
  PR_HST(ALLOC_PRIM_hst,2);
  PR_HST(ALLOC_PRIM_hst,3);
  PR_HST(ALLOC_PRIM_hst,4);
  PR_CTR(ALLOC_UPD_PAP_ctr);
  PR_CTR(ALLOC_UPD_PAP_adm);
  PR_CTR(ALLOC_UPD_PAP_gds);
  PR_CTR(ALLOC_UPD_PAP_slp);
  PR_HST(ALLOC_UPD_PAP_hst,0);
  PR_HST(ALLOC_UPD_PAP_hst,1);
  PR_HST(ALLOC_UPD_PAP_hst,2);
  PR_HST(ALLOC_UPD_PAP_hst,3);
  PR_HST(ALLOC_UPD_PAP_hst,4);

  PR_CTR(ALLOC_TSO_ctr);
  PR_CTR(ALLOC_TSO_adm);
  PR_CTR(ALLOC_TSO_gds);
  PR_CTR(ALLOC_TSO_slp);
  PR_HST(ALLOC_TSO_hst,0);
  PR_HST(ALLOC_TSO_hst,1);
  PR_HST(ALLOC_TSO_hst,2);
  PR_HST(ALLOC_TSO_hst,3);
  PR_HST(ALLOC_TSO_hst,4);

#ifdef PAR
  PR_CTR(ALLOC_FMBQ_ctr);
  PR_CTR(ALLOC_FMBQ_adm);
  PR_CTR(ALLOC_FMBQ_gds);
  PR_CTR(ALLOC_FMBQ_slp);
  PR_HST(ALLOC_FMBQ_hst,0);
  PR_HST(ALLOC_FMBQ_hst,1);
  PR_HST(ALLOC_FMBQ_hst,2);
  PR_HST(ALLOC_FMBQ_hst,3);
  PR_HST(ALLOC_FMBQ_hst,4);
  PR_CTR(ALLOC_FME_ctr);
  PR_CTR(ALLOC_FME_adm);
  PR_CTR(ALLOC_FME_gds);
  PR_CTR(ALLOC_FME_slp);
  PR_HST(ALLOC_FME_hst,0);
  PR_HST(ALLOC_FME_hst,1);
  PR_HST(ALLOC_FME_hst,2);
  PR_HST(ALLOC_FME_hst,3);
  PR_HST(ALLOC_FME_hst,4);
  PR_CTR(ALLOC_BF_ctr);
  PR_CTR(ALLOC_BF_adm);
  PR_CTR(ALLOC_BF_gds);
  PR_CTR(ALLOC_BF_slp);
  PR_HST(ALLOC_BF_hst,0);
  PR_HST(ALLOC_BF_hst,1);
  PR_HST(ALLOC_BF_hst,2);
  PR_HST(ALLOC_BF_hst,3);
  PR_HST(ALLOC_BF_hst,4);
#endif

  PR_CTR(ENT_VIA_NODE_ctr);
  PR_CTR(ENT_CON_ctr);
  PR_CTR(ENT_FUN_STD_ctr);
  PR_CTR(ENT_FUN_DIRECT_ctr);
  PR_CTR(ENT_IND_ctr);

/* The counters ENT_PERM_IND and UPD_{NEW,OLD}_PERM_IND are not dumped
 * at the end of execution unless update squeezing is turned off (+RTS
 * -Z =RtsFlags.GcFlags.squeezeUpdFrames), as they will be wrong
 * otherwise.  Why?  Because for each update frame squeezed out, we
 * count an UPD_NEW_PERM_IND *at GC time* (i.e., too early).  And
 * further, when we enter the closure that has been updated, we count
 * the ENT_PERM_IND, but we then enter the PERM_IND that was built for
 * the next update frame below, and so on down the chain until we
 * finally reach the value.  Thus we count many new ENT_PERM_INDs too
 * early.  
 * 
 * This of course refers to the -ticky version that uses PERM_INDs to
 * determine the number of closures entered 0/1/>1.  KSW 1999-04.  */
  COND_PR_CTR(ENT_PERM_IND_ctr,RtsFlags.GcFlags.squeezeUpdFrames == rtsTrue,"E!NT_PERM_IND_ctr requires +RTS -Z");

  PR_CTR(ENT_PAP_ctr);
  PR_CTR(ENT_AP_UPD_ctr);
  PR_CTR(ENT_BH_ctr);
  PR_CTR(ENT_THK_ctr);

  PR_CTR(RET_NEW_ctr);
  PR_CTR(RET_OLD_ctr);
  PR_CTR(RET_UNBOXED_TUP_ctr);
  PR_CTR(RET_SEMI_BY_DEFAULT_ctr);
  PR_CTR(RET_SEMI_IN_HEAP_ctr);
  PR_CTR(RET_SEMI_FAILED_IND_ctr);
  PR_CTR(RET_SEMI_FAILED_UNEVAL_ctr);
  PR_CTR(VEC_RETURN_ctr);

  PR_HST(RET_NEW_hst,0);
  PR_HST(RET_NEW_hst,1);
  PR_HST(RET_NEW_hst,2);
  PR_HST(RET_NEW_hst,3);
  PR_HST(RET_NEW_hst,4);
  PR_HST(RET_NEW_hst,5);
  PR_HST(RET_NEW_hst,6);
  PR_HST(RET_NEW_hst,7);
  PR_HST(RET_NEW_hst,8);
  PR_HST(RET_OLD_hst,0);
  PR_HST(RET_OLD_hst,1);
  PR_HST(RET_OLD_hst,2);
  PR_HST(RET_OLD_hst,3);
  PR_HST(RET_OLD_hst,4);
  PR_HST(RET_OLD_hst,5);
  PR_HST(RET_OLD_hst,6);
  PR_HST(RET_OLD_hst,7);
  PR_HST(RET_OLD_hst,8);
  PR_HST(RET_UNBOXED_TUP_hst,0);
  PR_HST(RET_UNBOXED_TUP_hst,1);
  PR_HST(RET_UNBOXED_TUP_hst,2);
  PR_HST(RET_UNBOXED_TUP_hst,3);
  PR_HST(RET_UNBOXED_TUP_hst,4);
  PR_HST(RET_UNBOXED_TUP_hst,5);
  PR_HST(RET_UNBOXED_TUP_hst,6);
  PR_HST(RET_UNBOXED_TUP_hst,7);
  PR_HST(RET_UNBOXED_TUP_hst,8);
  PR_HST(RET_SEMI_IN_HEAP_hst,0);
  PR_HST(RET_SEMI_IN_HEAP_hst,1);
  PR_HST(RET_SEMI_IN_HEAP_hst,2);
  PR_HST(RET_SEMI_IN_HEAP_hst,3);
  PR_HST(RET_SEMI_IN_HEAP_hst,4);
  PR_HST(RET_SEMI_IN_HEAP_hst,5);
  PR_HST(RET_SEMI_IN_HEAP_hst,6);
  PR_HST(RET_SEMI_IN_HEAP_hst,7);
  PR_HST(RET_SEMI_IN_HEAP_hst,8);
  PR_HST(RET_VEC_RETURN_hst,0);
  PR_HST(RET_VEC_RETURN_hst,1);
  PR_HST(RET_VEC_RETURN_hst,2);
  PR_HST(RET_VEC_RETURN_hst,3);
  PR_HST(RET_VEC_RETURN_hst,4);
  PR_HST(RET_VEC_RETURN_hst,5);
  PR_HST(RET_VEC_RETURN_hst,6);
  PR_HST(RET_VEC_RETURN_hst,7);
  PR_HST(RET_VEC_RETURN_hst,8);

  PR_CTR(RET_SEMI_loads_avoided);

  PR_CTR(UPDF_OMITTED_ctr);
  PR_CTR(UPDF_PUSHED_ctr);
  PR_CTR(SEQF_PUSHED_ctr);
  PR_CTR(CATCHF_PUSHED_ctr);

  PR_CTR(UPDF_RCC_PUSHED_ctr);
  PR_CTR(UPDF_RCC_OMITTED_ctr);

  PR_CTR(UPD_SQUEEZED_ctr);
  PR_CTR(UPD_CON_IN_NEW_ctr);
  PR_CTR(UPD_CON_IN_PLACE_ctr);
  PR_CTR(UPD_PAP_IN_NEW_ctr);
  PR_CTR(UPD_PAP_IN_PLACE_ctr);

  PR_CTR(UPD_BH_UPDATABLE_ctr);
  PR_CTR(UPD_BH_SINGLE_ENTRY_ctr);
  PR_CTR(UPD_CAF_BH_UPDATABLE_ctr);
  PR_CTR(UPD_CAF_BH_SINGLE_ENTRY_ctr);

  PR_HST(UPD_CON_IN_NEW_hst,0);
  PR_HST(UPD_CON_IN_NEW_hst,1);
  PR_HST(UPD_CON_IN_NEW_hst,2);
  PR_HST(UPD_CON_IN_NEW_hst,3);
  PR_HST(UPD_CON_IN_NEW_hst,4);
  PR_HST(UPD_CON_IN_NEW_hst,5);
  PR_HST(UPD_CON_IN_NEW_hst,6);
  PR_HST(UPD_CON_IN_NEW_hst,7);
  PR_HST(UPD_CON_IN_NEW_hst,8);
  PR_HST(UPD_PAP_IN_NEW_hst,0);
  PR_HST(UPD_PAP_IN_NEW_hst,1);
  PR_HST(UPD_PAP_IN_NEW_hst,2);
  PR_HST(UPD_PAP_IN_NEW_hst,3);
  PR_HST(UPD_PAP_IN_NEW_hst,4);
  PR_HST(UPD_PAP_IN_NEW_hst,5);
  PR_HST(UPD_PAP_IN_NEW_hst,6);
  PR_HST(UPD_PAP_IN_NEW_hst,7);
  PR_HST(UPD_PAP_IN_NEW_hst,8);

  PR_CTR(UPD_NEW_IND_ctr);
  /* see comment on ENT_PERM_IND_ctr */
  COND_PR_CTR(UPD_NEW_PERM_IND_ctr,RtsFlags.GcFlags.squeezeUpdFrames == rtsTrue,"U!PD_NEW_PERM_IND_ctr requires +RTS -Z");
  PR_CTR(UPD_OLD_IND_ctr);
  /* see comment on ENT_PERM_IND_ctr */
  COND_PR_CTR(UPD_OLD_PERM_IND_ctr,RtsFlags.GcFlags.squeezeUpdFrames == rtsTrue,"U!PD_OLD_PERM_IND_ctr requires +RTS -Z");

  PR_CTR(GC_SEL_ABANDONED_ctr);
  PR_CTR(GC_SEL_MINOR_ctr);
  PR_CTR(GC_SEL_MAJOR_ctr);
  PR_CTR(GC_FAILED_PROMOTION_ctr);
  PR_CTR(GC_WORDS_COPIED_ctr);
}

/* Data structure used in ``registering'' one of these counters. */

StgEntCounter *ticky_entry_ctrs = NULL; /* root of list of them */

/* To print out all the registered-counter info: */

static void
printRegisteredCounterInfo (FILE *tf)
{
    StgEntCounter *p;

    if ( ticky_entry_ctrs != NULL ) {
      fprintf(tf,"\n**************************************************\n\n");
    }
    fprintf(tf, "%11s%11s%11s %6s%6s    %-11s%-30s\n",
	    "Entries", "Slow ent", "Allocs", "Arity", "Stack", "Kinds", "Function");
    fprintf(tf, "--------------------------------------------------------------------------------\n");
    /* Function name at the end so it doesn't mess up the tabulation */

    for (p = ticky_entry_ctrs; p != NULL; p = p->link) {
	fprintf(tf, "%11ld%11ld%11ld %6u%6u    %-11s%-30s",
		p->entry_count,
		p->slow_entry_count,
		p->allocs,
		p->arity,
		p->stk_args,
		p->arg_kinds,
		p->str);

	fprintf(tf, "\n");

    }
}

/* Catch-all top-level counter struct.  Allocations from CAFs will go
 * here.
 */
StgEntCounter top_ct
	= { 0, 0, 0,
	    "TOP", "",
	    0, 0, 0, NULL };

#endif /* TICKY_TICKY */

