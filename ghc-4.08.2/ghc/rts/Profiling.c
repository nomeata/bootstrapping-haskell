/* -----------------------------------------------------------------------------
 * $Id: Profiling.c,v 1.20 2000/05/12 13:01:04 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Support for profiling
 *
 * ---------------------------------------------------------------------------*/

#ifdef PROFILING

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Profiling.h"
#include "Storage.h"
#include "Proftimer.h"
#include "Itimer.h"
#include "ProfHeap.h"

/*
 * Global variables used to assign unique IDs to cc's, ccs's, and 
 * closure_cats
 */

unsigned int CC_ID;
unsigned int CCS_ID;
unsigned int HP_ID;

/* Table sizes from old profiling system.  Not sure if we'll need
 * these.
 */
nat time_intervals = 0;
nat earlier_ticks  = 0;
nat max_cc_no      = 0;
nat max_mod_no     = 0;
nat max_grp_no     = 0;
nat max_descr_no   = 0;
nat max_type_no    = 0;

/* Are we time-profiling?
 */
rtsBool time_profiling = rtsFalse;

/* figures for the profiling report.
 */
static lnat total_alloc, total_prof_ticks;

/* Globals for opening the profiling log file(s)
 */
static char *prof_filename; /* prof report file name = <program>.prof */
FILE *prof_file;

static char *hp_filename;	/* heap profile (hp2ps style) log file */
FILE *hp_file;

/* The Current Cost Centre Stack (for attributing costs)
 */
CostCentreStack *CCCS;

/* Linked lists to keep track of cc's and ccs's that haven't
 * been declared in the log file yet
 */
CostCentre *CC_LIST;
CostCentreStack *CCS_LIST;

/*
 * Built-in cost centres and cost-centre stacks:
 *
 *    MAIN   is the root of the cost-centre stack tree.  If there are
 *           no _scc_s in the program, all costs will be attributed
 *           to MAIN.
 *
 *    SYSTEM is the RTS in general (scheduler, etc.).  All costs for
 *           RTS operations apart from garbage collection are attributed
 *           to SYSTEM.
 *
 *    GC     is the storage manager / garbage collector.
 *
 *    OVERHEAD gets all costs generated by the profiling system
 *           itself.  These are costs that would not be incurred
 *           during non-profiled execution of the program.
 *
 *    SUBSUMED is the one-and-only CCS placed on top-level functions. 
 *           It indicates that all costs are to be attributed to the
 *           enclosing cost centre stack.  SUBSUMED never accumulates
 *           any costs.  The is_caf flag is set on the subsumed cost
 *           centre.
 *
 *    DONT_CARE is a placeholder cost-centre we assign to static
 *           constructors.  It should *never* accumulate any costs.
 */

CC_DECLARE(CC_MAIN,      "MAIN", 	"MAIN",      CC_IS_BORING, );
CC_DECLARE(CC_SYSTEM,    "SYSTEM",   	"MAIN",      CC_IS_BORING, );
CC_DECLARE(CC_GC,        "GC",   	"GC",        CC_IS_BORING, );
CC_DECLARE(CC_OVERHEAD,  "OVERHEAD_of", "PROFILING", CC_IS_CAF,    );
CC_DECLARE(CC_SUBSUMED,  "SUBSUMED",    "MAIN",      CC_IS_CAF,    );
CC_DECLARE(CC_DONT_CARE, "DONT_CARE",   "MAIN",      CC_IS_BORING, );

CCS_DECLARE(CCS_MAIN, 	    CC_MAIN,       );
CCS_DECLARE(CCS_SYSTEM,	    CC_SYSTEM,     );
CCS_DECLARE(CCS_GC,         CC_GC,         );
CCS_DECLARE(CCS_OVERHEAD,   CC_OVERHEAD,   );
CCS_DECLARE(CCS_SUBSUMED,   CC_SUBSUMED,   );
CCS_DECLARE(CCS_DONT_CARE,  CC_DONT_CARE, );

/* 
 * Uniques for the XML log-file format
 */
#define CC_UQ         1
#define CCS_UQ        2
#define TC_UQ         3
#define HEAP_OBJ_UQ   4
#define TIME_UPD_UQ   5
#define HEAP_UPD_UQ   6

/* 
 * Static Functions
 */

static  CostCentreStack * ActualPush_     ( CostCentreStack *ccs, CostCentre *cc, 
					    CostCentreStack *new_ccs );
static  rtsBool           ccs_to_ignore   ( CostCentreStack *ccs );
static  void              count_ticks     ( CostCentreStack *ccs );
static  void              inherit_costs   ( CostCentreStack *ccs );
static  void              reportCCS       ( CostCentreStack *ccs, nat indent );
static  void              DecCCS          ( CostCentreStack *ccs );
static  void              DecBackEdge     ( CostCentreStack *ccs, 
					    CostCentreStack *oldccs );
static  CostCentreStack * CheckLoop       ( CostCentreStack *ccs, CostCentre *cc );
static  CostCentreStack * pruneCCSTree    ( CostCentreStack *ccs );
static  CostCentreStack * ActualPush      ( CostCentreStack *, CostCentre * );
static  CostCentreStack * IsInIndexTable  ( IndexTable *, CostCentre * );
static  IndexTable *      AddToIndexTable ( IndexTable *, CostCentreStack *, 
					    CostCentre *, unsigned int );

#ifdef DEBUG
static    void printCCS            ( CostCentreStack *ccs );
#endif
static    void initTimeProfiling   ( void );
static    void initProfilingLogFile( void );

static    void reportCCS_XML       ( CostCentreStack *ccs );

/* -----------------------------------------------------------------------------
   Initialise the profiling environment
   -------------------------------------------------------------------------- */

void
initProfiling1 (void)
{
  /* for the benefit of allocate()... */
  CCCS = CCS_SYSTEM;
  
  /* Initialize counters for IDs */
  CC_ID  = 1;
  CCS_ID = 1;
  HP_ID  = 1;
  
  /* Initialize Declaration lists to NULL */
  CC_LIST  = NULL;
  CCS_LIST = NULL;

  /* Register all the cost centres / stacks in the program 
   * CC_MAIN gets link = 0, all others have non-zero link.
   */
  REGISTER_CC(CC_MAIN);
  REGISTER_CC(CC_SYSTEM);
  REGISTER_CC(CC_GC);
  REGISTER_CC(CC_OVERHEAD);
  REGISTER_CC(CC_SUBSUMED);
  REGISTER_CC(CC_DONT_CARE);
  REGISTER_CCS(CCS_MAIN);
  REGISTER_CCS(CCS_SYSTEM);
  REGISTER_CCS(CCS_GC);
  REGISTER_CCS(CCS_OVERHEAD);
  REGISTER_CCS(CCS_SUBSUMED);
  REGISTER_CCS(CCS_DONT_CARE);

  CCCS = CCS_OVERHEAD;

  /* cost centres are registered by the per-module 
   * initialisation code now... 
   */
}

void
initProfiling2 (void)
{
  CostCentreStack *ccs, *next;

  CCCS = CCS_SYSTEM;

  /* Set up the log file, and dump the header and cost centre
   * information into it.  */
  initProfilingLogFile();

  /* find all the "special" cost centre stacks, and make them children
   * of CCS_MAIN.
   */
  ASSERT(CCS_MAIN->prevStack == 0);
  CCS_MAIN->root = CC_MAIN;
  DecCCS(CCS_MAIN);
  for (ccs = CCS_LIST; ccs != CCS_MAIN; ) {
    next = ccs->prevStack;
    ccs->prevStack = 0;
    ActualPush_(CCS_MAIN,ccs->cc,ccs);
    ccs->root = ccs->cc;
    ccs = next;
  }
  
  if (RtsFlags.CcFlags.doCostCentres) {
    initTimeProfiling();
  }

  if (RtsFlags.ProfFlags.doHeapProfile) {
    initHeapProfiling();
  }
}
  
static void
initProfilingLogFile(void)
{
    /* Initialise the log file name */
    prof_filename = stgMallocBytes(strlen(prog_argv[0]) + 6, "initProfiling");
    sprintf(prof_filename, "%s.prof", prog_argv[0]);

    /* open the log file */
    if ((prof_file = fopen(prof_filename, "w")) == NULL) {
	fprintf(stderr, "Can't open profiling report file %s\n", prof_filename);
	RtsFlags.CcFlags.doCostCentres = 0;
	return;
    }

    if (RtsFlags.CcFlags.doCostCentres == COST_CENTRES_XML) {
	/* dump the time, and the profiling interval */
	fprintf(prof_file, "\"%s\"\n", time_str());
	fprintf(prof_file, "\"%d ms\"\n", TICK_MILLISECS);
	
	/* declare all the cost centres */
	{
	    CostCentre *cc;
	    for (cc = CC_LIST; cc != NULL; cc = cc->link) {
		fprintf(prof_file, "%d %d \"%s\" \"%s\"\n",
			CC_UQ, cc->ccID, cc->label, cc->module);
	    }
	}
    }
    
    if (RtsFlags.ProfFlags.doHeapProfile) {
	/* Initialise the log file name */
	hp_filename = stgMallocBytes(strlen(prog_argv[0]) + 6, "initProfiling");
	sprintf(hp_filename, "%s.hp", prog_argv[0]);
	
	/* open the log file */
	if ((hp_file = fopen(hp_filename, "w")) == NULL) {
	    fprintf(stderr, "Can't open profiling report file %s\n", 
		    hp_filename);
	    RtsFlags.ProfFlags.doHeapProfile = 0;
	    return;
	}
    }
}

void
initTimeProfiling(void)
{
  time_profiling = rtsTrue;

  /* Start ticking */
  startProfTimer();
};

void 
endProfiling ( void )
{
  if (RtsFlags.CcFlags.doCostCentres) {
    stopProfTimer();
  }
  if (RtsFlags.ProfFlags.doHeapProfile) {
    endHeapProfiling();
  }
}

/* -----------------------------------------------------------------------------
   Set cost centre stack when entering a function.
   -------------------------------------------------------------------------- */
rtsBool entering_PAP;

CostCentreStack *
EnterFunCCS ( CostCentreStack *cccs, CostCentreStack *ccsfn )
{
  /* PAP_entry has already set CCCS for us */
  if (entering_PAP) {
    entering_PAP = rtsFalse;
    return CCCS;
  }

  if (ccsfn->root->is_caf == CC_IS_CAF) {
    return AppendCCS(cccs,ccsfn);
  } else {
    return ccsfn;
  }
}

/* -----------------------------------------------------------------------------
   Cost-centre stack manipulation
   -------------------------------------------------------------------------- */

#ifdef DEBUG
CostCentreStack * _PushCostCentre ( CostCentreStack *ccs, CostCentre *cc );
CostCentreStack *
PushCostCentre ( CostCentreStack *ccs, CostCentre *cc )
#define PushCostCentre _PushCostCentre
{
  IF_DEBUG(prof, 
	   fprintf(stderr,"Pushing %s on ", cc->label);
	   printCCS(ccs);
	   fprintf(stderr,"\n"));
  return PushCostCentre(ccs,cc);
}
#endif

CostCentreStack *
PushCostCentre ( CostCentreStack *ccs, CostCentre *cc )
{
  CostCentreStack *temp_ccs;
  
  if (ccs == EMPTY_STACK)
    return ActualPush(ccs,cc);
  else {
    if (ccs->cc == cc)
      return ccs;
    else {
      /* check if we've already memoized this stack */
      temp_ccs = IsInIndexTable(ccs->indexTable,cc);
      
      if (temp_ccs != EMPTY_STACK)
	return temp_ccs;
      else {
	temp_ccs = CheckLoop(ccs,cc);
	if (temp_ccs != NULL) {
	  /* we have recursed to an older CCS.  Mark this in
	   * the index table, and emit a "back edge" into the
	   * log file.
	   */
	  ccs->indexTable = AddToIndexTable(ccs->indexTable,temp_ccs,cc,1);
	  DecBackEdge(temp_ccs,ccs);
	  return temp_ccs;
	} else {
	  return ActualPush(ccs,cc);
	}
      }
    }
  }
}

static CostCentreStack *
CheckLoop ( CostCentreStack *ccs, CostCentre *cc )
{
  while (ccs != EMPTY_STACK) {
    if (ccs->cc == cc)
      return ccs;
    ccs = ccs->prevStack;
  }
  return NULL;
}

/* Append ccs1 to ccs2 (ignoring any CAF cost centre at the root of ccs1 */

#ifdef DEBUG
CostCentreStack *_AppendCCS ( CostCentreStack *ccs1, CostCentreStack *ccs2 );
CostCentreStack *
AppendCCS ( CostCentreStack *ccs1, CostCentreStack *ccs2 )
#define AppendCCS _AppendCCS
{
  IF_DEBUG(prof, 
	   if (ccs1 != ccs2) {
	     fprintf(stderr,"Appending ");
	     printCCS(ccs1);
	     fprintf(stderr," to ");
	     printCCS(ccs2);
	     fprintf(stderr,"\n");});
  return AppendCCS(ccs1,ccs2);
}
#endif

CostCentreStack *
AppendCCS ( CostCentreStack *ccs1, CostCentreStack *ccs2 )
{
  CostCentreStack *ccs = NULL;

  if (ccs1 == ccs2) {
    return ccs1;
  }

  if (ccs2->cc->is_caf == CC_IS_CAF) {
    return ccs1;
  }
  
  if (ccs2->prevStack != NULL) {
    ccs = AppendCCS(ccs1, ccs2->prevStack);
  }

  return PushCostCentre(ccs,ccs2->cc);
}

static CostCentreStack *
ActualPush ( CostCentreStack *ccs, CostCentre *cc )
{
  CostCentreStack *new_ccs;
  
  /* allocate space for a new CostCentreStack */
  new_ccs = (CostCentreStack *) stgMallocBytes(sizeof(CostCentreStack), "Error allocating space for CostCentreStack");
  
  return ActualPush_(ccs, cc, new_ccs);
}

static CostCentreStack *
ActualPush_ ( CostCentreStack *ccs, CostCentre *cc, CostCentreStack *new_ccs )
{
  /* assign values to each member of the structure */
  ASSIGN_CCS_ID(new_ccs->ccsID);
  
  new_ccs->cc = cc;
  new_ccs->prevStack = ccs;
  
  new_ccs->indexTable = EMPTY_TABLE;
  
  /* Initialise the various _scc_ counters to zero
   */
  new_ccs->scc_count        = 0;
  
  /* Initialize all other stats here.  There should be a quick way
   * that's easily used elsewhere too 
   */
  new_ccs->time_ticks = 0;
  new_ccs->mem_alloc = 0;
  new_ccs->inherited_ticks = 0;
  new_ccs->inherited_alloc = 0;
  
  new_ccs->root = ccs->root;

  /* update the memoization table for the parent stack */
  if (ccs != EMPTY_STACK)
    ccs->indexTable = AddToIndexTable(ccs->indexTable, new_ccs, cc, 
				      0/*not a back edge*/);
  
  /* make sure this CC is declared at the next heap/time sample */
  DecCCS(new_ccs);
  
  /* return a pointer to the new stack */
  return new_ccs;
}


static CostCentreStack *
IsInIndexTable(IndexTable *it, CostCentre *cc)
{
  while (it!=EMPTY_TABLE)
    {
      if (it->cc==cc)
	return it->ccs;
      else
	it = it->next;
    }
  
  /* otherwise we never found it so return EMPTY_TABLE */
  return EMPTY_TABLE;
}


static IndexTable *
AddToIndexTable(IndexTable *it, CostCentreStack *new_ccs, 
		CostCentre *cc, unsigned int back_edge)
{
  IndexTable *new_it;
  
  new_it = stgMallocBytes(sizeof(IndexTable), "AddToIndexTable");
  
  new_it->cc = cc;
  new_it->ccs = new_ccs;
  new_it->next = it;
  new_it->back_edge = back_edge;
  return new_it;
}


static void
DecCCS(CostCentreStack *ccs)
{
  if (prof_file && RtsFlags.CcFlags.doCostCentres == COST_CENTRES_XML) {
    if (ccs->prevStack == EMPTY_STACK)
      fprintf(prof_file, "%d %d 1 %d\n", CCS_UQ, 
	      ccs->ccsID, ccs->cc->ccID);
    else
      fprintf(prof_file, "%d %d 2 %d %d\n", CCS_UQ, 
	      ccs->ccsID, ccs->cc->ccID, ccs->prevStack->ccsID);
  }
}

static void
DecBackEdge( CostCentreStack *ccs, CostCentreStack *oldccs )
{
  if (prof_file && RtsFlags.CcFlags.doCostCentres == COST_CENTRES_XML) {
    if (ccs->prevStack == EMPTY_STACK)
      fprintf(prof_file, "%d %d 1 %d\n", CCS_UQ, 
	      ccs->ccsID, ccs->cc->ccID);
    else
      fprintf(prof_file, "%d %d 2 %d %d\n", CCS_UQ, 
	      ccs->ccsID, ccs->cc->ccID, oldccs->ccsID);
  }
}

/* -----------------------------------------------------------------------------
   Generating a time & allocation profiling report.
   -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
   Generating the aggregated per-cost-centre time/alloc report.
   -------------------------------------------------------------------------- */

static CostCentre *sorted_cc_list;

static void
aggregate_cc_costs( CostCentreStack *ccs )
{
  IndexTable *i;

  ccs->cc->mem_alloc += ccs->mem_alloc;
  ccs->cc->time_ticks += ccs->time_ticks;

  for (i = ccs->indexTable; i != 0; i = i->next) {
    if (!i->back_edge) {
      aggregate_cc_costs(i->ccs);
    }
  }
}

static void
insert_cc_in_sorted_list( CostCentre *new_cc )
{
  CostCentre **prev, *cc;

  prev = &sorted_cc_list;
  for (cc = sorted_cc_list; cc != NULL; cc = cc->link) {
    if (new_cc->time_ticks > cc->time_ticks) {
      new_cc->link = cc;
      *prev = new_cc;
      return;
    } else {
      prev = &(cc->link);
    }
  }
  new_cc->link = NULL;
  *prev = new_cc;
}

static void
report_per_cc_costs( void )
{
  CostCentre *cc, *next;

  aggregate_cc_costs(CCS_MAIN);
  sorted_cc_list = NULL;

  for (cc = CC_LIST; cc != NULL; cc = next) {
    next = cc->link;
    if (cc->time_ticks > total_prof_ticks/100
	|| cc->mem_alloc > total_alloc/100) {
      insert_cc_in_sorted_list(cc);
    }
  }
  
  fprintf(prof_file, "%-20s %-10s", "COST CENTRE", "MODULE");  
  fprintf(prof_file, "%6s %6s", "%time", "%alloc");
  if (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_VERBOSE) {
    fprintf(prof_file, "  %5s %9s", "ticks", "bytes");
  }
  fprintf(prof_file, "\n\n");

  for (cc = sorted_cc_list; cc != NULL; cc = cc->link) {
    fprintf(prof_file, "%-20s %-10s", cc->label, cc->module);
    fprintf(prof_file, "%6.1f %6.1f",
	    total_prof_ticks == 0 ? 0.0 : (cc->time_ticks / (StgFloat) total_prof_ticks * 100),
	    total_alloc == 0 ? 0.0 : (cc->mem_alloc / (StgFloat)
				      total_alloc * 100)
	    );

    if (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_VERBOSE) {
      fprintf(prof_file, "  %5ld %9ld", cc->time_ticks, cc->mem_alloc);
    }
    fprintf(prof_file, "\n");
  }

  fprintf(prof_file,"\n\n");
}

/* -----------------------------------------------------------------------------
   Generate the cost-centre-stack time/alloc report
   -------------------------------------------------------------------------- */

static void 
fprint_header( void )
{
  fprintf(prof_file, "%-24s %-10s           individual     inherited\n", "", "");

  fprintf(prof_file, "%-24s %-10s", "COST CENTRE", "MODULE");  
  fprintf(prof_file, "%8s  %5s %5s   %5s %5s", "entries", "%time", "%alloc", "%time", "%alloc");

  if (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_VERBOSE) {
    fprintf(prof_file, "  %5s %9s", "ticks", "bytes");
#if defined(PROFILING_DETAIL_COUNTS)
    fprintf(prof_file, "  %8s %8s %8s %8s %8s %8s %8s",
	    "closures", "thunks", "funcs", "PAPs", "subfuns", "subcafs", "cafssub");
#endif
  }

  fprintf(prof_file, "\n\n");
}

void
report_ccs_profiling( void )
{
    nat count;
    char temp[128]; /* sigh: magic constant */

    stopProfTimer();

    total_prof_ticks = 0;
    total_alloc = 0;
    count_ticks(CCS_MAIN);
    
    switch (RtsFlags.CcFlags.doCostCentres) {
    case 0:
      return;
    case COST_CENTRES_XML:
      gen_XML_logfile();
      return;
    default:
    }

    fprintf(prof_file, "\t%s Time and Allocation Profiling Report  (%s)\n", 
	    time_str(), "Final");

    fprintf(prof_file, "\n\t  ");
    fprintf(prof_file, " %s", prog_argv[0]);
    fprintf(prof_file, " +RTS");
    for (count = 0; rts_argv[count]; count++)
	fprintf(prof_file, " %s", rts_argv[count]);
    fprintf(prof_file, " -RTS");
    for (count = 1; prog_argv[count]; count++)
	fprintf(prof_file, " %s", prog_argv[count]);
    fprintf(prof_file, "\n\n");

    fprintf(prof_file, "\ttotal time  = %11.2f secs   (%lu ticks @ %d ms)\n",
	    total_prof_ticks / (StgFloat) TICK_FREQUENCY, 
	    total_prof_ticks, TICK_MILLISECS);

    fprintf(prof_file, "\ttotal alloc = %11s bytes",
	    ullong_format_string((ullong) total_alloc * sizeof(W_),
				 temp, rtsTrue/*commas*/));
    /* ToDo: 64-bit error! */

#if defined(PROFILING_DETAIL_COUNTS)
    fprintf(prof_file, "  (%lu closures)", total_allocs);
#endif
    fprintf(prof_file, "  (excludes profiling overheads)\n\n");

    report_per_cc_costs();

    inherit_costs(CCS_MAIN);

    fprint_header();
    reportCCS(pruneCCSTree(CCS_MAIN), 0);

    fclose(prof_file);
}

static void 
reportCCS(CostCentreStack *ccs, nat indent)
{
  CostCentre *cc;
  IndexTable *i;

  cc = ccs->cc;
  
  /* Only print cost centres with non 0 data ! */
  
  if ( RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_ALL ||
       ! ccs_to_ignore(ccs))
	/* force printing of *all* cost centres if -P -P */ 
    {

    fprintf(prof_file, "%-*s%-*s %-10s", 
	    indent, "", 24-indent, cc->label, cc->module);

    fprintf(prof_file, "%8ld  %5.1f %5.1f    %5.1f %5.1f",
	    ccs->scc_count, 
	    total_prof_ticks == 0 ? 0.0 : (ccs->time_ticks / (StgFloat) total_prof_ticks * 100),
	    total_alloc == 0 ? 0.0 : (ccs->mem_alloc / (StgFloat) total_alloc * 100),
	    total_prof_ticks == 0 ? 0.0 : (ccs->inherited_ticks / (StgFloat) total_prof_ticks * 100),
	    total_alloc == 0 ? 0.0 : (ccs->inherited_alloc / (StgFloat) total_alloc * 100)
	    );

    if (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_VERBOSE) {
      fprintf(prof_file, "  %5ld %9ld", ccs->time_ticks, ccs->mem_alloc*sizeof(W_));
#if defined(PROFILING_DETAIL_COUNTS)
      fprintf(prof_file, "  %8ld %8ld %8ld %8ld %8ld %8ld %8ld",
	      ccs->mem_allocs, ccs->thunk_count,
	      ccs->function_count, ccs->pap_count,
	      ccs->subsumed_fun_count,	ccs->subsumed_caf_count,
	      ccs->caffun_subsumed);
#endif
    }
    fprintf(prof_file, "\n");
  }

  for (i = ccs->indexTable; i != 0; i = i->next) {
    if (!i->back_edge) {
      reportCCS(i->ccs, indent+1);
    }
  }
}

/* Traverse the cost centre stack tree and accumulate
 * ticks/allocations.
 */
static void
count_ticks(CostCentreStack *ccs)
{
  IndexTable *i;
  
  if (!ccs_to_ignore(ccs)) {
    total_alloc += ccs->mem_alloc;
    total_prof_ticks += ccs->time_ticks;
  }
  for (i = ccs->indexTable; i != NULL; i = i->next)
    if (!i->back_edge) {
      count_ticks(i->ccs);
    }
}

/* Traverse the cost centre stack tree and inherit ticks & allocs.
 */
static void
inherit_costs(CostCentreStack *ccs)
{
  IndexTable *i;

  if (ccs_to_ignore(ccs)) { return; }

  ccs->inherited_ticks += ccs->time_ticks;
  ccs->inherited_alloc += ccs->mem_alloc;

  for (i = ccs->indexTable; i != NULL; i = i->next)
      if (!i->back_edge) {
	  inherit_costs(i->ccs);
	  ccs->inherited_ticks += i->ccs->inherited_ticks;
	  ccs->inherited_alloc += i->ccs->inherited_alloc;
      }
  
  return;
}

/* return rtsTrue if it is one of the ones that
 * should not be reported normally (because it confuses
 * the users)
 */
static rtsBool
ccs_to_ignore (CostCentreStack *ccs)
{
    if (    ccs == CCS_OVERHEAD 
	 || ccs == CCS_DONT_CARE
	 || ccs == CCS_GC 
	 || ccs == CCS_SYSTEM) {
	return rtsTrue;
    } else {
	return rtsFalse;
    }
}

static CostCentreStack *
pruneCCSTree( CostCentreStack *ccs )
{
  CostCentreStack *ccs1;
  IndexTable *i, **prev;
  
  prev = &ccs->indexTable;
  for (i = ccs->indexTable; i != 0; i = i->next) {
    if (i->back_edge) { continue; }

    ccs1 = pruneCCSTree(i->ccs);
    if (ccs1 == NULL) {
      *prev = i->next;
    } else {
      prev = &(i->next);
    }
  }

  if ( (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_ALL
	/* force printing of *all* cost centres if -P -P */ )
       
       || ( ccs->indexTable != 0 )
       || ( ccs->scc_count || ccs->time_ticks || ccs->mem_alloc )
      ) {
      return ccs;
  } else {
      return NULL;
  }
}

/* -----------------------------------------------------------------------------
   Generate the XML time/allocation profile
   -------------------------------------------------------------------------- */

void
gen_XML_logfile( void )
{
  fprintf(prof_file, "%d %lu", TIME_UPD_UQ, total_prof_ticks);

  reportCCS_XML(pruneCCSTree(CCS_MAIN));

  fprintf(prof_file, " 0\n");

  fclose(prof_file);
}

static void 
reportCCS_XML(CostCentreStack *ccs)
{
  CostCentre *cc;
  IndexTable *i;

  if (ccs_to_ignore(ccs)) { return; }

  cc = ccs->cc;
  
  fprintf(prof_file, " 1 %d %lu %lu %lu", 
	  ccs->ccsID, ccs->scc_count, ccs->time_ticks, ccs->mem_alloc);

  for (i = ccs->indexTable; i != 0; i = i->next) {
    if (!i->back_edge) {
      reportCCS_XML(i->ccs);
    }
  }
}

void
print_ccs (FILE *fp, CostCentreStack *ccs)
{
  if (ccs == CCCS) {
    fprintf(fp, "Cost-Centre Stack: ");
  }
  
  if (ccs != CCS_MAIN)
    {
      print_ccs(fp, ccs->prevStack);
      fprintf(fp, "->[%s,%s]", ccs->cc->label, ccs->cc->module);
    } else {
      fprintf(fp, "[%s,%s]", ccs->cc->label, ccs->cc->module);
    }

  if (ccs == CCCS) {
    fprintf(fp, "\n");
  }
}


#ifdef DEBUG
static void
printCCS ( CostCentreStack *ccs )
{
  fprintf(stderr,"<");
  for (; ccs; ccs = ccs->prevStack ) {
    fprintf(stderr,ccs->cc->label);
    if (ccs->prevStack) {
      fprintf(stderr,",");
    }
  }
  fprintf(stderr,">");
}
#endif

#endif /* PROFILING */