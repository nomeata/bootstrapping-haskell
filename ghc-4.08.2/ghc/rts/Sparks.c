/* ---------------------------------------------------------------------------
 * $Id: Sparks.c,v 1.2 2000/03/31 03:09:36 hwloidl Exp $
 *
 * (c) The GHC Team, 2000
 *
 * Sparking support for PAR and SMP versions of the RTS.
 *
 * -------------------------------------------------------------------------*/

//@node Spark Management Routines, , ,
//@section Spark Management Routines

//@menu
//* Includes::			
//* GUM code::			
//* GranSim code::		
//@end menu

//@node Includes, GUM code, Spark Management Routines, Spark Management Routines
//@subsection Includes

#include "Rts.h"
#include "Schedule.h"
#include "SchedAPI.h"
#include "Storage.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
# if defined(PAR)
# include "ParallelRts.h"
# elif defined(GRAN)
# include "GranSimRts.h"
# endif
#include "Sparks.h"

#if defined(SMP) || defined(PAR)

//@node GUM code, GranSim code, Includes, Spark Management Routines
//@subsection GUM code

static void slide_spark_pool( StgSparkPool *pool );

void
initSparkPools( void )
{
  Capability *cap;
  StgSparkPool *pool;

#ifdef SMP
  /* walk over the capabilities, allocating a spark pool for each one */
  for (cap = free_capabilities; cap != NULL; cap = cap->link) {
#else
  /* allocate a single spark pool */
  cap = &MainRegTable;
  {
#endif
    pool = &(cap->rSparks);
    
    pool->base = stgMallocBytes(RtsFlags.ParFlags.maxLocalSparks
				     * sizeof(StgClosure *),
				     "initSparkPools");
    pool->lim = pool->base + RtsFlags.ParFlags.maxLocalSparks;
    pool->hd  = pool->base;
    pool->tl  = pool->base;
  }
}

StgClosure *
findSpark( void )
{
  Capability *cap;
  StgSparkPool *pool;
  StgClosure *spark;

#ifdef SMP
  /* walk over the capabilities, allocating a spark pool for each one */
  for (cap = free_capabilities; cap != NULL; cap = cap->link) {
#else
  /* allocate a single spark pool */
  cap = &MainRegTable;
  {
#endif
    pool = &(cap->rSparks);
    while (pool->hd < pool->tl) {
      spark = *pool->hd++;
      if (closure_SHOULD_SPARK(spark))
	return spark;
    }
    slide_spark_pool(pool);
  }
  return NULL;
}

rtsBool
add_to_spark_queue( StgClosure *closure, StgSparkPool *pool )
{
  if (pool->tl == pool->lim)
    slide_spark_pool(pool);

  if (closure_SHOULD_SPARK(closure) && 
      pool->tl < pool->lim) {
    *(pool->tl++) = closure;
    return rtsTrue;
  } else {
    return rtsFalse;
  }
}

static void
slide_spark_pool( StgSparkPool *pool )
{
  StgClosure **sparkp, **to_sparkp;

  sparkp = pool->hd;
  to_sparkp = pool->base;
  while (sparkp < pool->tl) {
    ASSERT(to_sparkp<=sparkp);
    ASSERT(*sparkp!=NULL);
    ASSERT(LOOKS_LIKE_GHC_INFO((*sparkp)->header.info));

    if (closure_SHOULD_SPARK(*sparkp)) {
      *to_sparkp++ = *sparkp++;
    } else {
      sparkp++;
    }
  }
  pool->hd = pool->base;
  pool->tl = to_sparkp;
}

nat
spark_queue_len( StgSparkPool *pool ) 
{
  return (nat) (pool->tl - pool->hd);
}

/* Mark all nodes pointed to by sparks in the spark queues (for GC) Does an
   implicit slide i.e. after marking all sparks are at the beginning of the
   spark pool and the spark pool only contains sparkable closures 
*/
void
markSparkQueue( void )
{ 
  StgClosure **sparkp, **to_sparkp;
#ifdef DEBUG
  nat n, pruned_sparks;
#endif
  StgSparkPool *pool;
  Capability *cap;

#ifdef SMP
  /* walk over the capabilities, allocating a spark pool for each one */
  for (cap = free_capabilities; cap != NULL; cap = cap->link) {
#else
  /* allocate a single spark pool */
  cap = &MainRegTable;
  {
#endif
    pool = &(cap->rSparks);
    
#ifdef DEBUG
    n = 0;
    pruned_sparks = 0;
#endif

    sparkp = pool->hd;
    to_sparkp = pool->base;
    while (sparkp < pool->tl) {
      ASSERT(to_sparkp<=sparkp);
      ASSERT(*sparkp!=NULL);
      ASSERT(LOOKS_LIKE_GHC_INFO(((StgClosure *)*sparkp)->header.info));
      // ToDo?: statistics gathering here (also for GUM!)
      if (closure_SHOULD_SPARK(*sparkp)) {
	*to_sparkp = MarkRoot(*sparkp);
	to_sparkp++;
#ifdef DEBUG
	n++;
#endif
      } else {
#ifdef DEBUG
	pruned_sparks++;
#endif
      }
      sparkp++;
    }
    pool->hd = pool->base;
    pool->tl = to_sparkp;

#if defined(SMP)
    IF_DEBUG(scheduler,
	     belch("markSparkQueue: marked %d sparks and pruned %d sparks on [%x]",
		   n, pruned_sparks, pthread_self()));
#elif defined(PAR)
    IF_DEBUG(scheduler,
	     belch("markSparkQueue: marked %d sparks and pruned %d sparks on [%x]",
		   n, pruned_sparks, mytid));
#else
    IF_DEBUG(scheduler,
	     belch("markSparkQueue: marked %d sparks and pruned %d sparks",
		   n, pruned_sparks));
#endif

    IF_DEBUG(scheduler,
	     belch("markSparkQueue:   new spark queue len=%d; (hd=%p; tl=%p)",
		   spark_queue_len(pool), pool->hd, pool->tl));

  }
}

void
disposeSpark(spark)
StgClosure *spark;
{
#if !defined(SMP)
  Capability *cap;
  StgSparkPool *pool;

  cap = &MainRegTable;
  pool = &(cap->rSparks);
  ASSERT(pool->hd <= pool->tl && pool->tl <= pool->lim);
#endif
  ASSERT(spark != (StgClosure *)NULL);
  /* Do nothing */
}


#elif defined(GRAN)

//@node GranSim code,  , GUM code, Spark Management Routines
//@subsection GranSim code

//@menu
//* Basic interface to sparkq::	 
//* Aux fcts::			
//@end menu

//@node Basic interface to sparkq, Aux fcts, GranSim code, GranSim code
//@subsubsection Basic interface to sparkq
/* 
   Search the spark queue of the proc in event for a spark that's worth
   turning into a thread 
   (was gimme_spark in the old RTS)
*/
//@cindex findLocalSpark
void
findLocalSpark (rtsEvent *event, rtsBool *found_res, rtsSparkQ *spark_res)
{
   PEs proc = event->proc,       /* proc to search for work */
       creator = event->creator; /* proc that requested work */
   StgClosure* node;
   rtsBool found;
   rtsSparkQ spark_of_non_local_node = NULL, 
             spark_of_non_local_node_prev = NULL, 
             low_priority_spark = NULL, 
             low_priority_spark_prev = NULL,
             spark = NULL, prev = NULL;
  
   /* Choose a spark from the local spark queue */
   prev = (rtsSpark*)NULL;
   spark = pending_sparks_hds[proc];
   found = rtsFalse;

   // ToDo: check this code & implement local sparking !! -- HWL  
   while (!found && spark != (rtsSpark*)NULL)
     {
       ASSERT((prev!=(rtsSpark*)NULL || spark==pending_sparks_hds[proc]) &&
	      (prev==(rtsSpark*)NULL || prev->next==spark) &&
	      (spark->prev==prev));
       node = spark->node;
       if (!closure_SHOULD_SPARK(node)) 
         {
	   IF_GRAN_DEBUG(checkSparkQ,
			 belch("^^ pruning spark %p (node %p) in gimme_spark",
			       spark, node));

           if (RtsFlags.GranFlags.GranSimStats.Sparks)
             DumpRawGranEvent(proc, (PEs)0, SP_PRUNED,(StgTSO*)NULL,
			      spark->node, spark->name, spark_queue_len(proc));
  
	   ASSERT(spark != (rtsSpark*)NULL);
	   ASSERT(SparksAvail>0);
	   --SparksAvail;

	   ASSERT(prev==(rtsSpark*)NULL || prev->next==spark);
	   spark = delete_from_sparkq (spark, proc, rtsTrue);
	   if (spark != (rtsSpark*)NULL)
	     prev = spark->prev;
	   continue;
         }
       /* -- node should eventually be sparked */
       else if (RtsFlags.GranFlags.PreferSparksOfLocalNodes && 
               !IS_LOCAL_TO(PROCS(node),CurrentProc)) 
         {
	   barf("Local sparking not yet implemented");

           /* Remember first low priority spark */
           if (spark_of_non_local_node==(rtsSpark*)NULL) {
	     spark_of_non_local_node_prev = prev;
             spark_of_non_local_node = spark;
  	      }
  
           if (spark->next == (rtsSpark*)NULL) { 
  	     /* ASSERT(spark==SparkQueueTl);  just for testing */
  	     prev = spark_of_non_local_node_prev;
  	     spark = spark_of_non_local_node;
             found = rtsTrue;
             break;
           }
  
# if defined(GRAN) && defined(GRAN_CHECK)
           /* Should never happen; just for testing 
           if (spark==pending_sparks_tl) {
             fprintf(stderr,"ReSchedule: Last spark != SparkQueueTl\n");
	   	stg_exit(EXIT_FAILURE);
		} */
# endif
  	   prev = spark; 
  	   spark = spark->next;
	   ASSERT(SparksAvail>0);
           --SparksAvail;
	   continue;
         }
       else if ( RtsFlags.GranFlags.DoPrioritySparking || 
  		 (spark->gran_info >= RtsFlags.GranFlags.SparkPriority2) )
         {
	   if (RtsFlags.GranFlags.DoPrioritySparking)
	     barf("Priority sparking not yet implemented");

           found = rtsTrue;
         }
#if 0	   
       else /* only used if SparkPriority2 is defined */
         {
	   /* ToDo: fix the code below and re-integrate it */
           /* Remember first low priority spark */
           if (low_priority_spark==(rtsSpark*)NULL) { 
	     low_priority_spark_prev = prev;
             low_priority_spark = spark;
	   }
  
           if (spark->next == (rtsSpark*)NULL) { 
	        /* ASSERT(spark==spark_queue_tl);  just for testing */
	     prev = low_priority_spark_prev;
	     spark = low_priority_spark;
             found = rtsTrue;       /* take low pri spark => rc is 2  */
             break;
           }
  
           /* Should never happen; just for testing 
           if (spark==pending_sparks_tl) {
             fprintf(stderr,"ReSchedule: Last spark != SparkQueueTl\n");
  		stg_exit(EXIT_FAILURE);
             break;
	   } */                
	   prev = spark; 
	   spark = spark->next;

	   IF_GRAN_DEBUG(pri,
			 belch("++ Ignoring spark of priority %u (SparkPriority=%u); node=%p; name=%u\n", 
			       spark->gran_info, RtsFlags.GranFlags.SparkPriority, 
			       spark->node, spark->name);)
           }
#endif
   }  /* while (spark!=NULL && !found) */

   *spark_res = spark;
   *found_res = found;
}

/*
  Turn the spark into a thread.
  In GranSim this basically means scheduling a StartThread event for the
  node pointed to by the spark at some point in the future.
  (was munch_spark in the old RTS)
*/
//@cindex activateSpark
rtsBool
activateSpark (rtsEvent *event, rtsSparkQ spark) 
{
  PEs proc = event->proc,       /* proc to search for work */
      creator = event->creator; /* proc that requested work */
  StgTSO* tso;
  StgClosure* node;
  rtsTime spark_arrival_time;

  /* 
     We've found a node on PE proc requested by PE creator.
     If proc==creator we can turn the spark into a thread immediately;
     otherwise we schedule a MoveSpark event on the requesting PE
  */
     
  /* DaH Qu' yIchen */
  if (proc!=creator) { 

    /* only possible if we simulate GUM style fishing */
    ASSERT(RtsFlags.GranFlags.Fishing);

    /* Message packing costs for sending a Fish; qeq jabbI'ID */
    CurrentTime[proc] += RtsFlags.GranFlags.Costs.mpacktime;
  
    if (RtsFlags.GranFlags.GranSimStats.Sparks)
      DumpRawGranEvent(proc, (PEs)0, SP_EXPORTED,
		       (StgTSO*)NULL, spark->node,
		       spark->name, spark_queue_len(proc));

    /* time of the spark arrival on the remote PE */
    spark_arrival_time = CurrentTime[proc] + RtsFlags.GranFlags.Costs.latency;

    new_event(creator, proc, spark_arrival_time,
	      MoveSpark,
	      (StgTSO*)NULL, spark->node, spark);

    CurrentTime[proc] += RtsFlags.GranFlags.Costs.mtidytime;
	    
  } else { /* proc==creator i.e. turn the spark into a thread */

    if ( RtsFlags.GranFlags.GranSimStats.Global && 
	 spark->gran_info < RtsFlags.GranFlags.SparkPriority2 ) {

      globalGranStats.tot_low_pri_sparks++;
      IF_GRAN_DEBUG(pri,
		    belch("++ No high priority spark available; low priority (%u) spark chosen: node=%p; name=%u\n",
			  spark->gran_info, 
			  spark->node, spark->name);)
    } 
    
    CurrentTime[proc] += RtsFlags.GranFlags.Costs.threadcreatetime;
    
    node = spark->node;
    
# if 0
    /* ToDo: fix the GC interface and move to StartThread handling-- HWL */
    if (GARBAGE COLLECTION IS NECESSARY) {
      /* Some kind of backoff needed here in case there's too little heap */
#  if defined(GRAN_CHECK) && defined(GRAN)
      if (RtsFlags.GcFlags.giveStats)
	fprintf(RtsFlags.GcFlags.statsFile,"***** vIS Qu' chen veQ boSwI'; spark=%p, node=%p;  name=%u\n", 
		/* (found==2 ? "no hi pri spark" : "hi pri spark"), */
		spark, node, spark->name);
#  endif
      new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc]+1,
    		  FindWork,
    		  (StgTSO*)NULL, (StgClosure*)NULL, (rtsSpark*)NULL);
      barf("//// activateSpark: out of heap ; ToDo: call GarbageCollect()");
      GarbageCollect(GetRoots);
      // HWL old: ReallyPerformThreadGC(TSO_HS+TSO_CTS_SIZE,rtsFalse);
      // HWL old: SAVE_Hp -= TSO_HS+TSO_CTS_SIZE;
      spark = NULL;
      return; /* was: continue; */ /* to the next event, eventually */
    }
# endif
    
    if (RtsFlags.GranFlags.GranSimStats.Sparks)
      DumpRawGranEvent(CurrentProc,(PEs)0,SP_USED,(StgTSO*)NULL,
		       spark->node, spark->name,
		       spark_queue_len(CurrentProc));
    
    new_event(proc, proc, CurrentTime[proc],
	      StartThread, 
	      END_TSO_QUEUE, node, spark); // (rtsSpark*)NULL);
    
    procStatus[proc] = Starting;
  }
}

/* -------------------------------------------------------------------------
   This is the main point where handling granularity information comes into
   play. 
   ------------------------------------------------------------------------- */

#define MAX_RAND_PRI    100

/* 
   Granularity info transformers. 
   Applied to the GRAN_INFO field of a spark.
*/
static inline nat  ID(nat x) { return(x); };
static inline nat  INV(nat x) { return(-x); };
static inline nat  IGNORE(nat x) { return (0); };
static inline nat  RAND(nat x) { return ((random() % MAX_RAND_PRI) + 1); }

/* NB: size_info and par_info are currently unused (what a shame!) -- HWL */
//@cindex newSpark
rtsSpark *
newSpark(node,name,gran_info,size_info,par_info,local)
StgClosure *node;
nat name, gran_info, size_info, par_info, local;
{
  nat pri;
  rtsSpark *newspark;

  pri = RtsFlags.GranFlags.RandomPriorities ? RAND(gran_info) :
        RtsFlags.GranFlags.InversePriorities ? INV(gran_info) :
	RtsFlags.GranFlags.IgnorePriorities ? IGNORE(gran_info) :
                           ID(gran_info);

  if ( RtsFlags.GranFlags.SparkPriority!=0 && 
       pri<RtsFlags.GranFlags.SparkPriority ) {
    IF_GRAN_DEBUG(pri,
      belch(",, NewSpark: Ignoring spark of priority %u (SparkPriority=%u); node=%#x; name=%u\n", 
	      pri, RtsFlags.GranFlags.SparkPriority, node, name));
    return ((rtsSpark*)NULL);
  }

  newspark = (rtsSpark*) stgMallocBytes(sizeof(rtsSpark), "NewSpark");
  newspark->prev = newspark->next = (rtsSpark*)NULL;
  newspark->node = node;
  newspark->name = (name==1) ? CurrentTSO->gran.sparkname : name;
  newspark->gran_info = pri;
  newspark->global = !local;      /* Check that with parAt, parAtAbs !!*/

  if (RtsFlags.GranFlags.GranSimStats.Global) {
    globalGranStats.tot_sparks_created++;
    globalGranStats.sparks_created_on_PE[CurrentProc]++;
  }

  return(newspark);
}

//@cindex disposeSpark
void
disposeSpark(spark)
rtsSpark *spark;
{
  ASSERT(spark!=NULL);
  free(spark);
}

//@cindex disposeSparkQ
void 
disposeSparkQ(spark)
rtsSparkQ spark;
{
  if (spark==NULL) 
    return;

  disposeSparkQ(spark->next);

# ifdef GRAN_CHECK
  if (SparksAvail < 0) {
    fprintf(stderr,"disposeSparkQ: SparksAvail<0 after disposing sparkq @ %p\n", &spark);
    print_spark(spark);
  }
# endif

  free(spark);
}

/*
   With PrioritySparking add_to_spark_queue performs an insert sort to keep
   the spark queue sorted. Otherwise the spark is just added to the end of
   the queue. 
*/

//@cindex add_to_spark_queue
void
add_to_spark_queue(spark)
rtsSpark *spark;
{
  rtsSpark *prev = NULL, *next = NULL;
  nat count = 0;
  rtsBool found = rtsFalse;

  if ( spark == (rtsSpark *)NULL ) {
    return;
  }

  if (RtsFlags.GranFlags.DoPrioritySparking && (spark->gran_info != 0) ) {
    /* Priority sparking is enabled i.e. spark queues must be sorted */

    for (prev = NULL, next = pending_sparks_hd, count=0;
	 (next != NULL) && 
	 !(found = (spark->gran_info >= next->gran_info));
	 prev = next, next = next->next, count++) 
     {}

  } else {   /* 'utQo' */
    /* Priority sparking is disabled */
    
    found = rtsFalse;   /* to add it at the end */

  }

  if (found) {
    /* next points to the first spark with a gran_info smaller than that
       of spark; therefore, add spark before next into the spark queue */
    spark->next = next;
    if ( next == NULL ) {
      pending_sparks_tl = spark;
    } else {
      next->prev = spark;
    }
    spark->prev = prev;
    if ( prev == NULL ) {
      pending_sparks_hd = spark;
    } else {
      prev->next = spark;
    }
  } else {  /* (RtsFlags.GranFlags.DoPrioritySparking && !found) || !DoPrioritySparking */
    /* add the spark at the end of the spark queue */
    spark->next = NULL;			       
    spark->prev = pending_sparks_tl;
    if (pending_sparks_hd == NULL)
      pending_sparks_hd = spark;
    else
      pending_sparks_tl->next = spark;
    pending_sparks_tl = spark;	  
  } 
  ++SparksAvail;

  /* add costs for search in priority sparking */
  if (RtsFlags.GranFlags.DoPrioritySparking) {
    CurrentTime[CurrentProc] += count * RtsFlags.GranFlags.Costs.pri_spark_overhead;
  }

  IF_GRAN_DEBUG(checkSparkQ,
		belch("++ Spark stats after adding spark %p (node %p) to queue on PE %d",
		      spark, spark->node, CurrentProc);
		print_sparkq_stats());

#  if defined(GRAN_CHECK)
  if (RtsFlags.GranFlags.Debug.checkSparkQ) {
    for (prev = NULL, next =  pending_sparks_hd;
	 (next != NULL);
	 prev = next, next = next->next) 
      {}
    if ( (prev!=NULL) && (prev!=pending_sparks_tl) )
      fprintf(stderr,"SparkQ inconsistency after adding spark %p: (PE %u) pending_sparks_tl (%p) not end of queue (%p)\n",
	      spark,CurrentProc, 
	      pending_sparks_tl, prev);
  }
#  endif

#  if defined(GRAN_CHECK)
  /* Check if the sparkq is still sorted. Just for testing, really!  */
  if ( RtsFlags.GranFlags.Debug.checkSparkQ &&
       RtsFlags.GranFlags.Debug.pri ) {
    rtsBool sorted = rtsTrue;
    rtsSpark *prev, *next;

    if (pending_sparks_hd == NULL ||
	pending_sparks_hd->next == NULL ) {
      /* just 1 elem => ok */
    } else {
      for (prev = pending_sparks_hd,
	   next = pending_sparks_hd->next;
	   (next != NULL) ;
	   prev = next, next = next->next) {
	sorted = sorted && 
	         (prev->gran_info >= next->gran_info);
      }
    }
    if (!sorted) {
      fprintf(stderr,"ghuH: SPARKQ on PE %d is not sorted:\n",
	      CurrentProc);
      print_sparkq(CurrentProc);
    }
  }
#  endif
}

//@node Aux fcts,  , Basic interface to sparkq, GranSim code
//@subsubsection Aux fcts

//@cindex spark_queue_len
nat
spark_queue_len(proc) 
PEs proc;
{
 rtsSpark *prev, *spark;                     /* prev only for testing !! */
 nat len;

 for (len = 0, prev = NULL, spark = pending_sparks_hds[proc]; 
      spark != NULL; 
      len++, prev = spark, spark = spark->next)
   {}

#  if defined(GRAN_CHECK)
  if ( RtsFlags.GranFlags.Debug.checkSparkQ ) 
    if ( (prev!=NULL) && (prev!=pending_sparks_tls[proc]) )
      fprintf(stderr,"ERROR in spark_queue_len: (PE %u) pending_sparks_tl (%p) not end of queue (%p)\n",
	      proc, pending_sparks_tls[proc], prev);
#  endif

 return (len);
}

/* 
   Take spark out of the spark queue on PE p and nuke the spark. Adjusts
   hd and tl pointers of the spark queue. Returns a pointer to the next
   spark in the queue.
*/
//@cindex delete_from_sparkq
rtsSpark *
delete_from_sparkq (spark, p, dispose_too)     /* unlink and dispose spark */
rtsSpark *spark;
PEs p;
rtsBool dispose_too;
{
  rtsSpark *new_spark;

  if (spark==NULL) 
    barf("delete_from_sparkq: trying to delete NULL spark\n");

#  if defined(GRAN_CHECK)
  if ( RtsFlags.GranFlags.Debug.checkSparkQ ) {
    fprintf(stderr,"## |%p:%p| (%p)<-spark=%p->(%p) <-(%p)\n",
	    pending_sparks_hd, pending_sparks_tl,
	    spark->prev, spark, spark->next, 
	    (spark->next==NULL ? 0 : spark->next->prev));
  }
#  endif

  if (spark->prev==NULL) {
    /* spark is first spark of queue => adjust hd pointer */
    ASSERT(pending_sparks_hds[p]==spark);
    pending_sparks_hds[p] = spark->next;
  } else {
    spark->prev->next = spark->next;
  }
  if (spark->next==NULL) {
    ASSERT(pending_sparks_tls[p]==spark);
    /* spark is first spark of queue => adjust tl pointer */
    pending_sparks_tls[p] = spark->prev;
  } else {
    spark->next->prev = spark->prev;
  }
  new_spark = spark->next;
  
#  if defined(GRAN_CHECK)
  if ( RtsFlags.GranFlags.Debug.checkSparkQ ) {
    fprintf(stderr,"## |%p:%p| (%p)<-spark=%p->(%p) <-(%p); spark=%p will be deleted NOW \n",
	    pending_sparks_hd, pending_sparks_tl,
	    spark->prev, spark, spark->next, 
	    (spark->next==NULL ? 0 : spark->next->prev), spark);
  }
#  endif

  if (dispose_too)
    disposeSpark(spark);
                  
  return new_spark;
}

/* Mark all nodes pointed to by sparks in the spark queues (for GC) */
//@cindex markSparkQueue
void
markSparkQueue(void)
{ 
  StgClosure *MarkRoot(StgClosure *root); // prototype
  PEs p;
  rtsSpark *sp;

  for (p=0; p<RtsFlags.GranFlags.proc; p++)
    for (sp=pending_sparks_hds[p]; sp!=NULL; sp=sp->next) {
      ASSERT(sp->node!=NULL);
      ASSERT(LOOKS_LIKE_GHC_INFO(sp->node->header.info));
      // ToDo?: statistics gathering here (also for GUM!)
      sp->node = (StgClosure *)MarkRoot(sp->node);
    }
  IF_DEBUG(gc,
	   belch("@@ markSparkQueue: spark statistics at start of GC:");
	   print_sparkq_stats());
}

//@cindex print_spark
void
print_spark(spark)
rtsSpark *spark;
{ 
  char str[16];

  if (spark==NULL) {
    fprintf(stderr,"Spark: NIL\n");
    return;
  } else {
    sprintf(str,
	    ((spark->node==NULL) ? "______" : "%#6lx"), 
	    stgCast(StgPtr,spark->node));

    fprintf(stderr,"Spark: Node %8s, Name %#6x, Global %5s, Creator %5x, Prev %6p, Next %6p\n",
	    str, spark->name, 
            ((spark->global)==rtsTrue?"True":"False"), spark->creator, 
            spark->prev, spark->next);
  }
}

//@cindex print_sparkq
void
print_sparkq(proc)
PEs proc;
// rtsSpark *hd;
{
  rtsSpark *x = pending_sparks_hds[proc];

  fprintf(stderr,"Spark Queue of PE %d with root at %p:\n", proc, x);
  for (; x!=(rtsSpark*)NULL; x=x->next) {
    print_spark(x);
  }
}

/* 
   Print a statistics of all spark queues.
*/
//@cindex print_sparkq_stats
void
print_sparkq_stats(void)
{
  PEs p;

  fprintf(stderr, "SparkQs: [");
  for (p=0; p<RtsFlags.GranFlags.proc; p++)
    fprintf(stderr, ", PE %d: %d", p, spark_queue_len(p));
  fprintf(stderr, "\n");
}

#endif
