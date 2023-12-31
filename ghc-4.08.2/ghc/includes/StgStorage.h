/* -----------------------------------------------------------------------------
 * $Id: StgStorage.h,v 1.7 2000/04/11 16:36:53 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * STG Storage Manager Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGSTORAGE_H
#define STGSTORAGE_H

/* GENERATION GC NOTES
 *
 * We support an arbitrary number of generations, with an arbitrary number
 * of steps per generation.  Notes (in no particular order):
 *
 *       - all generations except the oldest should have two steps.  This gives
 *         objects a decent chance to age before being promoted, and in
 *         particular will ensure that we don't end up with too many
 *         thunks being updated in older generations.
 *
 *       - the oldest generation has one step.  There's no point in aging
 *         objects in the oldest generation.
 *
 *       - generation 0, step 0 (G0S0) is the allocation area.  It is given
 *         a fixed set of blocks during initialisation, and these blocks
 *         are never freed.
 *
 *       - during garbage collection, each step which is an evacuation
 *         destination (i.e. all steps except G0S0) is allocated a to-space.
 *         evacuated objects are allocated into the step's to-space until
 *         GC is finished, when the original step's contents may be freed
 *         and replaced by the to-space.
 *
 *       - the mutable-list is per-generation (not per-step).  G0 doesn't 
 *         have one (since every garbage collection collects at least G0).
 * 
 *       - block descriptors contain pointers to both the step and the
 *         generation that the block belongs to, for convenience.
 *
 *       - static objects are stored in per-generation lists.  See GC.c for
 *         details of how we collect CAFs in the generational scheme.
 *
 *       - large objects are per-step, and are promoted in the same way
 *         as small objects, except that we may allocate large objects into
 *         generation 1 initially.
 */

typedef struct _step {
  unsigned int no;		/* step number */
  bdescr *blocks;		/* blocks in this step */
  unsigned int n_blocks;	/* number of blocks */
  struct _step *to;		/* where collected objects from this step go */
  struct _generation *gen;	/* generation this step belongs to */
  bdescr *large_objects;	/* large objects (doubly linked) */

  /* temporary use during GC: */
  StgPtr  hp;			/* next free locn in to-space */
  StgPtr  hpLim;		/* end of current to-space block */
  bdescr *hp_bd;		/* bdescr of current to-space block */
  bdescr *to_space;		/* bdescr of first to-space block */
  unsigned int to_blocks;		/* number of blocks in to-space */
  bdescr *scan_bd;		/* block currently being scanned */
  StgPtr  scan;			/* scan pointer in current block */
  bdescr *new_large_objects;    /* large objects collected so far */
  bdescr *scavenged_large_objects; /* live large objects after GC (dbl link) */
} step;

typedef struct _generation {
  unsigned int no;		/* generation number */
  step *steps;			/* steps */
  unsigned int n_steps;		/* number of steps */
  unsigned int max_blocks;	/* max blocks in step 0 */
  StgMutClosure *mut_list;      /* mutable objects in this generation (not G0)*/
  StgMutClosure *mut_once_list; /* objects that point to younger generations */

  /* temporary use during GC: */
  StgMutClosure *saved_mut_list;

  /* stats information */
  unsigned int collections;
  unsigned int failed_promotions;
} generation;

/* -----------------------------------------------------------------------------
   Allocation area for compiled code

   OpenNursery(hp,hplim)        Opens the allocation area, and sets hp
   				and hplim appropriately.

   CloseNursery(hp)		Closes the allocation area.

   PleaseStopAllocating(void)   Arranges that the next call to
   				ExtendNursery() will fail, triggering
				a return to the scheduler.  This is
				useful for asynchronous interupts etc.
   -------------------------------------------------------------------------- */

#define OpenNursery(hp,hplim)				\
  (hp    = CurrentNursery->free-1,			\
   hplim = CurrentNursery->start + BLOCK_SIZE_W - 1)
  
#define CloseNursery(hp)  (CurrentNursery->free = (P_)(hp)+1)

/* -----------------------------------------------------------------------------
   Trigger a GC from Haskell land.
   -------------------------------------------------------------------------- */

extern void performGC(void);
extern void performMajorGC(void);
extern void performGCWithRoots(void (*get_roots)(void));

#endif /* STGSTORAGE_H */
