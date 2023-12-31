/*-----------------------------------------------------------------------------
 * $Id: Hash.c,v 1.1 1999/01/27 12:11:25 simonm Exp $
 *
 * (c) The AQUA Project, Glasgow University, 1995-1998
 * (c) The GHC Team, 1999
 *
 * Dynamically expanding linear hash tables, as described in
 * Per-\AAke Larson, ``Dynamic Hash Tables,'' CACM 31(4), April 1988,
 * pp. 446 -- 457.
 * -------------------------------------------------------------------------- */

#include "Rts.h"
#include "Hash.h"
#include "RtsUtils.h"

#define HSEGSIZE    1024    /* Size of a single hash table segment */
			    /* Also the minimum size of a hash table */
#define HDIRSIZE    1024    /* Size of the segment directory */
			    /* Maximum hash table size is HSEGSIZE * HDIRSIZE */
#define HLOAD	    5	    /* Maximum average load of a single hash bucket */

#define HCHUNK	    (1024 * sizeof(W_) / sizeof(HashList))
			    /* Number of HashList cells to allocate in one go */


/* Linked list of (key, data) pairs for separate chaining */
struct hashlist {
    StgWord key;
    void *data;
    struct hashlist *next;  /* Next cell in bucket chain (same hash value) */
};

typedef struct hashlist HashList;

struct hashtable {
    int split;		    /* Next bucket to split when expanding */
    int max;		    /* Max bucket of smaller table */
    int mask1;		    /* Mask for doing the mod of h_1 (smaller table) */
    int mask2;		    /* Mask for doing the mod of h_2 (larger table) */
    int kcount;		    /* Number of keys */
    int bcount;		    /* Number of buckets */
    HashList **dir[HDIRSIZE];	/* Directory of segments */
};

/* -----------------------------------------------------------------------------
 * Hash first using the smaller table.  If the bucket is less than the
 * next bucket to be split, re-hash using the larger table.
 * -------------------------------------------------------------------------- */

static int
hash(HashTable *table, W_ key)
{
    int bucket;

    /* Strip the boring zero bits */
    key /= sizeof(StgWord);

    /* Mod the size of the hash table (a power of 2) */
    bucket = key & table->mask1;

    if (bucket < table->split) {
	/* Mod the size of the expanded hash table (also a power of 2) */
	bucket = key & table->mask2;
    }
    return bucket;
}

/* -----------------------------------------------------------------------------
 * Allocate a new segment of the dynamically growing hash table.
 * -------------------------------------------------------------------------- */

static void
allocSegment(HashTable *table, int segment)
{
    table->dir[segment] = stgMallocBytes(HSEGSIZE * sizeof(HashList *), 
					 "allocSegment");
}


/* -----------------------------------------------------------------------------
 * Expand the larger hash table by one bucket, and split one bucket
 * from the smaller table into two parts.  Only the bucket referenced
 * by @table->split@ is affected by the expansion.
 * -------------------------------------------------------------------------- */

static void
expand(HashTable *table)
{
    int oldsegment;
    int oldindex;
    int newbucket;
    int newsegment;
    int newindex;
    HashList *hl;
    HashList *next;
    HashList *old, *new;

    if (table->split + table->max >= HDIRSIZE * HSEGSIZE)
	/* Wow!  That's big.  Too big, so don't expand. */
	return;

    /* Calculate indices of bucket to split */
    oldsegment = table->split / HSEGSIZE;
    oldindex = table->split % HSEGSIZE;

    newbucket = table->max + table->split;

    /* And the indices of the new bucket */
    newsegment = newbucket / HSEGSIZE;
    newindex = newbucket % HSEGSIZE;

    if (newindex == 0)
	allocSegment(table, newsegment);

    if (++table->split == table->max) {
	table->split = 0;
	table->max *= 2;
	table->mask1 = table->mask2;
	table->mask2 = table->mask2 << 1 | 1;
    }
    table->bcount++;

    /* Split the bucket, paying no attention to the original order */

    old = new = NULL;
    for (hl = table->dir[oldsegment][oldindex]; hl != NULL; hl = next) {
	next = hl->next;
	if (hash(table, hl->key) == newbucket) {
	    hl->next = new;
	    new = hl;
	} else {
	    hl->next = old;
	    old = hl;
	}
    }
    table->dir[oldsegment][oldindex] = old;
    table->dir[newsegment][newindex] = new;

    return;
}

void *
lookupHashTable(HashTable *table, StgWord key)
{
    int bucket;
    int segment;
    int index;
    HashList *hl;

    bucket = hash(table, key);
    segment = bucket / HSEGSIZE;
    index = bucket % HSEGSIZE;

    for (hl = table->dir[segment][index]; hl != NULL; hl = hl->next)
	if (hl->key == key)
	    return hl->data;

    /* It's not there */
    return NULL;
}

/* -----------------------------------------------------------------------------
 * We allocate the hashlist cells in large chunks to cut down on malloc
 * overhead.  Although we keep a free list of hashlist cells, we make
 * no effort to actually return the space to the malloc arena.
 * -------------------------------------------------------------------------- */

static HashList *freeList = NULL;

static HashList *
allocHashList(void)
{
    HashList *hl, *p;

    if ((hl = freeList) != NULL) {
	freeList = hl->next;
    } else {
        hl = stgMallocBytes(HCHUNK * sizeof(HashList), "allocHashList");

	freeList = hl + 1;
	for (p = freeList; p < hl + HCHUNK - 1; p++)
	    p->next = p + 1;
	p->next = NULL;
    }
    return hl;
}

static void
freeHashList(HashList *hl)
{
    hl->next = freeList;
    freeList = hl;
}

void
insertHashTable(HashTable *table, StgWord key, void *data)
{
    int bucket;
    int segment;
    int index;
    HashList *hl;

    /* We want no duplicates */
    ASSERT(lookupHashTable(table, key) == NULL);
    
    /* When the average load gets too high, we expand the table */
    if (++table->kcount >= HLOAD * table->bcount)
	expand(table);

    bucket = hash(table, key);
    segment = bucket / HSEGSIZE;
    index = bucket % HSEGSIZE;

    hl = allocHashList();

    hl->key = key;
    hl->data = data;
    hl->next = table->dir[segment][index];
    table->dir[segment][index] = hl;

}

void *
removeHashTable(HashTable *table, StgWord key, void *data)
{
    int bucket;
    int segment;
    int index;
    HashList *hl;
    HashList *prev = NULL;

    bucket = hash(table, key);
    segment = bucket / HSEGSIZE;
    index = bucket % HSEGSIZE;

    for (hl = table->dir[segment][index]; hl != NULL; hl = hl->next) {
	if (hl->key == key && (data == NULL || hl->data == data)) {
	    if (prev == NULL)
		table->dir[segment][index] = hl->next;
	    else
		prev->next = hl->next;
	    table->kcount--;
	    return hl->data;
	}
	prev = hl;
    }

    /* It's not there */
    ASSERT(data == NULL);
    return NULL;
}

/* -----------------------------------------------------------------------------
 * When we free a hash table, we are also good enough to free the
 * data part of each (key, data) pair, as long as our caller can tell
 * us how to do it.
 * -------------------------------------------------------------------------- */

void
freeHashTable(HashTable *table, void (*freeDataFun)(void *) )
{
    long segment;
    long index;
    HashList *hl;
    HashList *next;

    /* The last bucket with something in it is table->max + table->split - 1 */
    segment = (table->max + table->split - 1) / HSEGSIZE;
    index = (table->max + table->split - 1) % HSEGSIZE;

    while (segment >= 0) {
	while (index >= 0) {
	    for (hl = table->dir[segment][index]; hl != NULL; hl = next) {
		next = hl->next;
		if (freeDataFun != NULL)
		    (*freeDataFun)(hl->data);
		freeHashList(hl);
	    }
	    index--;
	}
	free(table->dir[segment]);
	segment--;
	index = HSEGSIZE - 1;
    }
    free(table);
}

/* -----------------------------------------------------------------------------
 * When we initialize a hash table, we set up the first segment as well,
 * initializing all of the first segment's hash buckets to NULL.
 * -------------------------------------------------------------------------- */

HashTable *
allocHashTable(void)
{
    HashTable *table;
    HashList **hb;

    table = stgMallocBytes(sizeof(HashTable),"allocHashTable");

    allocSegment(table, 0);

    for (hb = table->dir[0]; hb < table->dir[0] + HSEGSIZE; hb++)
	*hb = NULL;

    table->split = 0;
    table->max = HSEGSIZE;
    table->mask1 = HSEGSIZE - 1;
    table->mask2 = 2 * HSEGSIZE - 1;
    table->kcount = 0;
    table->bcount = HSEGSIZE;

    return table;
}
