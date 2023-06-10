/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getDirectoryContents.c,v 1.3 1998/12/02 13:27:40 simonm Exp $
 *
 * getDirectoryContents Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifndef LINK_MAX
#define LINK_MAX 1024
#endif

/* For cleanup of partial answer on error */

static void
freeEntries(char **entries, int count)
{
    int i;

    for (i = 0; i < count; i++)
	free(entries[i]);
    free(entries);
}

/* 
 * Our caller expects a malloc'ed array of malloc'ed string pointers.
 * To ensure consistency when mixing this with other directory
 * operations, we collect the entire list in one atomic operation,
 * rather than reading the directory lazily.
 */
StgAddr
getDirectoryContents(path)
StgByteArray path;
{
    struct stat sb;
    DIR *dir;
    struct dirent *d;
    char **entries;
    int alloc, count, len;

    /* Check for an actual directory */
    while (stat(path, &sb) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return NULL;
	}
    }
    if (!S_ISDIR(sb.st_mode)) {
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "not a directory";
	return NULL;
    }

    alloc = LINK_MAX;
    if ((entries = (char **) malloc(alloc * sizeof(char *))) == NULL) {
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "not enough virtual memory";
	return NULL;
    }
    
    while ((dir = opendir(path)) == NULL) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    free(entries);
	    return NULL;
	}
    }

    count = 0;
    for (;;) {
        errno = 0;  /* unchanged by readdir on EOF */
	while ((d = readdir(dir)) == NULL) {
	    if (errno == 0) {
		entries[count] = NULL;
		(void) closedir(dir);
		return (StgAddr) entries;
	    } else if (errno != EINTR) {
	        cvtErrno();
	        stdErrno();
		freeEntries(entries, count);
		(void) closedir(dir);
		return NULL;
	    }
	    errno = 0;
	}
        len = strlen(d->d_name);
	if ((entries[count] = malloc(len+1)) == NULL) {
	    ghc_errtype = ERR_RESOURCEEXHAUSTED;
	    ghc_errstr = "not enough virtual memory";
	    freeEntries(entries, count);
	    (void) closedir(dir);
	    return NULL;
	}
	strcpy(entries[count], d->d_name);
	/* Terminate the sucker */
	*(entries[count] + len) = 0;
	if (++count == alloc) {
	    alloc += LINK_MAX;
	    if ((entries = (char **) realloc(entries, alloc * sizeof(char *))) == NULL) {
		ghc_errtype = ERR_RESOURCEEXHAUSTED;
		ghc_errstr = "not enough virtual memory";
		freeEntries(entries, count);
		(void) closedir(dir);
		return NULL;
	    }
	}
    }
}
