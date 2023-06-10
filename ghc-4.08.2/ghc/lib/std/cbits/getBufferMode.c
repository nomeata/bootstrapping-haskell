/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getBufferMode.c,v 1.3 1998/12/02 13:27:35 simonm Exp $
 *
 * hIs...Buffered Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/*
 * We try to guess what the default buffer mode is going to be based 
 * on the type of file we're attached to.
 */

#define GBM_NB (0)
#define GBM_LB (-1)
#define GBM_BB (-2)
#define GBM_ERR (-3)

StgInt
getBufferMode(ptr)
StgForeignPtr ptr;
{
    IOFileObject* fo = (IOFileObject*)ptr;
    struct stat sb;
    int fd = fo->fd;

    /* Try to find out the file type */
    while (fstat(fd, &sb) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return GBM_ERR;
	}
    }
    /* Terminals are line-buffered by default */
    if (S_ISCHR(sb.st_mode) && isatty(fd) == 1) {
        fo ->flags |= FILEOBJ_LB;
	return GBM_LB;
    /* Default size block buffering for the others */
    } else {
        fo ->flags |= FILEOBJ_BB;
	return GBM_BB;
    }
}
