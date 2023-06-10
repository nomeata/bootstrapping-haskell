/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: removeDirectory.c,v 1.3 1998/12/02 13:27:47 simonm Exp $
 *
 * removeDirectory Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

StgInt
removeDirectory(path)
StgByteArray path;
{
    struct stat sb;

    /* Check for an actual directory */
    while (stat(path, &sb) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    if (!S_ISDIR(sb.st_mode)) {
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "not a directory";
	return -1;
    }
    while (rmdir(path) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    switch (ghc_errno) {
	    default:
		stdErrno();
		break;
	    case GHC_ENOTEMPTY:
	    case GHC_EEXIST:
		ghc_errtype = ERR_UNSATISFIEDCONSTRAINTS;
		ghc_errstr = "directory not empty";
		break;
	    }		
	    return -1;
	}
    }
    return 0;
}
