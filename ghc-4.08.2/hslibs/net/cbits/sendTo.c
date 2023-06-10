/* -----------------------------------------------------------------------------
 * $Id: sendTo.c,v 1.2 2000/04/03 11:35:14 simonmar Exp $
 *
 * sendTo run-time support
 *
 * (c) The GHC Team 1998
 * -------------------------------------------------------------------------- */

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
sendTo__(StgInt fd, StgAddr buf, StgInt nbytes, StgAddr to, StgInt sz)
{
    StgInt count;
    int flags = 0;

    while ( (count = sendto((int)fd, (void*)buf, (int)nbytes, 
			    flags, (struct sockaddr*)to, sz)) < 0) {
#if !defined(_WIN32) || defined(__CYGWIN__) || defined(__CYGWIN32__)
	if (errno == EAGAIN) {
	    errno = 0;
	    return FILEOBJ_BLOCKED_WRITE;
	}
#endif
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    return count;
}
