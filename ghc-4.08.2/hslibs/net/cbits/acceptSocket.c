#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[acceptSocket.lc]{Server wait for client to connect}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
acceptSocket(StgInt sockfd, StgAddr peer, StgAddr addrlen)
{
    StgInt fd;
    long flags;

    while ((fd = accept((int)sockfd, (struct sockaddr *)peer, 
			(int *)addrlen)) < 0) {
	if (errno == EAGAIN) {
	    errno = 0;
	    return FILEOBJ_BLOCKED_READ;

	} else if (errno != EINTR) {
	    cvtErrno();
	    switch (ghc_errno) {
	    default:
		stdErrno();
		break;
	    case GHC_EBADF:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Not a valid descriptor";
		break;
	    case GHC_EFAULT:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Address not in writeable part of user address space";
		break;
	    case GHC_ENOTSOCK:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Descriptor not a socket";
		break;
	    case GHC_EOPNOTSUPP:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Socket not of type that supports listen";
		break;
	    case GHC_EWOULDBLOCK:
		ghc_errtype = ERR_OTHERERROR;
		ghc_errstr  = "No sockets are present to be accepted";
		break;
	    }
	    return -1;
	}
    }

    /* set the non-blocking flag on this file descriptor */
#if !defined(_WIN32) || defined(__CYGWIN__) || defined(__CYGWIN32__)
    flags = fcntl(fd, F_GETFL);
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);
#endif

    return fd;
}
