#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\subsection[createSocket.lc]{Create a socket file descriptor}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
createSocket(StgInt family, StgInt type, StgInt protocol)
{
    int fd;
    long flags;

    if ((fd = socket((int)family, (int)type, (int)protocol)) < 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    switch (ghc_errno) {
	    default:
		stdErrno();
		break;
	    case GHC_EACCES:
		ghc_errtype = ERR_PERMISSIONDENIED;
		ghc_errstr  = "cannot create socket";
		break;
	    case GHC_EMFILE:
		ghc_errtype = ERR_RESOURCEEXHAUSTED;
		ghc_errstr  = "Too many open files";
		break;
	    case GHC_ENFILE:
		ghc_errtype = ERR_RESOURCEEXHAUSTED;
		ghc_errstr  = "System file table overflow";
		break;
	    case GHC_EPROTONOSUPPORT:
		ghc_errtype = ERR_UNSUPPORTEDOPERATION;
		ghc_errstr  = "Protocol type not supported";
		break;
	    case GHC_EPROTOTYPE:
		ghc_errtype = ERR_INAPPROPRIATETYPE;
		ghc_errstr  = "Protocol wrong type for socket";
		break;
	    }
	    return (StgInt)-1;
	}
    }

    /* set the non-blocking flag on this file descriptor */
#if !defined(_WIN32) || defined(__CYGWIN__) || defined(__CYGWIN32__)
    flags = fcntl(fd, F_GETFL);
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);
#endif

    return (StgInt)fd;
}
