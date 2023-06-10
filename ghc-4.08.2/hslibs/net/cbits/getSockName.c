#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[getSockName.lc]{Return name of process assoc with socket}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
getSockName(StgInt sockfd, StgAddr peer, StgAddr namelen)
{
    StgInt name;
    
    while ((name = getsockname((int) sockfd, (struct sockaddr *) peer, (int *) namelen)) < 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    switch (ghc_errno) {
	    default:
		stdErrno();
		break;
	    case GHC_EBADF:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Not a valid write descriptor";
		break;
	    case GHC_EFAULT:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Data not in writeable part of user address space";
		break;
	    case GHC_ENOBUFS:
		ghc_errtype = ERR_RESOURCEEXHAUSTED;
		ghc_errstr  = "Insuffcient resources";
		break;
	    case GHC_ENOTSOCK:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Descriptor is not a socket";
		break;
	    }
	    return -1;
	}
    }
    return name;
}
