#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[writeDescriptor.lc]{Stuff bytes down a descriptor}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
writeDescriptor(StgInt fd, StgAddr buf, StgInt nbytes)
{
    StgInt dumped;
    
    while ((dumped = write((int) fd, (char *) buf, (int) nbytes)) < 0) {
#if !defined(_WIN32) || defined(__CYGWIN__) || defined(__CYGWIN32__)
	if (errno == EAGAIN) {
	    errno = 0;
	    return FILEOBJ_BLOCKED_WRITE;
	}
#endif
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
	    case GHC_EDQUOT:
		ghc_errtype = ERR_RESOURCEEXHAUSTED;
		ghc_errstr  = "Disk quota exhausted";
		break;
	    case GHC_EFAULT:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Data not in writeable part of user address space";
		break;
	    case GHC_EFBIG:
		ghc_errtype = ERR_RESOURCEEXHAUSTED;
		ghc_errstr  = "Maximum process or system file size exceeded";
		break;
	    case GHC_EINVAL:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Seek pointer associated with descriptor negative";
		break;
	    case GHC_EIO:
		ghc_errtype = ERR_SYSTEMERROR;
		ghc_errstr  = "I/O error occurred while writing to file system";
		break;
	    case GHC_ENOSPC:
		ghc_errtype = ERR_RESOURCEEXHAUSTED;
		ghc_errstr  = "No space left on device";
		break;
	    case GHC_ENXIO:
		ghc_errtype = ERR_SYSTEMERROR;
		ghc_errstr  = "Hangup occurred";
		break;
	    case GHC_EPIPE:
		ghc_errtype = ERR_SYSTEMERROR;
		ghc_errstr  = "Write to not read pipe/unconnected socket caught";
		break;
	    case GHC_ERANGE:
		ghc_errtype = ERR_INVALIDARGUMENT;
		ghc_errstr  = "Too much or too little written to descriptor";
		break;
	    case GHC_EAGAIN:
	    case GHC_EWOULDBLOCK:
		ghc_errtype = ERR_OTHERERROR;
		ghc_errstr  = "No data could be written immediately";
		break;
	    }
	    return -1;
	}
    }
    return dumped;
}
