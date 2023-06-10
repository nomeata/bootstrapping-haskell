#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\subsection[connectSocket.lc]{Assign name to client socket}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
connectSocket(StgInt sockfd, StgAddr servaddr, 
	      StgInt addrlen, StgInt isUnixDomain)
{
    int rc;
    
    while ((rc = connect((int)sockfd, (struct sockaddr *)servaddr, 
			 (int)addrlen)) < 0) {
#if !defined(_WIN32) || defined(__CYGWIN__) || defined(__CYGWIN32__)
	if (errno == EINPROGRESS) {
	    errno = 0;
	    return FILEOBJ_BLOCKED_WRITE;
	
	}
#endif
	if (errno != EINTR) {
	    cvtConnectSocketErr(errno, isUnixDomain);
	    return -1;
	}
    }
    return 0;
}

/* Errors from a non-blocking connect() are discovered later via
 * getsockopt().  Hence we need a separate way to convert the error
 * code into something the I/O library understands.
 */
void
cvtConnectSocketErr(StgInt _ghc_errno, StgInt isUnixDomain)
{
    errno = _ghc_errno;
    cvtErrno();

    switch (ghc_errno) {
    default:
	stdErrno();
	break;
    case GHC_EACCES:
	ghc_errtype = ERR_PERMISSIONDENIED;
	if (isUnixDomain != 0)
	    ghc_errstr = "For a component of path prefix of path name";
	else
	    ghc_errstr = "Requested address protected, cannot bind socket";
	break;
    case GHC_EISCONN:
    case GHC_EADDRINUSE:
	ghc_errtype = ERR_RESOURCEBUSY;
	ghc_errstr  = "Address already in use";
	break;
    case GHC_EADDRNOTAVAIL:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr  = "Address not available from local machine";
	break;
    case GHC_EAFNOSUPPORT:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr  = "Address cannot be used with socket";
	break;
    case GHC_EALREADY:
	ghc_errtype = ERR_RESOURCEBUSY;
	ghc_errstr  = "Non-blocking socket, previous connection attempt not completed";
	break;
    case GHC_EBADF:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr  = "Not a valid socket file descriptor";
	break;
    case GHC_ECONNREFUSED:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr  = "Connection refused";
	break;
    case GHC_EFAULT:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr  = "Address not in valid part of process address space";
	break;
    case GHC_EINVAL:
	ghc_errtype = ERR_SYSTEMERROR;
	ghc_errstr  = "Specified size of structure not equal valid address for family";
	break;
    case GHC_ENETUNREACH:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr  = "Network not reachable from host";
	break;
    case GHC_ENOTSOCK:
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr  = "Descriptor for file, not a socket";
	break;
    case GHC_ETIMEDOUT:
	ghc_errtype = ERR_TIMEEXPIRED;
	ghc_errstr  = "Connection attempt timed out";
	break;
    case GHC_EIO:
	ghc_errtype = ERR_SYSTEMERROR;
	ghc_errstr  = "Could not make directory entry or alloc inode";
	break;
    case GHC_EISDIR:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr  = "A null path name was given";
	break;
    case GHC_ELOOP:
	ghc_errtype = ERR_SYSTEMERROR;
	ghc_errstr  = "Too many symbolic links encountered";
	break;
    case GHC_ENAMETOOLONG:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr  = "Max length of path name exceeded";
	break;
    case GHC_ENOENT:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr  = "Component in path prefix does not exist";
	break;
    case GHC_ENOTDIR:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr  = "Component in path prefix is not a directory";
	break;
    case GHC_EPROTOTYPE:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr  = "File referred to is a socket of differing type";
	break;
    }
}

