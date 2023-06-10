/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: errno.c,v 1.3 1998/12/02 13:27:20 simonm Exp $
 *
 * GHC Error Number Conversion
 */

#include "Rts.h"
#include "stgio.h"

int ghc_errno = 0;
int ghc_errtype = 0;

char *ghc_errstr = NULL;

StgAddr
getErrStr__()
{ return ((StgAddr)ghc_errstr); }

StgInt
getErrNo__()
{ return ((StgInt)ghc_errno); }

StgInt
getErrType__()
{ return ((StgInt)ghc_errtype); }


/* Collect all of the grotty #ifdef's in one place. */

void cvtErrno(void)
{
    switch(errno) {
#ifdef E2BIG
    case E2BIG:
	ghc_errno = GHC_E2BIG;
	break;
#endif
#ifdef EACCES
    case EACCES:
	ghc_errno = GHC_EACCES;
	break;
#endif
#ifdef EADDRINUSE
    case EADDRINUSE:
	ghc_errno = GHC_EADDRINUSE;
	break;
#endif
#ifdef EADDRNOTAVAIL
    case EADDRNOTAVAIL:
	ghc_errno = GHC_EADDRNOTAVAIL;
	break;
#endif
#ifdef EADV
    case EADV:
	ghc_errno = GHC_EADV;
	break;
#endif
#ifdef EAFNOSUPPORT
    case EAFNOSUPPORT:
	ghc_errno = GHC_EAFNOSUPPORT;
	break;
#endif
#ifdef EAGAIN
    case EAGAIN:
	ghc_errno = GHC_EAGAIN;
	break;
#endif
#ifdef EALREADY
    case EALREADY:
	ghc_errno = GHC_EALREADY;
	break;
#endif
#ifdef EBADF
    case EBADF:
	ghc_errno = GHC_EBADF;
	break;
#endif
#ifdef EBADMSG
    case EBADMSG:
	ghc_errno = GHC_EBADMSG;
	break;
#endif
#ifdef EBADRPC
    case EBADRPC:
	ghc_errno = GHC_EBADRPC;
	break;
#endif
#ifdef EBUSY
    case EBUSY:
	ghc_errno = GHC_EBUSY;
	break;
#endif
#ifdef ECHILD
    case ECHILD:
	ghc_errno = GHC_ECHILD;
	break;
#endif
#ifdef ECOMM
    case ECOMM:
	ghc_errno = GHC_ECOMM;
	break;
#endif
#ifdef ECONNABORTED
    case ECONNABORTED:
	ghc_errno = GHC_ECONNABORTED;
	break;
#endif
#ifdef ECONNREFUSED
    case ECONNREFUSED:
	ghc_errno = GHC_ECONNREFUSED;
	break;
#endif
#ifdef ECONNRESET
    case ECONNRESET:
	ghc_errno = GHC_ECONNRESET;
	break;
#endif
#ifdef EDEADLK
    case EDEADLK:
	ghc_errno = GHC_EDEADLK;
	break;
#endif
#ifdef EDESTADDRREQ
    case EDESTADDRREQ:
	ghc_errno = GHC_EDESTADDRREQ;
	break;
#endif
#ifdef EDIRTY
    case EDIRTY:
	ghc_errno = GHC_EDIRTY;
	break;
#endif
#ifdef EDOM
    case EDOM:
	ghc_errno = GHC_EDOM;
	break;
#endif
#ifdef EDQUOT
    case EDQUOT:
	ghc_errno = GHC_EDQUOT;
	break;
#endif
#ifdef EEXIST
    case EEXIST:
	ghc_errno = GHC_EEXIST;
	break;
#endif
#ifdef EFAULT
    case EFAULT:
	ghc_errno = GHC_EFAULT;
	break;
#endif
#ifdef EFBIG
    case EFBIG:
	ghc_errno = GHC_EFBIG;
	break;
#endif
#ifdef EFTYPE
    case EFTYPE:
	ghc_errno = GHC_EFTYPE;
	break;
#endif
#ifdef EHOSTDOWN
    case EHOSTDOWN:
	ghc_errno = GHC_EHOSTDOWN;
	break;
#endif
#ifdef EHOSTUNREACH
    case EHOSTUNREACH:
	ghc_errno = GHC_EHOSTUNREACH;
	break;
#endif
#ifdef EIDRM
    case EIDRM:
	ghc_errno = GHC_EIDRM;
	break;
#endif
#ifdef EILSEQ
    case EILSEQ:
	ghc_errno = GHC_EILSEQ;
	break;
#endif
#ifdef EINPROGRESS
    case EINPROGRESS:
	ghc_errno = GHC_EINPROGRESS;
	break;
#endif
#ifdef EINTR
    case EINTR:
	ghc_errno = GHC_EINTR;
	break;
#endif
#ifdef EINVAL
    case EINVAL:
	ghc_errno = GHC_EINVAL;
	break;
#endif
#ifdef EIO
    case EIO:
	ghc_errno = GHC_EIO;
	break;
#endif
#ifdef EISCONN
    case EISCONN:
	ghc_errno = GHC_EISCONN;
	break;
#endif
#ifdef EISDIR
    case EISDIR:
	ghc_errno = GHC_EISDIR;
	break;
#endif
#ifdef ELOOP
    case ELOOP:
	ghc_errno = GHC_ELOOP;
	break;
#endif
#ifdef EMFILE
    case EMFILE:
	ghc_errno = GHC_EMFILE;
	break;
#endif
#ifdef EMLINK
    case EMLINK:
	ghc_errno = GHC_EMLINK;
	break;
#endif
#ifdef EMSGSIZE
    case EMSGSIZE:
	ghc_errno = GHC_EMSGSIZE;
	break;
#endif
#ifdef EMULTIHOP
    case EMULTIHOP:
	ghc_errno = GHC_EMULTIHOP;
	break;
#endif
#ifdef ENAMETOOLONG
    case ENAMETOOLONG:
	ghc_errno = GHC_ENAMETOOLONG;
	break;
#endif
#ifdef ENETDOWN
    case ENETDOWN:
	ghc_errno = GHC_ENETDOWN;
	break;
#endif
#ifdef ENETRESET
    case ENETRESET:
	ghc_errno = GHC_ENETRESET;
	break;
#endif
#ifdef ENETUNREACH
    case ENETUNREACH:
	ghc_errno = GHC_ENETUNREACH;
	break;
#endif
#ifdef ENFILE
    case ENFILE:
	ghc_errno = GHC_ENFILE;
	break;
#endif
#ifdef ENOBUFS
    case ENOBUFS:
	ghc_errno = GHC_ENOBUFS;
	break;
#endif
#ifdef ENODATA
    case ENODATA:
	ghc_errno = GHC_ENODATA;
	break;
#endif
#ifdef ENODEV
    case ENODEV:
	ghc_errno = GHC_ENODEV;
	break;
#endif
#ifdef ENOENT
    case ENOENT:
	ghc_errno = GHC_ENOENT;
	break;
#endif
#ifdef ENOEXEC
    case ENOEXEC:
	ghc_errno = GHC_ENOEXEC;
	break;
#endif
#ifdef ENOLCK
    case ENOLCK:
	ghc_errno = GHC_ENOLCK;
	break;
#endif
#ifdef ENOLINK
    case ENOLINK:
	ghc_errno = GHC_ENOLINK;
	break;
#endif
#ifdef ENOMEM
    case ENOMEM:
	ghc_errno = GHC_ENOMEM;
	break;
#endif
#ifdef ENOMSG
    case ENOMSG:
	ghc_errno = GHC_ENOMSG;
	break;
#endif
#ifdef ENONET
    case ENONET:
	ghc_errno = GHC_ENONET;
	break;
#endif
#ifdef ENOPROTOOPT
    case ENOPROTOOPT:
	ghc_errno = GHC_ENOPROTOOPT;
	break;
#endif
#ifdef ENOSPC
    case ENOSPC:
	ghc_errno = GHC_ENOSPC;
	break;
#endif
#ifdef ENOSR
    case ENOSR:
	ghc_errno = GHC_ENOSR;
	break;
#endif
#ifdef ENOSTR
    case ENOSTR:
	ghc_errno = GHC_ENOSTR;
	break;
#endif
#ifdef ENOSYS
    case ENOSYS:
	ghc_errno = GHC_ENOSYS;
	break;
#endif
#ifdef ENOTBLK
    case ENOTBLK:
	ghc_errno = GHC_ENOTBLK;
	break;
#endif
#ifdef ENOTCONN
    case ENOTCONN:
	ghc_errno = GHC_ENOTCONN;
	break;
#endif
#ifdef ENOTDIR
    case ENOTDIR:
	ghc_errno = GHC_ENOTDIR;
	break;
#endif
#ifndef aix_TARGET_OS
/* AIX returns EEXIST where 4.3BSD used ENOTEMPTY.
 * there is an ENOTEMPTY defined as the same as EEXIST, and
 * therefore it won't work properly on a case statement.
 * another option is to define _ALL_SOURCE for aix, which
 * gives a different number for ENOTEMPTY.
 * I haven't tried that. -- andre.
 */
#ifdef ENOTEMPTY
    case ENOTEMPTY:
	ghc_errno = GHC_ENOTEMPTY;
	break;
#endif
#endif
#ifdef ENOTSOCK
    case ENOTSOCK:
	ghc_errno = GHC_ENOTSOCK;
	break;
#endif
#ifdef ENOTTY
    case ENOTTY:
	ghc_errno = GHC_ENOTTY;
	break;
#endif
#ifdef ENXIO
    case ENXIO:
	ghc_errno = GHC_ENXIO;
	break;
#endif
#ifdef EOPNOTSUPP
    case EOPNOTSUPP:
	ghc_errno = GHC_EOPNOTSUPP;
	break;
#endif
#ifdef EPERM
    case EPERM:
	ghc_errno = GHC_EPERM;
	break;
#endif
#ifdef EPFNOSUPPORT
    case EPFNOSUPPORT:
	ghc_errno = GHC_EPFNOSUPPORT;
	break;
#endif
#ifdef EPIPE
    case EPIPE:
	ghc_errno = GHC_EPIPE;
	break;
#endif
#ifdef EPROCLIM
    case EPROCLIM:
	ghc_errno = GHC_EPROCLIM;
	break;
#endif
#ifdef EPROCUNAVAIL
    case EPROCUNAVAIL:
	ghc_errno = GHC_EPROCUNAVAIL;
	break;
#endif
#ifdef EPROGMISMATCH
    case EPROGMISMATCH:
	ghc_errno = GHC_EPROGMISMATCH;
	break;
#endif
#ifdef EPROGUNAVAIL
    case EPROGUNAVAIL:
	ghc_errno = GHC_EPROGUNAVAIL;
	break;
#endif
#ifdef EPROTO
    case EPROTO:
	ghc_errno = GHC_EPROTO;
	break;
#endif
#ifdef EPROTONOSUPPORT
    case EPROTONOSUPPORT:
	ghc_errno = GHC_EPROTONOSUPPORT;
	break;
#endif
#ifdef EPROTOTYPE
    case EPROTOTYPE:
	ghc_errno = GHC_EPROTOTYPE;
	break;
#endif
#ifdef ERANGE
    case ERANGE:
	ghc_errno = GHC_ERANGE;
	break;
#endif
#ifdef EREMCHG
    case EREMCHG:
	ghc_errno = GHC_EREMCHG;
	break;
#endif
#ifdef EREMOTE
    case EREMOTE:
	ghc_errno = GHC_EREMOTE;
	break;
#endif
#ifdef EROFS
    case EROFS:
	ghc_errno = GHC_EROFS;
	break;
#endif
#ifdef ERPCMISMATCH
    case ERPCMISMATCH:
	ghc_errno = GHC_ERPCMISMATCH;
	break;
#endif
#ifdef ERREMOTE
    case ERREMOTE:
	ghc_errno = GHC_ERREMOTE;
	break;
#endif
#ifdef ESHUTDOWN
    case ESHUTDOWN:
	ghc_errno = GHC_ESHUTDOWN;
	break;
#endif
#ifdef ESOCKTNOSUPPORT
    case ESOCKTNOSUPPORT:
	ghc_errno = GHC_ESOCKTNOSUPPORT;
	break;
#endif
#ifdef ESPIPE
    case ESPIPE:
	ghc_errno = GHC_ESPIPE;
	break;
#endif
#ifdef ESRCH
    case ESRCH:
	ghc_errno = GHC_ESRCH;
	break;
#endif
#ifdef ESRMNT
    case ESRMNT:
	ghc_errno = GHC_ESRMNT;
	break;
#endif
#ifdef ESTALE
    case ESTALE:
	ghc_errno = GHC_ESTALE;
	break;
#endif
#ifdef ETIME
    case ETIME:
	ghc_errno = GHC_ETIME;
	break;
#endif
#ifdef ETIMEDOUT
    case ETIMEDOUT:
	ghc_errno = GHC_ETIMEDOUT;
	break;
#endif
#ifdef ETOOMANYREFS
    case ETOOMANYREFS:
	ghc_errno = GHC_ETOOMANYREFS;
	break;
#endif
#ifdef ETXTBSY
    case ETXTBSY:
	ghc_errno = GHC_ETXTBSY;
	break;
#endif
#ifdef EUSERS
    case EUSERS:
	ghc_errno = GHC_EUSERS;
	break;
#endif
#if 0
#ifdef EWOULDBLOCK
    case EWOULDBLOCK:
	ghc_errno = GHC_EWOULDBLOCK;
	break;
#endif
#endif
#ifdef EXDEV
    case EXDEV:
	ghc_errno = GHC_EXDEV;
	break;
#endif
    default:
	ghc_errno = errno;
	break;
    }
}

void
stdErrno(void)
{
    switch(ghc_errno) {
    default:
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "unexpected error";
	break;
    case 0:
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "no error";
    case GHC_E2BIG:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "argument list too long";
	break;
    case GHC_EACCES:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr = "inadequate access permission";
	break;
    case GHC_EADDRINUSE:
	ghc_errtype = ERR_RESOURCEBUSY;
	ghc_errstr = "address already in use";
	break;
    case GHC_EADDRNOTAVAIL:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "address not available";
	break;
    case GHC_EADV:
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "RFS advertise error";
	break;
    case GHC_EAFNOSUPPORT:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "address family not supported by protocol family";
	break;
    case GHC_EAGAIN:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "insufficient resources";
	break;
    case GHC_EALREADY:
	ghc_errtype = ERR_ALREADYEXISTS;
	ghc_errstr = "operation already in progress";
	break;
    case GHC_EBADF:
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "internal error (EBADF)";
	break;
    case GHC_EBADMSG:
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "next message has wrong type";
	break;
    case GHC_EBADRPC:
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "invalid RPC request or response";
	break;
    case GHC_EBUSY:
	ghc_errtype = ERR_RESOURCEBUSY;
	ghc_errstr = "device busy";
	break;
    case GHC_ECHILD:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "no child processes";
	break;
    case GHC_ECOMM:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "no virtual circuit could be found";
	break;
    case GHC_ECONNABORTED:
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "aborted connection";
	break;
    case GHC_ECONNREFUSED:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "no listener on remote host";
	break;
    case GHC_ECONNRESET:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "connection reset by peer";
	break;
    case GHC_EDEADLK:
	ghc_errtype = ERR_RESOURCEBUSY;
	ghc_errstr = "resource deadlock avoided";
	break;
    case GHC_EDESTADDRREQ:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "destination address required";
	break;
    case GHC_EDIRTY:
	ghc_errtype = ERR_UNSATISFIEDCONSTRAINTS;
	ghc_errstr = "file system dirty";
	break;
    case GHC_EDOM:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "argument too large";
	break;
    case GHC_EDQUOT:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr = "quota exceeded";
	break;
    case GHC_EEXIST:
	ghc_errtype = ERR_ALREADYEXISTS;
	ghc_errstr = "file already exists";
	break;
    case GHC_EFAULT:
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "internal error (EFAULT)";
	break;
    case GHC_EFBIG:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr = "file too large";
	break;
    case GHC_EFTYPE:
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "inappropriate NFS file type or format";
	break;
    case GHC_EHOSTDOWN:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "destination host down";
	break;
    case GHC_EHOSTUNREACH:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "remote host is unreachable";
	break;
    case GHC_EIDRM:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "IPC identifier removed";
	break;
    case GHC_EILSEQ:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "invalid wide character";
	break;
    case GHC_EINPROGRESS:
	ghc_errtype = ERR_ALREADYEXISTS;
	ghc_errstr = "operation now in progress";
	break;
    case GHC_EINTR:
	ghc_errtype = ERR_INTERRUPTED;
	ghc_errstr = "interrupted system call";
	break;
    case GHC_EINVAL:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "invalid argument";
	break;
    case GHC_EIO:
	ghc_errtype = ERR_HARDWAREFAULT;
	ghc_errstr = "unknown I/O fault";
	break;
    case GHC_EISCONN:
	ghc_errtype = ERR_ALREADYEXISTS;
	ghc_errstr = "socket is already connected";
	break;
    case GHC_EISDIR:
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "file is a directory";
	break;
    case GHC_ELOOP:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "too many symbolic links";
	break;
    case GHC_EMFILE:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "process file table full";
	break;
    case GHC_EMLINK:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "too many links";
	break;
    case GHC_EMSGSIZE:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "message too long";
	break;
    case GHC_EMULTIHOP:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "multi-hop RFS request";
	break;
    case GHC_ENAMETOOLONG:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "filename too long";
	break;
    case GHC_ENETDOWN:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "network is down";
	break;
    case GHC_ENETRESET:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "remote host rebooted; connection lost";
	break;
    case GHC_ENETUNREACH:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "remote network is unreachable";
	break;
    case GHC_ENFILE:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "system file table full";
	break;
    case GHC_ENOBUFS:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "no buffer space available";
	break;
    case GHC_ENODATA:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "no message on the stream head read queue";
	break;
    case GHC_ENODEV:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "no such device";
	break;
    case GHC_ENOENT:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "no such file or directory";
	break;
    case GHC_ENOEXEC:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "not an executable file";
	break;
    case GHC_ENOLCK:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "no file locks available";
	break;
    case GHC_ENOLINK:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "RFS link has been severed";
	break;
    case GHC_ENOMEM:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "not enough virtual memory";
	break;
    case GHC_ENOMSG:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "no message of desired type";
	break;
    case GHC_ENONET:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "host is not on a network";
	break;
    case GHC_ENOPROTOOPT:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "operation not supported by protocol";
	break;
    case GHC_ENOSPC:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "no space left on device";
	break;
    case GHC_ENOSR:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "out of stream resources";
	break;
    case GHC_ENOSTR:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "not a stream device";
	break;
    case GHC_ENOSYS:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "function not implemented";
	break;
    case GHC_ENOTBLK:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "not a block device";
	break;
    case GHC_ENOTCONN:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "socket is not connected";
	break;
    case GHC_ENOTDIR:
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "not a directory";
	break;
    case GHC_ENOTEMPTY:
	ghc_errtype = ERR_UNSATISFIEDCONSTRAINTS;
	ghc_errstr = "directory not empty";
	break;
    case GHC_ENOTSOCK:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "not a socket";
	break;
    case GHC_ENOTTY:
	ghc_errtype = ERR_ILLEGALOPERATION;
	ghc_errstr = "inappropriate ioctl for device";
	break;
    case GHC_ENXIO:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "no such device or address";
	break;
    case GHC_EOPNOTSUPP:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "operation not supported on socket";
	break;
    case GHC_EPERM:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr = "privileged operation";
	break;
    case GHC_EPFNOSUPPORT:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "protocol family not supported";
	break;
    case GHC_EPIPE:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "broken pipe";
	break;
    case GHC_EPROCLIM:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr = "too many processes";
	break;
    case GHC_EPROCUNAVAIL:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "unimplemented RPC procedure";
	break;
    case GHC_EPROGMISMATCH:
	ghc_errtype = ERR_PROTOCOLERROR;
	ghc_errstr = "unsupported RPC program version";
	break;
    case GHC_EPROGUNAVAIL:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "RPC program unavailable";
	break;
    case GHC_EPROTO:
	ghc_errtype = ERR_PROTOCOLERROR;
	ghc_errstr = "error in streams protocol";
	break;
    case GHC_EPROTONOSUPPORT:
	ghc_errtype = ERR_PROTOCOLERROR;
	ghc_errstr = "protocol not supported";
	break;
    case GHC_EPROTOTYPE:
	ghc_errtype = ERR_PROTOCOLERROR;
	ghc_errstr = "wrong protocol for socket";
	break;
    case GHC_ERANGE:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "result too large";
	break;
    case GHC_EREMCHG:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "remote address changed";
	break;
    case GHC_EREMOTE:
	ghc_errtype = ERR_ILLEGALOPERATION;
	ghc_errstr = "too many levels of remote in path";
	break;
    case GHC_EROFS:
	ghc_errtype = ERR_PERMISSIONDENIED;
	ghc_errstr = "read-only file system";
	break;
    case GHC_ERPCMISMATCH:
	ghc_errtype = ERR_PROTOCOLERROR;
	ghc_errstr = "RPC version is wrong";
	break;
    case GHC_ERREMOTE:
	ghc_errtype = ERR_ILLEGALOPERATION;
	ghc_errstr = "object is remote";
	break;
    case GHC_ESHUTDOWN:
	ghc_errtype = ERR_ILLEGALOPERATION;
	ghc_errstr = "can't send after socket shutdown";
	break;
    case GHC_ESOCKTNOSUPPORT:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "socket type not supported";
	break;
    case GHC_ESPIPE:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "can't seek on a pipe";
	break;
    case GHC_ESRCH:
	ghc_errtype = ERR_NOSUCHTHING;
	ghc_errstr = "no such process";
	break;
    case GHC_ESRMNT:
	ghc_errtype = ERR_UNSATISFIEDCONSTRAINTS;
	ghc_errstr = "RFS resources still mounted by remote host(s)";
	break;
    case GHC_ESTALE:
	ghc_errtype = ERR_RESOURCEVANISHED;
	ghc_errstr = "stale NFS file handle";
	break;
    case GHC_ETIME:
	ghc_errtype = ERR_TIMEEXPIRED;
	ghc_errstr = "timer expired";
	break;
    case GHC_ETIMEDOUT:
	ghc_errtype = ERR_TIMEEXPIRED;
	ghc_errstr = "connection timed out";
	break;
    case GHC_ETOOMANYREFS:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "too many references; can't splice";
	break;
    case GHC_ETXTBSY:
	ghc_errtype = ERR_RESOURCEBUSY;
	ghc_errstr = "text file in-use";
	break;
    case GHC_EUSERS:
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "quota table full";
	break;
    case GHC_EWOULDBLOCK:
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "operation would block";
	break;
    case GHC_EXDEV:
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "can't make a cross-device link";
	break;
    }
}

void
convertErrno(void)
{
 cvtErrno();
 stdErrno();
}
