/* -----------------------------------------------------------------------------
 * $Id: stgerror.h,v 1.1 1999/11/26 16:25:56 simonmar Exp $
 *
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1999
 *
 * Error codes used by the IO subsystem.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGERROR_H
#define STGERROR_H

#define ERR_ALREADYEXISTS 1
#define ERR_HARDWAREFAULT 2
#define ERR_ILLEGALOPERATION 3
#define ERR_INAPPROPRIATETYPE 4
#define ERR_INTERRUPTED 5
#define ERR_INVALIDARGUMENT 6
#define ERR_NOSUCHTHING 7
#define ERR_OTHERERROR 8
#define ERR_PERMISSIONDENIED 9
#define ERR_PROTOCOLERROR 10
#define ERR_RESOURCEBUSY 11
#define ERR_RESOURCEEXHAUSTED 12
#define ERR_RESOURCEVANISHED 13
#define ERR_SYSTEMERROR 14
#define ERR_TIMEEXPIRED 15
#define ERR_UNSATISFIEDCONSTRAINTS 16
#define ERR_UNSUPPORTEDOPERATION 17
#define ERR_USERERROR 18
#define ERR_EOF 19

#define GHC_E2BIG -1
#define GHC_EACCES -2
#define GHC_EADDRINUSE -3
#define GHC_EADDRNOTAVAIL -4
#define GHC_EADV -5
#define GHC_EAFNOSUPPORT -6
#define GHC_EAGAIN -7
#define GHC_EAIO -8
#define GHC_EALREADY -9
#define GHC_EBADF -10
#define GHC_EBADMSG -11
#define GHC_EBADRPC -12
#define GHC_EBUSY -13
#define GHC_ECANCELED -14
#define GHC_ECHILD -15
#define GHC_ECLONEME -16
#define GHC_ECOMM -17
#define GHC_ECONNABORTED -18
#define GHC_ECONNREFUSED -19
#define GHC_ECONNRESET -20
#define GHC_EDEADLK -21
#define GHC_EDESTADDRREQ -22
#define GHC_EDIRTY -23
#define GHC_EDOM -24
#define GHC_EDOTDOT -25
#define GHC_EDQUOT -26
#define GHC_EDUPPKG -27
#define GHC_EEXIST -28
#define GHC_EFAIL -29
#define GHC_EFAULT -30
#define GHC_EFBIG -31
#define GHC_EFTYPE -32
#define GHC_EHOSTDOWN -33
#define GHC_EHOSTUNREACH -34
#define GHC_EIDRM -35
#define GHC_EILSEQ -36
#define GHC_EINPROG -37
#define GHC_EINPROGRESS -38
#define GHC_EINTR -39
#define GHC_EINVAL -40
#define GHC_EIO -41
#define GHC_EISCONN -42
#define GHC_EISDIR -43
#define GHC_ELOOP -44
#define GHC_EMEDIA -45
#define GHC_EMFILE -46
#define GHC_EMLINK -47
#define GHC_EMSGSIZE -48
#define GHC_EMTIMERS -49
#define GHC_EMULTIHOP -50
#define GHC_ENAMETOOLONG -51
#define GHC_ENETDOWN -52
#define GHC_ENETRESET -53
#define GHC_ENETUNREACH -54
#define GHC_ENFILE -55
#define GHC_ENOBUFS -56
#define GHC_ENODATA -57
#define GHC_ENODEV -58
#define GHC_ENOENT -59
#define GHC_ENOEXEC -60
#define GHC_ENOLCK -61
#define GHC_ENOLINK -62
#define GHC_ENOMEM -63
#define GHC_ENOMSG -64
#define GHC_ENONET -65
#define GHC_ENOPKG -66
#define GHC_ENOPROTOOPT -67
#define GHC_ENOSPC -68
#define GHC_ENOSR -69
#define GHC_ENOSTR -70
#define GHC_ENOSYM -71
#define GHC_ENOSYS -72
#define GHC_ENOTBLK -73
#define GHC_ENOTCONN -74
#define GHC_ENOTDIR -75
#define GHC_ENOTEMPTY -76
#define GHC_ENOTSOCK -77
#define GHC_ENOTSUP -78
#define GHC_ENOTTY -79
#define GHC_ENXIO -80
#define GHC_EOPNOTSUPP -81
#define GHC_EPERM -82
#define GHC_EPFNOSUPPORT -83
#define GHC_EPIPE -84
#define GHC_EPROCLIM -85
#define GHC_EPROCUNAVAIL -86
#define GHC_EPROGMISMATCH -87
#define GHC_EPROGUNAVAIL -88
#define GHC_EPROTO -89
#define GHC_EPROTONOSUPPORT -90
#define GHC_EPROTOTYPE -91
#define GHC_ERANGE -92
#define GHC_ERELOCATED -93
#define GHC_EREMCHG -94
#define GHC_EREMOTE -95
#define GHC_EROFS -96
#define GHC_ERPCMISMATCH -97
#define GHC_ERREMOTE -98
#define GHC_ESHUTDOWN -99
#define GHC_ESOCKTNOSUPPORT -100
#define GHC_ESOFT -101
#define GHC_ESPIPE -102
#define GHC_ESRCH -103
#define GHC_ESRMNT -104
#define GHC_ESTALE -105
#define GHC_ETIME -106
#define GHC_ETIMEDOUT -107
#define GHC_ETOOMANYREFS -108
#define GHC_ETXTBSY -109
#define GHC_EUSERS -110
#define GHC_EVERSION -111
#define GHC_EWOULDBLOCK -112
#define GHC_EXDEV -113

#endif /* STGERROR_H */
