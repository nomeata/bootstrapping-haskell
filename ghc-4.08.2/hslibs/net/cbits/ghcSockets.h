#ifndef GHC_SOCKETS_H
#define GHC_SOCKETS_H

#include "stgio.h"

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__)
#include <winsock.h>
#else

#include <ctype.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <limits.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_TCP_H
# include <netinet/tcp.h>
#endif
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#include <sys/uio.h>

/* ToDo: featurise this */
#if  !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
#include <sys/un.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#endif /* !HAVE_WINSOCK_H */

/* acceptSocket.c */
StgInt	acceptSocket (StgInt, StgAddr, StgAddr);

/* bindSocket.c */
StgInt	bindSocket (StgInt, StgAddr, StgInt, StgInt);

/* connectSocket.c */
StgInt	connectSocket (StgInt, StgAddr, StgInt, StgInt);
void    cvtConnectSocketErr(StgInt, StgInt);

/* createSocket.c */
StgInt	createSocket (StgInt, StgInt, StgInt);

/* getSockName.c */
StgInt	getSockName (StgInt, StgAddr, StgAddr);

/* getPeerName.c */
StgInt	getPeerName (StgInt, StgAddr, StgAddr);

/* listenSocket.c */
StgInt	listenSocket (StgInt, StgInt);

/* shutdownSocket.c */
StgInt	shutdownSocket (StgInt, StgInt);

/* readDescriptor.c */
StgInt	readDescriptor (StgInt, StgAddr, StgInt);

/* recvFrom.c */
StgInt	recvFrom__ (StgInt, StgAddr, StgInt, StgAddr);

/* sendTo.c */
StgInt	sendTo__ (StgInt, StgAddr, StgInt, StgAddr, StgInt);

/* socketOpt.c */
StgInt	getSocketOption__ (StgInt, StgInt, StgInt);
StgInt	setSocketOption__ (StgInt, StgInt, StgInt, StgInt);

/* writeDescriptor.c */
StgInt	writeDescriptor (StgInt, StgAddr, StgInt);

/* initWinSock.c */
#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__)
StgInt  initWinSock();
void    shutdownWinSock();
#endif

#endif /* !GHC_SOCKETS_H */
