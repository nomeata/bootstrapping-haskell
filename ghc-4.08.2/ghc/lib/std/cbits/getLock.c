/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getLock.c,v 1.7 1999/12/08 15:47:07 simonmar Exp $
 *
 * stdin/stout/stderr Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifndef FD_SETSIZE
#define FD_SETSIZE 256
#endif

typedef struct {
    dev_t device;
    ino_t inode;
    int fd;
} Lock;

static Lock readLock[FD_SETSIZE];
static Lock writeLock[FD_SETSIZE];

static int readLocks = 0;
static int writeLocks = 0;

int
lockFile(fd, for_writing, exclusive)
int fd;
int for_writing;
int exclusive;
{
    int i;
    struct stat sb;

    while (fstat(fd, &sb) < 0) {
	if (errno != EINTR) {
#ifndef _WIN32
	    return -1;
#else
	    /* fstat()ing socket fd's seems to fail with CRT's fstat(),
	       so let's just silently return and hope for the best..
	    */
	    return 0;
#endif
	}
    }

    /* Only lock regular files */
    if (!S_ISREG(sb.st_mode))
	return 0;
    
    if (for_writing) {
      /* opening a file for writing, check to see whether
         we don't have any read locks on it already.. */
      for (i = 0; i < readLocks; i++) {
	 if (readLock[i].inode == sb.st_ino && readLock[i].device == sb.st_dev) {
#ifndef __MINGW32__
	    errno = EAGAIN;
	    return -1;
#else
	    break;    
#endif
	 }	    
      }
      /* If we're determined that there is only a single
         writer to the file, check to see whether the file
	 hasn't already been opened for writing..
      */
      if (exclusive) {
	for (i = 0; i < writeLocks; i++) {
	  if (writeLock[i].inode == sb.st_ino && writeLock[i].device == sb.st_dev) {
#ifndef __MINGW32__
	     errno = EAGAIN;
	     return -1;
#else
	     break;
#endif
	  }
        }
      }
      /* OK, everything is cool lock-wise, record it and leave. */
      i = writeLocks++;
      writeLock[i].device = sb.st_dev;
      writeLock[i].inode = sb.st_ino;
      writeLock[i].fd = fd;
      return 0;
    } else { 
      /* For reading, it's simpler - just check to see
         that there's no-one writing to the underlying file. */
      for (i = 0; i < writeLocks; i++) {
	if (writeLock[i].inode == sb.st_ino && writeLock[i].device == sb.st_dev) {
#ifndef __MINGW32__
	     errno = EAGAIN;
	     return -1;
#else
	     break;
#endif
        }
      }
      /* Fit in new entry, reusing an existing table entry, if possible. */
      for (i = 0; i < readLocks; i++) {
	 if (readLock[i].inode == sb.st_ino && readLock[i].device == sb.st_dev) {
	   return 0;
	 }
      }
      i = readLocks++;
      readLock[i].device = sb.st_dev;
      readLock[i].inode = sb.st_ino;
      readLock[i].fd = fd;
      return 0;
    }

}

int
unlockFile(fd)
int fd;
{
    int i;

    for (i = 0; i < readLocks; i++)
	if (readLock[i].fd == fd) {
	    while (++i < readLocks)
		readLock[i - 1] = readLock[i];
	    readLocks--;
	    return 0;
	}

    for (i = 0; i < writeLocks; i++)
	if (writeLock[i].fd == fd) {
	    while (++i < writeLocks)
		writeLock[i - 1] = writeLock[i];
	    writeLocks--;
	    return 0;
	}
     /* Signal that we did not find an entry */
    return 1;
}

/* getLock() is used when opening the standard file descriptors */
StgInt
getLock(fd, for_writing)
StgInt fd;
StgInt for_writing;
{
    if (lockFile(fd, for_writing, 0) < 0) {
	if (errno == EBADF)
	    return 0;
	else {
	    cvtErrno();
	    switch (ghc_errno) {
	    default:
		stdErrno();
		break;
	    case GHC_EACCES:
	    case GHC_EAGAIN:
		ghc_errtype = ERR_RESOURCEBUSY;
		ghc_errstr = "file is locked";
		break;
	    }
	    /* Not so sure we want to do this, since getLock() 
	    is only called on the standard file descriptors.. */
	    /*(void) close(fd); */
	    return -1;
	}
    }
    return 1;
}
