/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: filePutc.c,v 1.11 1999/12/08 15:47:07 simonmar Exp $
 *
 * hPutChar Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__) && !defined(__CYGWIN32__)
#define USE_WINSOCK
#endif

#ifdef USE_WINSOCK
#include <winsock.h>
#endif

#define TERMINATE_LINE(x)   ((x) == '\n')

StgInt
filePutc(StgForeignPtr ptr, StgChar c)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc = 0;

    /* What filePutc needs to do:

         - if there's no buffering => write it out.
	 - if the buffer is line-buffered
	        write out buffer (+char), iff buffer would be full afterwards ||
					      new char is the newline character
		add to buffer , otherwise
         - if the buffer is fully-buffered
	       write out buffer (+char), iff adding char fills up buffer.
	       add char to buffer, otherwise.

     In the cases where a file is buffered, the invariant is that operations
     that fill up a buffer also flushes them. A consequence of this here, is 
     that we're guaranteed to be passed a buffer with space for (at least)
     the one char we're adding.

     Supporting RW objects adds yet another twist, since we have to make
     sure that if such objects have been read from just previously, we
     flush(i.e., empty) the buffer first. (We could be smarter about this,
     but aren't!)

    */

    if ( FILEOBJ_READABLE(fo) && FILEOBJ_JUST_READ(fo) ) {
        rc = flushReadBuffer(ptr);
        if (rc<0) return rc;
    }

    fo->flags = (fo->flags & ~FILEOBJ_RW_READ) | FILEOBJ_RW_WRITE;
	      
    /* check whether we can just add it to the buffer.. */
    if ( FILEOBJ_UNBUFFERED(fo) ) {
        ; 
    } else {
	/* We're buffered, add it to the pack */
       ((char*)fo->buf)[fo->bufWPtr] = (char)c;
       fo->bufWPtr++;
      /* If the buffer filled up as a result, *or*
         the added character terminated a line
            => flush.
      */
      if ( FILEOBJ_BUFFER_FULL(fo) || 
           (FILEOBJ_LINEBUFFERED(fo) && TERMINATE_LINE(c)) ) {
        rc = writeBuffer(ptr, fo->bufWPtr);
	/* Undo the write if we're blocking..*/
	if (rc == FILEOBJ_BLOCKED_WRITE ) fo->bufWPtr--;
      }
      return rc;
    }

    /* Unbuffered, write the character directly. */
    while ((rc = (
#ifdef USE_WINSOCK
	         fo->flags & FILEOBJ_WINSOCK ?
		 send(fo->fd, &c, 1, 0) :
		 write(fo->fd, &c, 1))) <= 0) {
#else
  	         write(fo->fd, &c, 1))) <= 0) {
#endif

        if ( rc == -1 && errno == EAGAIN) {
	    errno = 0;
	    return FILEOBJ_BLOCKED_WRITE;
	} else if (rc == 0 || (rc == -1 && errno != EINTR)) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }

    return 0;
}
