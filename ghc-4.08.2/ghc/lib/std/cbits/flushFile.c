/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: flushFile.c,v 1.7.2.1 2000/09/25 12:25:35 simonmar Exp $
 *
 * hFlush Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
flushFile(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc = 0;

    if ( FILEOBJ_WRITEABLE(fo) && FILEOBJ_JUST_WRITTEN(fo) &&
	 FILEOBJ_NEEDS_FLUSHING(fo) ) {
       rc = writeBuffer(ptr, fo->bufWPtr - fo->bufRPtr);
    }

    return rc;
}

StgInt
flushBuffer(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc = 0;

    if ( FILEOBJ_WRITEABLE(fo) && FILEOBJ_JUST_WRITTEN(fo) &&
	 FILEOBJ_NEEDS_FLUSHING(fo) ) {
	rc = writeBuffer(ptr, fo->bufWPtr - fo->bufRPtr);
	if (rc<0) return rc;
    }
    
    /* TODO: shouldn't we do the lseek stuff from flushReadBuffer
     * here???? --SDM
     */

    /* Reset read & write pointer for input buffers */
    if ( (fo->flags & FILEOBJ_READ) ) {
       fo->bufRPtr=0;
       fo->bufWPtr=0;
    }
    return 0;
}

/*
 For RW file objects, flushing input buffers doesn't just involve 
 resetting the read & write pointers, we also have to change the
 underlying file position to point to the effective read position.

 (Sigh, I now understand the real reason for why stdio opted for
 the solution of leaving this to the programmer!)
*/
StgInt
flushReadBuffer(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int delta;

    delta = fo->bufWPtr - fo->bufRPtr;

    if ( delta > 0 ) {
       while ( lseek(fo->fd, -delta, SEEK_CUR) == -1) {
	  if (errno != EINTR) {
	     cvtErrno();
	     stdErrno();
	     return -1;
	  }
       }
    }

    fo->bufRPtr=0;
    fo->bufWPtr=0;
    return 0;
}

void
flushConnectedBuf(StgForeignPtr ptr)
{
    StgInt rc;
    IOFileObject* fo = (IOFileObject*)ptr;
    
    /* if the stream is connected to an output stream, flush it. */
    if ( fo->connectedTo != NULL && fo->connectedTo->fd != -1 &&
         (fo->connectedTo->flags & FILEOBJ_WRITE) ) {
       rc = flushBuffer((StgForeignPtr)fo->connectedTo);
    }
    /* Willfully ignore the return code for now. */
    return;
}

  
