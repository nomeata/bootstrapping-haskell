/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileGetc.c,v 1.6 2000/01/18 12:41:03 simonmar Exp $
 *
 * hGetChar Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#define EOT 4

/* Pre-condition: only ever called on a readable fileObject */
StgInt
fileGetc(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc=0;
    unsigned char c;
    
#if 0
    fprintf(stderr, "fgc: %d %d %d\n", fo->bufRPtr, fo->bufWPtr, fo->flags);
#endif
    /*
      fileGetc does the following:
	- if the input is buffered, try fetch the char from buffer.
	- failing that,
    
          - if the input stream is 'connected' to an output stream,
	    flush it before requesting any input.
	  - if unbuffered, read in one character.
	  - if line-buffered, read in one line, returning the first.
	  - if block-buffered, fill up block, returning the first.
    */

    if ( FILEOBJ_WRITEABLE(fo) && FILEOBJ_JUST_WRITTEN(fo) && FILEOBJ_NEEDS_FLUSHING(fo) ) {
        rc = flushBuffer(ptr);
	if (rc < 0) return rc;
    }

    fo->flags = (fo->flags & ~FILEOBJ_RW_WRITE) | FILEOBJ_RW_READ;

    if ( FILEOBJ_IS_EOF(fo) ) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
	return -1;
    }

    if ( FILEOBJ_BUFFER_EMPTY(fo) ) {
       ;
    } else if ( FILEOBJ_UNBUFFERED(fo) && !FILEOBJ_HAS_PUSHBACKS(fo) ) {
       ;
    } else if ( FILEOBJ_UNBUFFERED(fo) ) { /* Unbuffered stream has pushbacks, retrieve them */
          c=((unsigned char*)(fo->buf))[fo->bufRPtr++];
	  return (int)c;
    } else {
          c=((unsigned char*)(fo->buf))[fo->bufRPtr];
          fo->bufRPtr++;
	  return (int)c;
    }
    
    /* Nothing in the buffer, go out and fetch a byte for our customer,
       filling up the buffer if needs be.
    */
    if ( FILEOBJ_UNBUFFERED(fo) ) {
    	return (readChar(ptr));
    } else if ( FILEOBJ_LINEBUFFERED(fo) ) {

        /* if input stream is connect to an output stream, flush it first */
        if ( fo->connectedTo != NULL   &&
             fo->connectedTo->fd != -1 &&
            (fo->connectedTo->flags & FILEOBJ_WRITE)  ) {
           rc = flushFile((StgForeignPtr)fo->connectedTo);
        }
        if (rc < 0) return rc;

	rc = fill_up_line_buffer(fo);
	if (rc < 0) return rc;

        c=((unsigned char*)(fo->buf))[fo->bufRPtr];
        fo->bufRPtr++;
        return (int)c;

    } else { /* Fully-buffered */
        rc = readBlock(ptr);
	if (rc < 0) return rc;
  
        c=((unsigned char*)(fo->buf))[fo->bufRPtr];
        fo->bufRPtr++;
        return (int)c;
    }
}
