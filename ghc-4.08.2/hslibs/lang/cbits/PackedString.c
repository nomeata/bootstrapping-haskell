/* -----------------------------------------------------------------------------
 * $Id: PackedString.c,v 1.1 1999/11/25 17:10:06 simonmar Exp $
 *
 * PackedString C bits
 *
 * (c) The GHC Team 1998
 * -------------------------------------------------------------------------- */

#include "Rts.h"

StgInt
byteArrayHasNUL__ (StgByteArray ba, StgInt len)
{
    StgInt i;

    for (i = 0; i < len; i++) {
	if (*(ba + i) == '\0') {
	    return(1); /* true */
	}
    }

    return(0); /* false */
}
