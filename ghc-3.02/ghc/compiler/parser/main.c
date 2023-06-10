/* This is the "top-level" file for the *standalone* hsp parser.
   See also hsclink.c.  (WDP 94/10)
*/

#include <stdio.h>

#include "hspincl.h"
#include "constants.h"
#include "utils.h"

/*OLD:static char *progname;*/		/* The name of the program.              */


/**********************************************************************
*                                                                     *
*                                                                     *
*     The main program                                                *
*                                                                     *
*                                                                     *
**********************************************************************/

int
main(int argc, char **argv)
{
    Lnil = mklnil();	/* The null list -- used in lsing, etc. */

    argv++; argc--;
    process_args(argc,argv);

    hash_init();
    yyinit();

    if(yyparse() == 0 && !etags)
      {
	/* No syntax errors. */
	pprogram(root);
	printf("\n");
	exit(0);
      } 
    else if(etags)
      {
	exit(0);
      }
    else
      {
	/* There was a syntax error. */
	printf("\n");
	exit(1);
      }
}
