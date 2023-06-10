/* There's currently no #define that indicate whether we're
   compiling a .hc file. */
#ifdef __GHC__
extern void CreateBMPFile(PSTR pszFileName, HBITMAP hBmp, HDC hDC);
#else
#include "dumpBMP.c"
#endif
