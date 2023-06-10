/*
 * Finalizers used throughout the Win32 library - 
 * on the ghc side we supply them in a separate .c,
 * for Hugs we inline a copy.
 */
#ifndef _FINALIZERS_H
#define _FINALIZERS_H

#ifdef __GHC__
extern void deleteObj(HANDLE h);
#else
static
void
deleteObj(HANDLE h)
{
  if (h == NULL)
    return;
  else {
    DeleteObject(h);
  }
}

#endif

#endif /* _FINALIZERS_H */
