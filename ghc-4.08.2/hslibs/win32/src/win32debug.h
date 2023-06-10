#ifndef __WIN32_LIB_DEBUG_H
/* prefix WIN32_LIB to give it a better chance of being unique */
#define __WIN32_LIB_DEBUG_H

#if  defined(__GHC__) && defined(WIN32_LIB_DEBUG)
extern char* __current_fun__;
#endif

#endif /* __WIN32_LIB_DEBUG_H */
