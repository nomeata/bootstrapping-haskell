#if 0
%---------------------------------------------------------------*
%
\section{Underlying code for converting to/from ``bytes''}
%
%---------------------------------------------------------------*

Stolen from HBC, more or less.

A \tr{I_ foo2bytes__(foo in, ptr arr)} routine takes a \tr{foo}
input \tr{in}, scribbles some appropriate bytes into the array passed
to it, \tr{arr}, and returns the number of bytes so put.

A \tr{I_ bytes2foo__(ptr arr, foo *out)} routine looks at the
array of bytes given to it (\tr{arr}) and gives them back interpreted
as a \tr{foo} (sticks it in the place pointed to by \tr{out}).  It
returns the number of bytes taken.

\begin{code}
#endif /* 0 */

#include "rtsdefs.h"
#include "ByteOps.h"

#if __STDC__
    /* need the ANSI arg decl, so "short" and "float" args dont get promoted */
#define X2BYTES(type)				\
I_						\
CAT2(type,2bytes__)(type in, unsigned char *arr)\
{ 						\
    union {					\
	type i;					\
	unsigned char cs[sizeof (type)];	\
    } u;					\
    int k;					\
						\
    u.i = in;					\
    for (k = sizeof (type) - 1; k >= 0; k--)	\
	arr[k] = u.cs[k];			\
						\
    return(sizeof (type));			\
}

#else /* not STDC */
#define X2BYTES(type)				\
I_						\
CAT2(type,2bytes__)(in, arr)			\
  type in;					\
  unsigned char *arr;				\
{ 						\
    union {					\
	type i;					\
	unsigned char cs[sizeof (type)];	\
    } u;					\
    int k;					\
						\
    u.i = in;					\
    for (k = sizeof (type) - 1; k >= 0; k--)	\
	arr[k] = u.cs[k];			\
						\
    return(sizeof (type));			\
}
#endif /* not STDC */

X2BYTES(long)
X2BYTES(int)
X2BYTES(short)
X2BYTES(float)
X2BYTES(double)
    
#define BYTES2X(ctype,htype)			\
I_						\
CAT3(bytes2,ctype,__)(P_ in, htype *out)	\
{						\
    union {					\
	ctype i;				\
	unsigned char cs[sizeof (ctype)];	\
    } u;					\
    unsigned int k;				\
    unsigned char *arr = (unsigned char *) in;	\
						\
    for (k = 0; k < sizeof(ctype); k++)		\
	u.cs[k] = arr[k];			\
						\
    *out = (htype) u.i;				\
						\
    return(sizeof (ctype));			\
}
    
static STG_INLINE
void
assign_flt(W_ p_dest[], StgFloat src)
{ 
    float_thing y;
    y.f = src;
    *p_dest = y.fu;
}


static STG_INLINE
void
assign_dbl(W_ p_dest[], StgDouble src)
{
    double_thing y;
    y.d = src;
    p_dest[0] = y.du.dhi;
    p_dest[1] = y.du.dlo;
}

#define BYTES2FX(ctype,htype,assign_fx)		\
I_						\
CAT3(bytes2,ctype,__)(P_ in, htype *out)	\
{						\
    union {					\
	ctype i;				\
	unsigned char cs[sizeof (ctype)];	\
    } u;					\
    unsigned int k;				\
    unsigned char *arr = (unsigned char *) in;	\
						\
    for (k = 0; k < sizeof(ctype); k++)		\
	u.cs[k] = arr[k];			\
						\
    assign_fx(out, (htype) u.i);		\
						\
    return(sizeof (ctype));			\
}
    
BYTES2X(long,I_)
BYTES2X(int,I_)
BYTES2X(short,I_)

BYTES2FX(float,StgFloat,assign_flt)
BYTES2FX(double,StgDouble,assign_dbl)
