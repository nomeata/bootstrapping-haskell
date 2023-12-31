/* -----------------------------------------------------------------------------
 * $Id: StrHash.c,v 1.1 2000/04/05 15:32:08 simonmar Exp $
 *
 * (c) The GHC Team, 1994-2000
 *
 * Hashing functions based on:
 *
 *   "Fast Hashing of Variable Length Text Strings"
 *    Peter K. Pearson, CACM June 1990
 *
 * They return a 32 bit value containing 16 bits of hash value.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "StrHash.h"

static const StgChar auxTable[] = {
(StgChar)0x01, (StgChar)0x57, (StgChar)0x31, (StgChar)0x0c, (StgChar)0xb0, (StgChar)0xb2, (StgChar)0x66, (StgChar)0xa6, 
(StgChar)0x79, (StgChar)0xc1, (StgChar)0x06, (StgChar)0x54, (StgChar)0xf9, (StgChar)0xe6, (StgChar)0x2c, (StgChar)0xa3, 
(StgChar)0x0e, (StgChar)0xc5, (StgChar)0xd5, (StgChar)0xb5, (StgChar)0xa1, (StgChar)0x55, (StgChar)0xda, (StgChar)0x50, 
(StgChar)0x40, (StgChar)0xef, (StgChar)0x18, (StgChar)0xe2, (StgChar)0xec, (StgChar)0x8e, (StgChar)0x26, (StgChar)0xc8, 
(StgChar)0x6e, (StgChar)0xb1, (StgChar)0x68, (StgChar)0x67, (StgChar)0x8d, (StgChar)0xfd, (StgChar)0xff, (StgChar)0x32, 
(StgChar)0x4d, (StgChar)0x65, (StgChar)0x51, (StgChar)0x12, (StgChar)0x2d, (StgChar)0x60, (StgChar)0x1f, (StgChar)0xde, 
(StgChar)0x19, (StgChar)0x6b, (StgChar)0xbe, (StgChar)0x46, (StgChar)0x56, (StgChar)0xed, (StgChar)0xf0, (StgChar)0x22, 
(StgChar)0x48, (StgChar)0xf2, (StgChar)0x14, (StgChar)0xd6, (StgChar)0xf4, (StgChar)0xe3, (StgChar)0x95, (StgChar)0xeb, 
(StgChar)0x61, (StgChar)0xea, (StgChar)0x39, (StgChar)0x16, (StgChar)0x3c, (StgChar)0xfa, (StgChar)0x52, (StgChar)0xaf, 
(StgChar)0xd0, (StgChar)0x05, (StgChar)0x7f, (StgChar)0xc7, (StgChar)0x6f, (StgChar)0x3e, (StgChar)0x87, (StgChar)0xf8, 
(StgChar)0xae, (StgChar)0xa9, (StgChar)0xd3, (StgChar)0x3a, (StgChar)0x42, (StgChar)0x9a, (StgChar)0x6a, (StgChar)0xc3, 
(StgChar)0xf5, (StgChar)0xab, (StgChar)0x11, (StgChar)0xbb, (StgChar)0xb6, (StgChar)0xb3, (StgChar)0x00, (StgChar)0xf3, 
(StgChar)0x84, (StgChar)0x38, (StgChar)0x94, (StgChar)0x4b, (StgChar)0x80, (StgChar)0x85, (StgChar)0x9e, (StgChar)0x64, 
(StgChar)0x82, (StgChar)0x7e, (StgChar)0x5b, (StgChar)0x0d, (StgChar)0x99, (StgChar)0xf6, (StgChar)0xd8, (StgChar)0xdb, 
(StgChar)0x77, (StgChar)0x44, (StgChar)0xdf, (StgChar)0x4e, (StgChar)0x53, (StgChar)0x58, (StgChar)0xc9, (StgChar)0x63, 
(StgChar)0x7a, (StgChar)0x0b, (StgChar)0x5c, (StgChar)0x20, (StgChar)0x88, (StgChar)0x72, (StgChar)0x34, (StgChar)0x0a, 
(StgChar)0x8a, (StgChar)0x1e, (StgChar)0x30, (StgChar)0xb7, (StgChar)0x9c, (StgChar)0x23, (StgChar)0x3d, (StgChar)0x1a, 
(StgChar)0x8f, (StgChar)0x4a, (StgChar)0xfb, (StgChar)0x5e, (StgChar)0x81, (StgChar)0xa2, (StgChar)0x3f, (StgChar)0x98, 
(StgChar)0xaa, (StgChar)0x07, (StgChar)0x73, (StgChar)0xa7, (StgChar)0xf1, (StgChar)0xce, (StgChar)0x03, (StgChar)0x96, 
(StgChar)0x37, (StgChar)0x3b, (StgChar)0x97, (StgChar)0xdc, (StgChar)0x5a, (StgChar)0x35, (StgChar)0x17, (StgChar)0x83, 
(StgChar)0x7d, (StgChar)0xad, (StgChar)0x0f, (StgChar)0xee, (StgChar)0x4f, (StgChar)0x5f, (StgChar)0x59, (StgChar)0x10, 
(StgChar)0x69, (StgChar)0x89, (StgChar)0xe1, (StgChar)0xe0, (StgChar)0xd9, (StgChar)0xa0, (StgChar)0x25, (StgChar)0x7b, 
(StgChar)0x76, (StgChar)0x49, (StgChar)0x02, (StgChar)0x9d, (StgChar)0x2e, (StgChar)0x74, (StgChar)0x09, (StgChar)0x91, 
(StgChar)0x86, (StgChar)0xe4, (StgChar)0xcf, (StgChar)0xd4, (StgChar)0xca, (StgChar)0xd7, (StgChar)0x45, (StgChar)0xe5, 
(StgChar)0x1b, (StgChar)0xbc, (StgChar)0x43, (StgChar)0x7c, (StgChar)0xa8, (StgChar)0xfc, (StgChar)0x2a, (StgChar)0x04, 
(StgChar)0x1d, (StgChar)0x6c, (StgChar)0x15, (StgChar)0xf7, (StgChar)0x13, (StgChar)0xcd, (StgChar)0x27, (StgChar)0xcb, 
(StgChar)0xe9, (StgChar)0x28, (StgChar)0xba, (StgChar)0x93, (StgChar)0xc6, (StgChar)0xc0, (StgChar)0x9b, (StgChar)0x21, 
(StgChar)0xa4, (StgChar)0xbf, (StgChar)0x62, (StgChar)0xcc, (StgChar)0xa5, (StgChar)0xb4, (StgChar)0x75, (StgChar)0x4c, 
(StgChar)0x8c, (StgChar)0x24, (StgChar)0xd2, (StgChar)0xac, (StgChar)0x29, (StgChar)0x36, (StgChar)0x9f, (StgChar)0x08, 
(StgChar)0xb9, (StgChar)0xe8, (StgChar)0x71, (StgChar)0xc4, (StgChar)0xe7, (StgChar)0x2f, (StgChar)0x92, (StgChar)0x78, 
(StgChar)0x33, (StgChar)0x41, (StgChar)0x1c, (StgChar)0x90, (StgChar)0xfe, (StgChar)0xdd, (StgChar)0x5d, (StgChar)0xbd, 
(StgChar)0xc2, (StgChar)0x8b, (StgChar)0x70, (StgChar)0x2b, (StgChar)0x47, (StgChar)0x6d, (StgChar)0xb8, (StgChar)0xd1};

hash_t
hash_str(char *str)
{
    hash_t h1, h2;
    hash_t ch = (hash_t) *(str++);

    if (ch == 0) return 0;

    h1 = (hash_t) auxTable[ch];
    h2 = (hash_t) auxTable[ch+1];

    while ( (ch = (hash_t)*(str++) ) != 0) {
	h1 = (hash_t) auxTable[h1^ch];
	h2 = (hash_t) auxTable[h2^ch];
    }
    
    return ( h2 << 8 | h1 );
}

hash_t
hash_fixed(char *data, nat len)
{
    hash_t ch = (hash_t) *(data++);
    hash_t h1 = (hash_t) auxTable[ch];
    hash_t h2 = (hash_t) auxTable[ch+1];

    ASSERT(len > 0);

    while (--len > 0) {
	ch = (hash_t) *(data++);
	h1 = (hash_t) auxTable[h1^ch];
	h2 = (hash_t) auxTable[h2^ch];
    }
    
    return ( h2 << 8 | h1 );
}
