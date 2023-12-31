/* --------------------------------------------------------------------------
   Time-stamp: <Sun Dec 05 1999 21:02:36 Stardate: [-30]4004.38 hwloidl>
   $Id: HLC.h,v 1.2 2000/01/13 14:34:07 hwloidl Exp $

   High Level Communications Header (HLC.h)

   Contains the high-level definitions (i.e. communication
   subsystem independent) used by GUM
   Phil Trinder, Glasgow University, 12 December 1994
   H-W. Loidl, Heriot-Watt, November 1999
   ----------------------------------------------------------------------- */

#ifndef __HLC_H
#define __HLC_H

#ifdef PAR

#include "LLC.h"

#define NEW_FISH_AGE           0
#define NEW_FISH_HISTORY       0
#define NEW_FISH_HUNGER        0
#define FISH_LIFE_EXPECTANCY  10


//@node GUM Message Sending and Unpacking Functions
//@subsection GUM Message Sending and Unpacking Functions

rtsBool  initMoreBuffers(void);

void 	 sendFetch (globalAddr *ga, globalAddr *bqga, int load);
void 	 sendResume(globalAddr *rga, int nelem, rtsPackBuffer *data);
void 	 sendAck (GlobalTaskId task, int ngas, globalAddr *gagamap);
void 	 sendFish (GlobalTaskId destPE, GlobalTaskId origPE, int age, int history, int hunger);
void 	 sendFree (GlobalTaskId destPE, int nelem, P_ data);
void 	 sendSchedule(GlobalTaskId origPE, int nelem, rtsPackBuffer *data);

//@node Message-Processing Functions
//@subsection Message-Processing Functions

void 	 processMessages(void);
void 	 processFetches(void);
void 	 processTheRealFetches(void);

//@node Miscellaneous Functions
//@subsection Miscellaneous Functions

void 	 prepareFreeMsgBuffers(void);
void 	 freeRemoteGA (int pe, globalAddr *ga);
void 	 sendFreeMessages(void);

GlobalTaskId  choosePE(void);
StgClosure   *createBlockedFetch (globalAddr ga, globalAddr rga);
void 	      waitForTermination(void);

void          DebugPrintGAGAMap (globalAddr *gagamap, int nGAs);

#endif /* PAR */
#endif /* __HLC_H */
