e%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-99
%
\section[Time]{Haskell 1.4 Time of Day Library}

The {\em Time} library provides standard functionality for
clock times, including timezone information (i.e, the functionality of
"time.h",  adapted to the Haskell environment), It follows RFC 1129 in
its use of Coordinated Universal Time (UTC).

2000/06/17 <michael.weber@post.rwth-aachen.de>:
RESTRICTIONS:
  * min./max. time diff currently is restricted to
    [minBound::Int, maxBound::Int]

  * surely other restrictions wrt. min/max bounds


NOTES:
  * printing times

    `showTime' (used in `instance Show ClockTime') always prints time
    converted to the local timezone (even if it is taken from
    `(toClockTime . toUTCTime)'), whereas `calendarTimeToString'
    honors the tzone & tz fields and prints UTC or whatever timezone
    is stored inside CalendarTime.

    Maybe `showTime' should be changed to use UTC, since it would
    better correspond to the actual representation of `ClockTime'
    (can be done by replacing localtime(3) by gmtime(3)).


BUGS:
  * obvious bugs now should be fixed, but there are surely more (and
    less obvious one's) lurking around :-}

  * gettimeofday(2) returns secs and _microsecs_, not pico-secs!
    this should be changed accordingly (also means updating the H98
    report)

  * add proper handling of microsecs, currently, they're mostly
    ignored

  * `formatFOO' case of `%s' is currently broken...


TODO:
  * check for unusual date cases, like 1970/1/1 00:00h, and conversions
    between different timezone's etc.

  * check, what needs to be in the IO monad, the current situation
    seems to be a bit inconsistent to me

  * sync #ifdef'ed __HUGS__ parts with current changes (only few)

  * check whether `isDst = -1' works as expected on other arch's
    (Solaris anyone?)

  * add functions to parse strings to `CalendarTime' (some day...)

  * implement padding capabilities ("%_", "%-") in `formatFOO'

  * add rfc822 timezone (+0200 is CEST) representation ("%z") in `formatFOO'


\begin{code}
{-# OPTIONS -#include "cbits/timezone.h" -#include "cbits/stgio.h"  #-}
module Time 
     (
        Month(..)
     ,  Day(..)

     ,  ClockTime(..) -- non-standard, lib. report gives this as abstract
     ,	getClockTime

     ,  TimeDiff(..)
     ,  noTimeDiff      -- non-standard (but useful when constructing TimeDiff vals.)
     ,  diffClockTimes
     ,  addToClockTime

     ,  normalizeTimeDiff -- non-standard
     ,  timeDiffToString  -- non-standard
     ,  formatTimeDiff    -- non-standard

     ,  CalendarTime(..)
     ,	toCalendarTime
     ,  toUTCTime
     ,  toClockTime
     ,  calendarTimeToString
     ,  formatCalendarTime

     ) where

#ifdef __HUGS__
import PreludeBuiltin
#else
import PrelGHC		( RealWorld, (>#), (<#), (==#),
			  newIntArray#, readIntArray#, 
			  unsafeFreezeByteArray#,
			  int2Integer#, negateInt# )
import PrelBase		( Int(..) )
import PrelNum		( Integer(..), fromInt )
import PrelIOBase	( IO(..), unsafePerformIO, stToIO, constructErrorAndFail )
import PrelShow		( showList__ )
import PrelPack 	( unpackCString, unpackCStringBA,
			  new_ps_array, freeze_ps_array
			)
import PrelByteArr	( MutableByteArray(..) )
import PrelHandle	( Bytes )
import PrelAddr		( Addr )

#endif

import Ix
import Char     	( intToDigit )
import Locale

\end{code}

One way to partition and give name to chunks of a year and a week:

\begin{code}
data Month
 = January   | February | March    | April
 | May       | June     | July     | August
 | September | October  | November | December
 deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data Day 
 = Sunday   | Monday | Tuesday | Wednesday
 | Thursday | Friday | Saturday
 deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

\end{code}

@ClockTime@ is an abstract type, used for the internal clock time.
Clock times may be compared, converted to strings, or converted to an
external calendar time @CalendarTime@.

\begin{code}
#ifdef __HUGS__
-- I believe Int64 is more than big enough.
-- In fact, I think one of Int32 or Word32 would do. - ADR
data ClockTime = TOD Int64 Int64 deriving (Eq, Ord)
#else
data ClockTime = TOD Integer 		-- Seconds since 00:00:00 on 1 Jan 1970
		     Integer		-- Picoseconds with the specified second
	       deriving (Eq, Ord)
		
#endif
\end{code}

When a @ClockTime@ is shown, it is converted to a string of the form
@"Mon Nov 28 21:45:41 GMT 1994"@.

For now, we are restricted to roughly:
Fri Dec 13 20:45:52 1901 through Tue Jan 19 03:14:07 2038, because
we use the C library routines based on 32 bit integers.

\begin{code}
#ifdef __HUGS__
#warning Show ClockTime is bogus
instance Show ClockTime
#else
instance Show ClockTime where
    showsPrec p (TOD (S# i) _nsec) = 
      case int2Integer# i of (# s, d #) -> showsPrec p (TOD (J# s d) _nsec)
    showsPrec _ (TOD (J# s# d#) _nsec) = 
      showString $ unsafePerformIO $ do
            let buflen@(I# buflen#) = 50 -- big enough for error message
	    buf <- allocChars buflen 
	    if s# <# (negateInt# 1#) || s# ># 1# then
	       return "ClockTime.show{Time}: out of range"
	     else do
  	       rc <- showTime (I# s#) d# buflen buf
	       if rc < 0 then
	          return "ClockTime.show{Time}: internal error"
	        else do
		  ba <- stToIO (freeze_ps_array buf buflen#)
	          return (unpackCStringBA ba)

    showList = showList__ (showsPrec 0)
#endif
\end{code}


@CalendarTime@ is a user-readable and manipulable
representation of the internal $ClockTime$ type.  The
numeric fields have the following ranges.

\begin{verbatim}
Value         Range             Comments
-----         -----             --------

year    -maxInt .. maxInt       [Pre-Gregorian dates are inaccurate]
mon           0 .. 11           [Jan = 0, Dec = 11]
day           1 .. 31
hour          0 .. 23
min           0 .. 59
sec           0 .. 61           [Allows for two leap seconds]
picosec       0 .. (10^12)-1    [This could be over-precise?]
wday          0 .. 6            [Sunday = 0, Saturday = 6]
yday          0 .. 365          [364 in non-Leap years]
tz       -43200 .. 43200        [Variation from UTC in seconds]
\end{verbatim}

The {\em tzname} field is the name of the time zone.  The {\em isdst}
field indicates whether Daylight Savings Time would be in effect.

\begin{code}
data CalendarTime 
 = CalendarTime  {
     ctYear    :: Int,
     ctMonth   :: Month,
     ctDay     :: Int,
     ctHour    :: Int,
     ctMin     :: Int,
     ctSec     :: Int,
#ifdef __HUGS__
     ctPicosec :: Int64,
#else
     ctPicosec :: Integer,
#endif
     ctWDay    :: Day,
     ctYDay    :: Int,
     ctTZName  :: String,
     ctTZ      :: Int,
     ctIsDST   :: Bool
 }
 deriving (Eq,Ord,Read,Show)

\end{code}

The @TimeDiff@ type records the difference between two clock times in
a user-readable way.

\begin{code}
data TimeDiff
 = TimeDiff {
     tdYear    :: Int,
     tdMonth   :: Int,
     tdDay     :: Int,
     tdHour    :: Int,
     tdMin     :: Int,
     tdSec     :: Int,
#ifdef __HUGS__
     tdPicosec :: Int64   -- not standard
#else
     tdPicosec :: Integer -- not standard
#endif
   }
   deriving (Eq,Ord,Read,Show)

noTimeDiff :: TimeDiff
noTimeDiff = TimeDiff 0 0 0 0 0 0 0
\end{code}

@getClockTime@ returns the current time in its internal representation.

\begin{code}
getClockTime :: IO ClockTime
getClockTime = do
    i1 <- malloc1
    i2 <- malloc1
    rc <- primGetClockTime i1 i2
    if rc == 0 
	then do
	    sec  <- cvtUnsigned i1
	    nsec <- cvtUnsigned i2
	    return (TOD sec (nsec * 1000))
    	else
	    constructErrorAndFail "getClockTime"

#ifdef __HUGS__
malloc1 = primNewByteArray sizeof_int64
cvtUnsigned arr = primReadInt64Array arr 0
#else
malloc1 :: IO (MutableByteArray RealWorld Int)
malloc1 = IO $ \ s# ->
  case newIntArray# 1# s# of 
   (# s2#, barr# #) -> (# s2#, MutableByteArray bot bot barr# #)
  where 
	bot = error "Time.malloc1"

   --  The C routine fills in an unsigned word.  We don't have 
   --	`unsigned2Integer#,' so we freeze the data bits and use them 
   --	for an MP_INT structure.  Note that zero is still handled specially,
   --	although (J# 1# (ptr to 0#)) is probably acceptable to gmp.

cvtUnsigned :: MutableByteArray RealWorld Int -> IO Integer
cvtUnsigned (MutableByteArray _ _ arr#) = IO $ \ s# ->
  case readIntArray# arr# 0# s# of 
    (# s2#, r# #) | r# ==# 0#  -> (# s2#, 0 #)
   	          | otherwise  ->
            	     case unsafeFreezeByteArray# arr# s2# of
                       (# s3#, frozen# #) -> (# s3#, J# 1# frozen# #)
#endif
\end{code}

@addToClockTime@ {\em d} {\em t} adds a time difference {\em d} and a
clock time {\em t} to yield a new clock time.  The difference {\em d}
may be either positive or negative.  @[diffClockTimes@ {\em t1} {\em
t2} returns the difference between two clock times {\em t1} and {\em
t2} as a @TimeDiff@.


\begin{code}
addToClockTime  :: TimeDiff  -> ClockTime -> ClockTime
addToClockTime (TimeDiff year mon day hour min sec psec) 
	       (TOD c_sec c_psec) = 
	let
	  sec_diff = fromInt sec + 60 * fromInt min + 3600 * fromInt hour + 24 * 3600 * fromInt day
	  cal      = toUTCTime (TOD (c_sec + sec_diff) (c_psec + psec))
                                                       -- FIXME! ^^^^
          new_mon  = fromEnum (ctMonth cal) + r_mon 
	  (month', yr_diff)
	    | new_mon < 0  = (toEnum (12 + new_mon), (-1))
	    | new_mon > 11 = (toEnum (new_mon `mod` 12), 1)
	    | otherwise    = (toEnum new_mon, 0)
	    
	  (r_yr, r_mon) = mon `quotRem` 12

          year' = ctYear cal + year + r_yr + yr_diff
	in
	toClockTime cal{ctMonth=month', ctYear=year'}

diffClockTimes  :: ClockTime -> ClockTime -> TimeDiff
-- diffClockTimes is meant to be the dual to `addToClockTime'.
-- If you want to have the TimeDiff properly splitted, use
-- `normalizeTimeDiff' on this function's result
--
-- CAVEAT: see comment of normalizeTimeDiff
diffClockTimes (TOD sa pa) (TOD sb pb) =
    noTimeDiff{ tdSec     = fromIntegral (sa - sb) 
                -- FIXME: can handle just 68 years...
              , tdPicosec = pa - pb
              }


normalizeTimeDiff :: TimeDiff -> TimeDiff
-- FIXME: handle psecs properly
-- FIXME: ?should be called by formatTimeDiff automagically?
--
-- when applied to something coming out of `diffClockTimes', you loose
-- the duality to `addToClockTime', since a year does not always have
-- 365 days, etc.
--
-- apply this function as late as possible to prevent those "rounding"
-- errors
normalizeTimeDiff td =
  let
      rest0 = tdSec td 
               + 60 * (tdMin td 
                    + 60 * (tdHour td 
                         + 24 * (tdDay td 
                              + 30 * (tdMonth td 
                                   + 365 * tdYear td))))

      (diffYears,  rest1)    = rest0 `quotRem` (365 * 24 * 3600)
      (diffMonths, rest2)    = rest1 `quotRem` (30 * 24 * 3600)
      (diffDays,   rest3)    = rest2 `quotRem` (24 * 3600)
      (diffHours,  rest4)    = rest3 `quotRem` 3600
      (diffMins,   diffSecs) = rest4 `quotRem` 60
  in
      td{ tdYear = diffYears
        , tdMonth = diffMonths
        , tdDay   = diffDays
        , tdHour  = diffHours
        , tdMin   = diffMins
        , tdSec   = diffSecs
        }

\end{code}

@toCalendarTime@ {\em t} converts {\em t} to a local time, modified by
the current timezone and daylight savings time settings.  @toUTCTime@
{\em t} converts {\em t} into UTC time.  @toClockTime@ {\em l}
converts {\em l} into the corresponding internal @ClockTime@.  The
{\em wday}, {\em yday}, {\em tzname}, and {\em isdst} fields are
ignored.

\begin{code}
#ifdef __HUGS__
toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime (TOD sec psec) = do
    res    <- allocWords sizeof_int64
    zoneNm <- allocChars 32
    prim_SETZONE res zoneNm
    rc <- prim_toLocalTime sec res
    if rc /= 0
     then constructErrorAndFail "Time.toCalendarTime: out of range"
     else do
       sec   <-  get_tm_sec   res
       min   <-  get_tm_min   res
       hour  <-  get_tm_hour  res
       mday  <-  get_tm_mday  res
       mon   <-  get_tm_mon   res
       year  <-  get_tm_year  res
       wday  <-  get_tm_wday  res
       yday  <-  get_tm_yday  res
       isdst <-  get_tm_isdst res
       zone  <-  prim_ZONE    res
       tz    <-  prim_GMTOFF  res
       tzname <- primUnpackCString zone
       return (CalendarTime (1900+year) mon mday hour min sec psec 
            		    (toEnum wday) yday tzname tz (isdst /= 0))

toUTCTime :: ClockTime -> CalendarTime
toUTCTime  (TOD sec psec) = unsafePerformIO $ do
       res    <- allocWords sizeof_int64
       zoneNm <- allocChars 32
       prim_SETZONE res zoneNm
       rc <- prim_toUTCTime sec res
       if rc /= 0
	then error "Time.toUTCTime: out of range"
        else do
	    sec   <- get_tm_sec  res
	    min   <- get_tm_min  res
	    hour  <- get_tm_hour res
	    mday  <- get_tm_mday res
	    mon   <- get_tm_mon  res
	    year  <- get_tm_year res
	    wday  <- get_tm_wday res
	    yday  <- get_tm_yday res
            return (CalendarTime (1900+year) mon mday hour min sec psec 
            		  (toEnum wday) yday "UTC" 0 False)

toClockTime :: CalendarTime -> ClockTime
toClockTime (CalendarTime year mon mday hour min sec psec wday yday tzname tz isdst) =
    if psec < 0 || psec > 999999999999 then
        error "Time.toClockTime: picoseconds out of range"
    else if tz < -43200 || tz > 43200 then
        error "Time.toClockTime: timezone offset out of range"
    else
        unsafePerformIO ( do
	    res <- allocWords sizeof_int64
	    rc <- toClockSec year (fromEnum mon) mday hour min sec isDst res
            if rc /= (0::Int)
             then do
               tm <- primReadInt64Array res 0
               return (TOD tm psec)
	     else error "Time.toClockTime: can't perform conversion"
        )
    where
     isDst = if isdst then (1::Int) else 0

#else
toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime (TOD (S# i) psec) 
  = case int2Integer# i of (# s, d #) -> toCalendarTime (TOD (J# s d) psec)
toCalendarTime (TOD (J# s# d#) psec) = do
    res    <- allocWords sizeof_struct_tm
    zoneNm <- allocChars 32
    prim_SETZONE res zoneNm
    rc     <- prim_toLocalTime (I# s#) d# res
    if rc == 0
     then constructErrorAndFail "Time.toCalendarTime: out of range"
     else do
       sec   <-  get_tm_sec res
       min   <-  get_tm_min res
       hour  <-  get_tm_hour res
       mday  <-  get_tm_mday res
       mon   <-  get_tm_mon  res
       year  <-  get_tm_year res
       wday  <-  get_tm_wday res
       yday  <-  get_tm_yday res
       isdst <-  get_tm_isdst res
       zone  <-  get_ZONE res
       tz    <-  get_GMTOFF res
       let tzname = unpackCString zone
           month  
	    | mon >= 0 && mon <= 11 = toEnum mon
	    | otherwise             = error ("toCalendarTime: illegal month value: " ++ show mon)
	    
       return (CalendarTime (1900+year) month mday hour min sec psec 
            		    (toEnum wday) yday tzname tz (isdst /= (0::Int)))

toUTCTime :: ClockTime -> CalendarTime
toUTCTime (TOD (S# i) psec) 
  = case int2Integer# i of (# s, d #) -> toUTCTime (TOD (J# s d) psec)
toUTCTime  (TOD (J# s# d#) psec) = unsafePerformIO $ do
       res    <- allocWords sizeof_struct_tm
       zoneNm <- allocChars 32
       prim_SETZONE res zoneNm
       rc     <-  prim_toUTCTime (I# s#) d# res
       if rc == 0
	then error "Time.toUTCTime: out of range"
        else do
	    sec   <- get_tm_sec res
	    min   <- get_tm_min res
	    hour  <- get_tm_hour res
	    mday  <- get_tm_mday res
	    mon   <- get_tm_mon res
	    year  <- get_tm_year res
	    wday  <- get_tm_wday res
	    yday  <- get_tm_yday res
	    let
             month  
	      | mon >= 0 && mon <= 11 = toEnum mon
	      | otherwise             = error ("toCalendarTime: illegal month value: " ++ show mon)

            return (CalendarTime (1900+year) month mday hour min sec psec 
            		  (toEnum wday) yday "UTC" 0 False)

toClockTime :: CalendarTime -> ClockTime
toClockTime (CalendarTime year mon mday hour min sec psec _wday _yday _tzname tz isdst) =
    if psec < 0 || psec > 999999999999 then
        error "Time.toClockTime: picoseconds out of range"
    else if tz < -43200 || tz > 43200 then
        error "Time.toClockTime: timezone offset out of range"
    else
        unsafePerformIO ( do
	    res <- malloc1
	    rc  <- toClockSec year (fromEnum mon) mday hour min sec tz isDst res
            if rc /= 0
             then do
	       i <- cvtUnsigned res
	       return (TOD i psec)
	     else error "Time.toClockTime: can't perform conversion"
        )
    where
     -- `isDst' causes the date to be wrong by one hour...
     -- FIXME: check, whether this works on other arch's than Linux, too...
     -- 
     -- so we set it to (-1) (means `unknown') and let `mktime' determine
     -- the real value...
     isDst = -1     -- if isdst then (1::Int) else 0
#endif


-- (copied from PosixUtil, for now)
-- Allocate a mutable array of characters with no indices.

#ifdef __HUGS__
allocChars :: Int -> IO (PrimMutableByteArray RealWorld)
allocChars size = primNewByteArray size

-- Allocate a mutable array of words with no indices

allocWords :: Int -> IO (PrimMutableByteArray RealWorld)
allocWords size = primNewByteArray size
#else
allocChars :: Int -> IO (MutableByteArray RealWorld Int)
allocChars (I# size#) = stToIO (new_ps_array size#)

-- Allocate a mutable array of words with no indices

allocWords :: Int -> IO (MutableByteArray RealWorld Int)
allocWords (I# size#) = IO $ \ s# ->
    case newIntArray# size# s# of 
      (# s2#, barr# #) -> 
	(# s2#, MutableByteArray bot bot barr# #)
  where
    bot = error "Time.allocWords"
#endif
\end{code}

\begin{code}
calendarTimeToString  :: CalendarTime -> String
calendarTimeToString  =  formatCalendarTime defaultTimeLocale "%c"

formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime l fmt (CalendarTime year mon day hour min sec _
                                       wday yday tzname _ _) =
        doFmt fmt
  where doFmt ('%':'-':cs) = doFmt ('%':cs) -- padding not implemented
        doFmt ('%':'_':cs) = doFmt ('%':cs) -- padding not implemented
        doFmt ('%':c:cs)   = decode c ++ doFmt cs
        doFmt (c:cs) = c : doFmt cs
        doFmt "" = ""

        decode 'A' = fst (wDays l  !! fromEnum wday) -- day of the week, full name
        decode 'a' = snd (wDays l  !! fromEnum wday) -- day of the week, abbrev.
        decode 'B' = fst (months l !! fromEnum mon)  -- month, full name
        decode 'b' = snd (months l !! fromEnum mon)  -- month, abbrev
        decode 'h' = snd (months l !! fromEnum mon)  -- ditto
        decode 'C' = show2 (year `quot` 100)         -- century
        decode 'c' = doFmt (dateTimeFmt l)           -- locale's data and time format.
        decode 'D' = doFmt "%m/%d/%y"
        decode 'd' = show2 day                       -- day of the month
        decode 'e' = show2' day                      -- ditto, padded
        decode 'H' = show2 hour                      -- hours, 24-hour clock, padded
        decode 'I' = show2 (to12 hour)               -- hours, 12-hour clock
        decode 'j' = show3 yday                      -- day of the year
        decode 'k' = show2' hour                     -- hours, 24-hour clock, no padding
        decode 'l' = show2' (to12 hour)              -- hours, 12-hour clock, no padding
        decode 'M' = show2 min                       -- minutes
        decode 'm' = show2 (fromEnum mon+1)          -- numeric month
        decode 'n' = "\n"
        decode 'p' = (if hour < 12 then fst else snd) (amPm l) -- am or pm
        decode 'R' = doFmt "%H:%M"
        decode 'r' = doFmt (time12Fmt l)
        decode 'T' = doFmt "%H:%M:%S"
        decode 't' = "\t"
        decode 'S' = show2 sec			     -- seconds
        decode 's' = show2 sec			     -- number of secs since Epoch. (ToDo.)
        decode 'U' = show2 ((yday + 7 - fromEnum wday) `div` 7) -- week number, starting on Sunday.
        decode 'u' = show (let n = fromEnum wday in  -- numeric day of the week (1=Monday, 7=Sunday)
                           if n == 0 then 7 else n)
        decode 'V' =                                 -- week number (as per ISO-8601.)
            let (week, days) =                       -- [yep, I've always wanted to be able to display that too.]
                   (yday + 7 - if fromEnum wday > 0 then 
                               fromEnum wday - 1 else 6) `divMod` 7
            in  show2 (if days >= 4 then
                          week+1 
                       else if week == 0 then 53 else week)

        decode 'W' =				     -- week number, weeks starting on monday
            show2 ((yday + 7 - if fromEnum wday > 0 then 
                               fromEnum wday - 1 else 6) `div` 7)
        decode 'w' = show (fromEnum wday)            -- numeric day of the week, weeks starting on Sunday.
        decode 'X' = doFmt (timeFmt l)               -- locale's preferred way of printing time.
        decode 'x' = doFmt (dateFmt l)               -- locale's preferred way of printing dates.
        decode 'Y' = show year                       -- year, including century.
        decode 'y' = show2 (year `rem` 100)          -- year, within century.
        decode 'Z' = tzname                          -- timezone name
        decode '%' = "%"
        decode c   = [c]


show2, show2', show3 :: Int -> String
show2 x = [intToDigit (x `quot` 10), intToDigit (x `rem` 10)]

show2' x = if x < 10 then [ ' ', intToDigit x] else show2 x

show3 x = intToDigit (x `quot` 100) : show2 (x `rem` 100)

to12 :: Int -> Int
to12 h = let h' = h `mod` 12 in if h' == 0 then 12 else h'
\end{code}

Useful extensions for formatting TimeDiffs.

\begin{code}
timeDiffToString :: TimeDiff -> String
timeDiffToString = formatTimeDiff defaultTimeLocale "%c"

formatTimeDiff :: TimeLocale -> String -> TimeDiff -> String
formatTimeDiff l fmt td@(TimeDiff year month day hour min sec _)
 = doFmt fmt
  where 
   doFmt ""         = ""
   doFmt ('%':'-':cs) = doFmt ('%':cs) -- padding not implemented
   doFmt ('%':'_':cs) = doFmt ('%':cs) -- padding not implemented
   doFmt ('%':c:cs) = decode c ++ doFmt cs
   doFmt (c:cs)     = c : doFmt cs

   decode spec =
    case spec of
      'B' -> fst (months l !! fromEnum month)
      'b' -> snd (months l !! fromEnum month)
      'h' -> snd (months l !! fromEnum month)
      'c' -> defaultTimeDiffFmt td
      'C' -> show2 (year `quot` 100)
      'D' -> doFmt "%m/%d/%y"
      'd' -> show2 day
      'e' -> show2' day
      'H' -> show2 hour
      'I' -> show2 (to12 hour)
      'k' -> show2' hour
      'l' -> show2' (to12 hour)
      'M' -> show2 min
      'm' -> show2 (fromEnum month + 1)
      'n' -> "\n"
      'p' -> (if hour < 12 then fst else snd) (amPm l)
      'R' -> doFmt "%H:%M"
      'r' -> doFmt (time12Fmt l)
      'T' -> doFmt "%H:%M:%S"
      't' -> "\t"
      'S' -> show2 sec
      's' -> show2 sec -- Implementation-dependent, sez the lib doc..
      'X' -> doFmt (timeFmt l)
      'x' -> doFmt (dateFmt l)
      'Y' -> show year
      'y' -> show2 (year `rem` 100)
      '%' -> "%"
      c   -> [c]

   defaultTimeDiffFmt (TimeDiff year month day hour min sec _) =
       foldr (\ (v,s) rest -> 
                  (if v /= 0 
                     then show v ++ ' ':(addS v s)
                       ++ if null rest then "" else ", "
                     else "") ++ rest
             )
             ""
             (zip [year, month, day, hour, min, sec] (intervals l))

   addS v s = if abs v == 1 then fst s else snd s
\end{code}

\begin{code}
foreign import "libHS_cbits" "get_tm_sec"   unsafe get_tm_sec   :: MBytes -> IO Int
foreign import "libHS_cbits" "get_tm_min"   unsafe get_tm_min   :: MBytes -> IO Int
foreign import "libHS_cbits" "get_tm_hour"  unsafe get_tm_hour  :: MBytes -> IO Int
foreign import "libHS_cbits" "get_tm_mday"  unsafe get_tm_mday  :: MBytes -> IO Int
foreign import "libHS_cbits" "get_tm_mon"   unsafe get_tm_mon   :: MBytes -> IO Int
foreign import "libHS_cbits" "get_tm_year"  unsafe get_tm_year  :: MBytes -> IO Int
foreign import "libHS_cbits" "get_tm_wday"  unsafe get_tm_wday  :: MBytes -> IO Int
foreign import "libHS_cbits" "get_tm_yday"  unsafe get_tm_yday  :: MBytes -> IO Int
foreign import "libHS_cbits" "get_tm_isdst" unsafe get_tm_isdst :: MBytes -> IO Int
	       		   
foreign import "libHS_cbits" "prim_ZONE"    unsafe prim_ZONE    :: Bytes -> IO Addr
foreign import "libHS_cbits" "prim_GMTOFF"  unsafe prim_GMTOFF  :: Bytes -> IO Int
	       		   
foreign import "libHS_cbits" "sizeof_struct_tm" unsafe sizeof_struct_tm :: Int

#ifdef __HUGS__
-- believed to be at least 1 bit (the sign bit!) bigger than sizeof_time_t
sizeof_int64 :: Int
sizeof_int64 = 8
#endif

type MBytes = MutableByteArray RealWorld Int

foreign import "libHS_cbits" "sizeof_time_t" unsafe sizeof_time_t    :: Int

foreign import "libHS_cbits" "prim_SETZONE" unsafe prim_SETZONE :: MBytes -> MBytes -> IO ()
#ifdef __HUGS__
foreign import "libHS_cbits" "prim_toLocalTime"  unsafe prim_toLocalTime :: Int64 -> MBytes -> IO Int
foreign import "libHS_cbits" "prim_toUTCTime"    unsafe prim_toUTCTime   :: Int64 -> MBytes -> IO Int
#else
foreign import "libHS_cbits" "toLocalTime"  unsafe prim_toLocalTime :: Int -> Bytes -> MBytes -> IO Int
foreign import "libHS_cbits" "toUTCTime"    unsafe prim_toUTCTime   :: Int -> Bytes -> MBytes -> IO Int
#endif

foreign import "libHS_cbits" "get_ZONE"  unsafe get_ZONE   :: MBytes -> IO Addr
foreign import "libHS_cbits" "GMTOFF"    unsafe get_GMTOFF :: MBytes -> IO Int


foreign import "libHS_cbits" "toClockSec" unsafe 
            toClockSec   :: Int -> Int -> Int -> Int -> Int 
	    		 -> Int -> Int -> Int -> MBytes -> IO Int

foreign import "libHS_cbits" "getClockTime"  unsafe 
           primGetClockTime :: MutableByteArray RealWorld Int
	                    -> MutableByteArray RealWorld Int
			    -> IO Int
foreign import "libHS_cbits" "showTime" unsafe 
           showTime :: Int
	            -> Bytes
		    -> Int
		    -> MBytes
		    -> IO Int
\end{code}
