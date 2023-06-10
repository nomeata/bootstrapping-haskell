################################################################
# Makefile for Win32 library
#
# This Makefile only works with GNUMake.  If you feel like supplying
# us with a more portable Makefile, we'll be happy to distribute it
# as well.
################################################################

# This goes first to make it the default
default		: all

# All generated C files #include errors.h to get consistent error messages
$(DLLS)		: errors.h

GUILIBS		= kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib comctl32.lib winmm.lib advapi32.lib


# Describe how your system is configured here

ROOT		= $(HOME)
HUGSDIR 	= $(ROOT)/hugs
GCDIR		= $(ROOT)/fptools/green-card
RUNHUGS		= $(ROOT)/hugs/runhugs
CPP		= gcc -P -E -x c -traditional

# GC	    	= $(ROOT)/green-card.exe --target Hugs
GC	    	= $(RUNHUGS) -F"$(CPP)" -h1m $(GCDIR)/src/GreenCard.lhs --target Hugs
GC_INCLUDES 	= --include-dir $(GCDIR)/lib/hugs
GCPP_FLAGS	= -DTARGET_HUGS

# Where to find GreenCard.h - in the Hugs source code
INCLUDES	= -I $(HUGSDIR)/src

################################################################
# Explicit dependencies
################################################################

Win32Window.dll: WndProc.c

################################################################
# Standard rules from here on
################################################################

GCS  		= $(wildcard *.gc)
DLLS 		= $(addsuffix .dll, $(basename $(GCS)))
GEN_HSS  	= $(addsuffix .hs,  $(basename $(GCS)))
GEN_CFILES 	= $(addsuffix .c, $(basename $(GCS)))

all:		$(GEN_HSS) $(DLLS) 

.SUFFIXES	:
.SUFFIXES	: .pgc .gc .hs .dll .c

%.hs %.c	: %.gc
		$(CPP) $(GCPP_FLAGS) $< | perl -pe 's#\\n#\n#g' >$*_cpp.gc
		$(GC) $(GC_INCLUDES) -I . $*_cpp.gc
		rm $*_cpp.gc
		mv $*_cpp.hs $*.hs
		mv $*_cpp.c  $*.c
%.dll		: %.c
		cl /nologo /LD /MD $(INCLUDES) $(GUILIBS) $(GCPP_FLAGS) -DSTRICT -o $@ $*.c
%.obj		: %.c
		cl /nologo $(INCLUDES) $(GCPP_FLAGS) -DSTRICT -o $@ $*.c

# Cleanliness is next to dependencies

clean		:
		rm -f *.obj *.exp *.lib 
		rm -f *.hi
		rm -f $(GEN_CFILES) $(GEN_HSS) $(DLLS)

# Dependencies

$(GCS)		: $(PGCS)
depends.mk	::
		perl mkGCDep *.gc >depends.mk

include depends.mk

################################################################
# End of Makefile
################################################################
