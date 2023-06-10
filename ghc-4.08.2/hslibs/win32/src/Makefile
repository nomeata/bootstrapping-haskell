#
# Joint Hugs/GHC (GNU) Makefile for GreenCard-based Win32 library
#

#== Start of user-serviceable parts

# What system you're building for.
# Legal values: ghc, hugs
FOR_SYSTEM=ghc

# Whether the FFI backend is being used or not.
# Legal values: YES and NO.
USING_FFI_BACKEND=NO

# ghc only: library kinds
# Legal values: normal, dll , profiled
LIB_WAYS=normal dll

# Are you building the library optimised or not?
# Legal values: YES, NO
LIB_OPTIMISED=YES


# Toplevel directory of green-card distribution.
# If you're using an installed green-card, you'll
# need to set the GCLIBDIR and GC variables explicitly.
#
GCDIR		= ../../../green-card
ifeq "$(FOR_SYSTEM)" "ghc"
GCLIBDIR        = $(GCDIR)/lib/ghc
GC	    	= $(GCDIR)/src/green-card
endif
ifeq "$(FOR_SYSTEM)" "hugs"
GCDIR_DOS	= z:/fptools-new-rts/green-card
GCLIBDIR        = $(GCDIR)/lib/hugs:.
GC	    	= $(RUNHUGS) -h1m  $(GCDIR_DOS)/src/GreenCard.lhs
endif

GHC    	        = ../../../ghc/driver/ghc-inplace
HUGSDIR 	= /hugs98
HUGSSRCDIR      = c:/hugs/src
RUNHUGS	        = $(HUGSDIR)/runhugs
CPP		= gcc -P -E -x c -traditional

#== End of user serviceable parts
#   (you shouldn't have to edit any of what follows.)


all::
# These specify the input files and some of the intermediate files

SRCS		= $(wildcard *.gc *.pgc) 
C_SRCS		+= $(patsubst %.gc, , $(wildcard *.gc))
C_SRCS		+= $(patsubst %.pgc, , $(wildcard *.pgc))

HS		= $(addsuffix .hs, $(basename $(SRCS)))
STUB_OBJS	+= $(patsubst %.c, %.o, $(C_SRCS))

ifeq "$(USING_FFI_BACKEND)" "YES"
STUB_OBJS	+= $(addsuffix _stub.o, $(basename $(SRCS)))
endif

# Conversion from long format LIB_WAYS list into short format:
#  normal          -> n
#  dll             -> dll
#  profiled        -> p
SHORT_NORMAL=n
SHORT_DLL=dll
SHORT_PROF=p
SHORT_CONC=mc
SHORT_PROFCONC=mr
WAYS=$(subst normal,$(SHORT_NORMAL),\
     $(subst dll,$(SHORT_DLL),\
     $(subst profiled,$(SHORT_PROF),\
     $(LIB_WAYS))))

OBJS		 = $(addsuffix .$(way_)o,  $(basename $(HS))) errors.o dumpBMP.o WndProc.o diatemp.o finalizers.o
HI_FILES         = $(addsuffix .$(way_)hi,  $(basename $(HS)))
LIBOBJS		 += $(OBJS) $(STUB_OBJS)
MKDEPENDHS_OPTS += $(foreach way,$(WAYS),-optdep-s -optdep$(way)) $(foreach obj,$(MKDEPENDHS_OBJ_SUFFICES),-optdep-o -optdep$(obj))

ifeq "$(FOR_SYSTEM)" "hugs"
# All generated C files #include errors.h to get consistent error messages
$(DLLS)		: errors.h

GUILIBS		= kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib comctl32.lib winmm.lib advapi32.lib

DLLS 		= $(addsuffix .dll, $(basename $(SRCS)))
GEN_CFILES 	= $(addsuffix .c,   $(basename $(SRCS)))
endif
GEN_GCFILES     = Win32Window.gc Win32HDC.gc


################################################################
# Programs
################################################################

RM     	= rm -f
CP	= cp
AR     	= ar
RANLIB 	= ranlib
CPP	= gcc -P -E -x c -traditional
CC	= gcc -mno-cygwin
ZIP     = zip
SED     = sed
MV      = mv

# Use GreenCard compiled with GHC or interpreted by Hugs

################################################################

GC_OPTS		= $(SRC_GC_OPTS) $(EXTRA_GC_OPTS) $($*_GC_OPTS)
CC_OPTS		= $(SRC_CC_OPTS) $(EXTRA_CC_OPTS) $($*_CC_OPTS)

ifeq "$(FOR_SYSTEM)" "ghc"
HC_OPTS		= $(SRC_HC_OPTS) $(EXTRA_HC_OPTS) $($*_HC_OPTS)
SRC_HC_OPTS     += -H10M -fglasgow-exts -fno-prune-tydecls -optc-D__GHC__=4
SRC_HC_OPTS     += -package-name win32 -package greencard
ifneq "$(way)" "dll"
SRC_HC_OPTS	+= -static
endif

Win32HDC_HC_OPTS += -cpp
Win32Registry_HC_OPTS += -Onot
ifneq "$(way)" ""
SRC_HC_OPTS     += -hisuf $(way_)hi
endif

endif

ifeq "$(LIB_OPTIMISED)" "YES"
SRC_HC_OPTS         += -O
Win32Window_HC_OPTS += -H14m
endif

ifeq "$(way)" "$(SHORT_PROFCONC)"
SRC_HC_OPTS	+= -prof -concurrent
MKDEPENDHS_OPTS += -optdep-s -optdeppc
endif

GC_TARGET	= $(FOR_SYSTEM)
ifeq "$(USING_FFI_BACKEND)" "YES"
GC_TGT		= ffi
else
GC_TGT		= $(GC_TARGET)
endif
SRC_GC_OPTS  	+= --target $(GC_TGT) --include-dir $(GCLIBDIR)
ifeq "$(DEBUG)" "YES"
SRC_GC_OPTS     += --stub-debug
SRC_CC_OPTS     += -DDEBUG -O
SRC_HC_OPTS     += -optc-DWIN32_LIB_DEBUG
endif

ifeq "$(FOR_SYSTEM)" "hugs"
GCPP_OPTS	= -DTARGET_HUGS
INCLUDES	= -I $(HUGSSRCDIR)
else
GCPP_OPTS	= -D__GLASGOW_HASKELL__
SRC_CC_OPTS     += -D__GHC__=4 $(EXTRA_CC_OPTS)
endif

ifeq "$(way)" "$(SHORT_NORMAL)"
way  :=
way_ :=
_way :=
else
ifneq "$(way)" ""
way_		  := $(way)_
_way		  := _$(way)
endif
endif


################################################################
# Suffix rules taken from the GHC users guide
################################################################

ifeq "$(FOR_SYSTEM)" "hugs"
.SUFFIXES	:
.SUFFIXES	: .gc .hs .dll .c
endif

ifeq "$(FOR_SYSTEM)" "ghc"
.SUFFIXES	: .gc .lhs .hs .hi .o .c .a
endif

ifeq "$(FOR_SYSTEM)" "ghc"
%.$(way_)hi	: %.$(way_)o
		@:

%.$(way_)o	: %.lhs
		$(GHC) $(HC_OPTS) -c $< -o $*.$(way_)o -osuf $(way_)o

%.$(way_)o	: %.hs
		$(GHC) $(HC_OPTS) -c $< -o $*.$(way_)o -osuf $(way_)o

%.o		: %.c
		$(CC) $(CC_OPTS) -c $<

%_stub.o	: %_stub.c
		@:

libHSwin32$(_way).a : $(LIBOBJS)
		$(RM) $@
		ar clqs $@ $(LIBOBJS) Win32Dialogue_stub.$(way_)o Win32Window_stub.$(way_)o
		ranlib $@

%.hs		: %.gc
		$(GC) $(GC_OPTS) $< -o $@

.PRECIOUS:	%.gc

%.gc		: %.pgc
		$(CPP) $(GCPP_OPTS) $< | perl -pe 's#\\n#\n#g' > $@

endif

ifeq "$(FOR_SYSTEM)" "hugs"

%.gc		: %.pgc
		$(CPP) $(GCPP_OPTS) $< | perl -pe 's#\\n#\n#g' > $@

%.c %.hs	: %.gc
		$(GC) $(GC_OPTS) -I . $< -o $@

%.dll		: %.c
		cl /nologo /LD /MD $(INCLUDES) $(GUILIBS) $(GCPP_OPTS) -DSTRICT -o $@ $*.c

%.obj		: %.c
		cl /nologo $(INCLUDES) $(GCPP_OPTS) -DSTRICT -o $@ $*.c
endif

# This goes first to make it the default
ifeq "$(FOR_SYSTEM)" "hugs"
all ::		$(HS) $(DLLS) 
else
ifneq "$(way)" ""
all		:: libHSwin32$(_way).a
endif
endif

#
# Dependency generation
#
depend :: $(GEN_GCFILES)

ifeq "$(FOR_SYSTEM)" "ghc"
depend :: Win32Window.hs

depend :: $(HS) $(SRCS) 
	perl mkGCDep *.gc > .depend
	$(GHC) -M -optdep-f -optdep.depend $(HC_OPTS) $(MKDEPENDHS_OPTS) $(HS)
	$(SED) -e 's#$(GCDIR)/lib/$(GC_TARGET)#$$(GCDIR)/lib/$$(GC_TARGET)#g' < .depend > .depend.fixed && $(RM) .depend && $(MV) .depend.fixed .depend
else 
depend :: $(SRCS)
	perl mkGCDep *.gc > .depend
endif

#
# Source distributions
#
SRC_DIST_DIR=win32
SRC_DIST_SNAP_NAME=win32-$(shell date +%d%m%y).zip
H_DIST_FILES=Win32Aux.h WndProc.h dumpBMP.h errors.h hugsQuit.h finalizers.h diatemp.h
C_DIST_FILES=WndProc.c dumpBMP.c errors.c finalizers.c diatemp.c
MK_DIST_FILES=Makefile mkGCDep .depend
GC_DIST_FILES=$(filter-out Win32Window.gc, $(filter-out Win32HDC.gc, $(wildcard *.gc *.pgc)))
EXTRA_DIST_FILES=dist/README.ghc dist/README.hugs dist/ghc-win32.patch

dist ::
	mkdir $(SRC_DIST_DIR)
	$(CP) $(GC_DIST_FILES) $(H_DIST_FILES) $(C_DIST_FILES) $(MK_DIST_FILES) $(SRC_DIST_DIR)
	mkdir $(SRC_DIST_DIR)/dist
	$(CP) $(EXTRA_DIST_FILES) $(SRC_DIST_DIR)/dist
	$(ZIP) -r $(SRC_DIST_SNAP_NAME) $(SRC_DIST_DIR)
#	$(RM) -rf $(SRC_DIST_DIR)

publish ::
	scp1 -v $(SRC_DIST_SNAP_NAME) sof@bathurst.dcs.gla.ac.uk:/local/ftp/pub/haskell/glasgow/green-card/

#
# Binary Hugs distributions
#
ifeq "$(FOR_SYSTEM)" "hugs"
BIN_DIST_DIR=win32
BIN_DIST_SNAP_NAME=win32-hugs-$(shell date +%d%m%y).zip
# Ship these too to make it self-contained.
STDDIS_FILES=c:/Hugs98/lib/win32/StdDIS.hs c:/Hugs98/lib/win32/StdDIS.dll
BIN_DIST_FILES=$(HS) $(DLLS) $(STDDIS_FILES)
BIN_DIST_EXTRAS=dist/README.hugs
binary-dist ::
	mkdir $(BIN_DIST_DIR)
	$(CP) $(BIN_DIST_FILES) $(BIN_DIST_EXTRAS) $(BIN_DIST_DIR)
	$(ZIP) -r $(BIN_DIST_SNAP_NAME) $(BIN_DIST_DIR)
	$(RM) -rf $(BIN_DIST_DIR)
	@echo Created binary dist: $(BIN_DIST_SNAP_NAME)

# Of limited use.
publish-binary-dist ::
	scp1 -v $(BIN_DIST_SNAP_NAME) sof@bathurst.dcs.gla.ac.uk:/local/ftp/pub/haskell/glasgow/green-card/

endif

#
# Binary ghc distributions
#
ifeq "$(FOR_SYSTEM)" "ghc"
GhcVersion=$(shell $(GHC) --version 2>&1 | sed -e "s/.*version \([^,]*\).*/\1/g" )
BIN_DIST_TOP_DIR=lib
BIN_DIST_LIB_DIR=$(BIN_DIST_TOP_DIR)/i386-unknown-cygwin32/ghc-$(GhcVersion)

ifeq "$(GhcVersion)" "2.10"
BIN_DIST_HI_DIR=$(BIN_DIST_LIB_DIR)/hslibs/win32/imports
else
BIN_DIST_HI_DIR=$(BIN_DIST_LIB_DIR)/imports/win32
endif

BIN_DIST_SNAP_NAME=win32-ghc-$(shell date +%d%m%y).zip

# Ship these too to make it self-contained.
BIN_DIST_ARCHIVE=libHSwin32$(_way).a
BIN_DIST_HI_FILES=$(HI_FILES) $(GCLIBDIR)/StdDIS.$(way_)hi
BIN_DIST_EXTRAS=dist/README.ghc dist/ghc-win32.patch

ifneq "$(way)" ""
binary-dist ::
	mkdir -p $(BIN_DIST_HI_DIR)
	$(CP) $(BIN_DIST_HI_FILES) $(BIN_DIST_HI_DIR)
	$(CP) $(BIN_DIST_ARCHIVE)  $(BIN_DIST_LIB_DIR)
endif
endif

#
# Clean up
#
ifeq "$(FOR_SYSTEM)" "hugs"
clean		::
		rm -f *.obj *.exp *.lib 
		rm -f $(GEN_CFILES) $(HS) $(DLLS) $(GEN_GCFILES)
else
veryclean	:: clean
		rm -f $(HS)
clean		::
		rm -f *.$(way_)o *.$(way_)hi $(GEN_GCFILES) libHSwin32$(_way).a $(HS)
endif

#
# Way management - GHC specific magic.
#
ifeq "$(FOR_SYSTEM)" "ghc"
ifeq "$(way)" ""
__FILE_SUFFIXES := o hi

WAY_TARGETS     = $(foreach way,$(WAYS),$(foreach suffix, $(__FILE_SUFFIXES), %.$(way)_$(suffix)))
LIB_WAY_TARGETS = $(foreach way,$(filter-out n, $(WAYS)),libHSwin32_$(way).a)

$(WAY_TARGETS) :
	$(MAKE) way=$(basename $(subst _,.,$(subst .,,$(suffix $@)))) $@

$(LIB_WAY_TARGETS) :
	$(MAKE) $(MFLAGS) $@ way=$(subst .,,$(suffix $(subst _,.,$(basename $@))))

endif

ifeq "$(way)" "dll"
IMPLIBS=-luser32 -lgdi32 -ladvapi32

HSwin32.dll : libHSwin32_dll.a
	$(GHC) $(HC_OPTS) --mk-dll -o HSwin32.dll -optdll--export-all -optdll--output-def=HSwin32.def Win32Dialogue_stub.o Win32Window_stub.o libHSwin32_dll.a $(IMPLIBS)

all :: HSwin32.dll

endif

ifneq "$(WAYS)" ""
ifeq "$(way)" ""
all clean install binary-dist ::
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(WAYS) ; do \
	  $(MAKE) way=$$i $(MFLAGS) $@ ; \
	  if [ $$? -eq 0 ] ; then true; else exit $$x_on_err; fi; \
	done

binary-dist ::
	$(CP) $(BIN_DIST_EXTRAS) $(BIN_DIST_TOP_DIR)
	$(ZIP) -r $(BIN_DIST_SNAP_NAME) $(BIN_DIST_TOP_DIR)

endif
endif
endif

# Hard-wired dependencies

ifeq "$(FOR_SYSTEM)" "hugs"
Win32Window.dll: WndProc.c
else
errors.o  : errors.c
dumpBMP.o : dumpBMP.c
WndProc.o : WndProc.c WndProc.h
endif

-include .depend
