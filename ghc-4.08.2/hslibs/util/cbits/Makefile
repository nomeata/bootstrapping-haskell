# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.9 2000/05/12 11:48:31 rrt Exp $
#
# (c) The GHC Team, 1999
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

CC = $(HC)

SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_IO_INCLUDE_DIR)

LIBRARY=libHSutil_cbits.a
LIBOBJS=$(C_OBJS)

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
LIBOBJS:=$(filter-out selectFrom.$(way_)o, $(LIBOBJS))
endif

INSTALL_LIBS += $(LIBRARY)

# -----------------------------------------------------------------------------
# Win32 DLL stuff

ifeq "$(filter dll,$(WAYS))" "dll"
override WAYS=dll
else
override WAYS=
endif

ifneq "$(way)" "dll"
SRC_CC_OPTS += -static
endif

DLL_NAME = HSutil_cbits.dll
SRC_BLD_DLL_OPTS += --export-all --output-def=HSutil_cbits.def DllVersionInfo.o
SRC_BLD_DLL_OPTS += -lwinmm -lwsock32 -lHSrts_imp -lHSstd_cbits_imp -lgmp -L. -L$(GHC_RUNTIME_DIR)/gmp -L$(GHC_RUNTIME_DIR) -L$(GHC_IO_INCLUDE_DIR)

ifeq "$(way)" "dll"
all :: DllVersionInfo.o
endif

ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS += $(DLL_NAME)
ifneq "$(way)" "dll"
INSTALL_LIBS += $(patsubst %.a, %_imp.a, $(LIBRARY))
endif
endif

# -----------------------------------------------------------------------------=

include $(TOP)/mk/target.mk
