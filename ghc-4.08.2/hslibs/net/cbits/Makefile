# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.5.2.2 2000/08/24 11:04:15 rrt Exp $
#
# (c) The GHC Team, 1999
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

CC = $(HC)

ifeq "$(EnableWin32DLLs)" "YES"
  C_SRCS := $(filter-out selectFrom.c,$(C_SRCS))
endif

SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_IO_INCLUDE_DIR)

LIBRARY=libHSnet_cbits$(_way).a
LIBOBJS=$(C_OBJS)

INSTALL_LIBS = $(LIBRARY)

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

ifeq "$(way)" "dll"
DLL_NAME = HSnet_cbits.dll
DLL_IMPLIB_NAME = libHSnet_cbits_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HSnet_cbits.def DllVersionInfo.o
SRC_BLD_DLL_OPTS += -lwinmm -lwsock32 -lHSrts_imp -lHSstd_cbits_imp -lgmp -L. -L$(GHC_RUNTIME_DIR)/gmp -L$(GHC_RUNTIME_DIR) -L$(GHC_IO_INCLUDE_DIR)

all :: DllVersionInfo.o
endif

ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS += $(DLL_NAME)
ifneq "$(way)" "dll"
INSTALL_LIBS += $(patsubst %.a, %_imp.a, $(LIBRARY))
endif
endif

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
