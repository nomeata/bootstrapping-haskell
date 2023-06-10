#
# Makefile for hslibs subdir
#
TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS=cbits

HS_SRCS= $(wildcard src/*.hs)
LIBRARY=libHShbc$(_way).a
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.hs=.$(way_)hi)

SRC_HC_OPTS += -recomp -isrc -cpp -fvia-C -fglasgow-exts -dcore-lint -Rghc-timing

#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#
override datadir:=$(libdir)/hslibs/hbc/imports

#
# Files to install from here
# 
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

include $(TOP)/mk/target.mk

#
# Specific flags
#
src/ByteOps_HC_OPTS    = '-\#include"cbits/ByteOps.h"'
