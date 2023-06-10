#
# Makefile for cbits subdirectory
#
TOP=../..
include $(TOP)/mk/boilerplate.mk

# No ways
override WAYS=

# Hack!
SRC_CC_OPTS += -I$(FPTOOLS_TOP)/ghc/includes

C_SRCS=$(wildcard *.c)
LIBRARY=libHShbc_cbits.a
LIBOBJS=$(C_OBJS)
INSTALL_LIBS+=$(LIBRARY)

include $(TOP)/mk/target.mk
