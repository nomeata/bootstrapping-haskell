TOP=../..
include $(TOP)/mk/boilerplate.mk

C_SRCS  = $(wildcard *.c)
H_SRCS  = $(wildcard *.h)
C_PROG 	= hp2ps

INSTALL_PROGS += $(C_PROG)

SRC_CC_OPTS += -O
LIBS	     = -lm

CLEAN_FILES += $(C_OBJS) $(C_PROG)

include $(TOP)/mk/target.mk
