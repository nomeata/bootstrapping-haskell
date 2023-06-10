# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.12 2000/04/02 21:03:45 andy Exp $
# (c) The GHC Team 1997-2000

TOP = ..
include $(TOP)/mk/boilerplate.mk

ifeq "$(GhcWithHscBuiltViaC)" "YES"
# just lang if we're bootstrapping
SUBDIRS = lang
else
# order is important here, there are dependencies between the libraries...
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SUBDIRS = lang concurrent posix util data text net tools
else
SUBDIRS = lang concurrent util data text net tools
endif
endif

include $(TOP)/mk/target.mk
