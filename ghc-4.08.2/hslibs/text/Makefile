# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.6.2.1 2000/05/19 11:05:07 simonmar Exp $
#
# (c) The GHC Team, 1997-1999
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

HSLIB = text
HSLIB_DEPS = lang data
HSLIB_HAS_CBITS = YES

NOT_FOR_HUGS = MatchPS.lhs Regex.lhs RegexString.lhs

SRCS_FROM_SUBDIRS = html haxml/lib parsec

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing

parsec/TokenDef_HC_OPTS = -Onot

include $(TOP)/mk/target.mk

