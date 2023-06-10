#################################################################################
#
#			    suffix.mk
#
#		Suffix rules for fptools
#
#################################################################################

# 
# This file contain the default suffix rules for all the fptools projects.
#


# No need to define .SUFFIXES because we don't use any suffix rules
# Instead we use gmake's pattern rules exlusively

.SUFFIXES:

# However, if $(way) is set then we have to define $(way_) and $(_way)
# from it in the obvious fashion.
# This must be done here (or earlier), but not in target.mk with the other
# way management, because the pattern rules in this file take a snapshot of
# the value of $(way_) and $(_way), and it's no good setting them later!

ifneq "$(way)" ""
  way_ := $(way)_
  _way := _$(way)
endif

#-----------------------------------------------------------------------------
# Haskell Suffix Rules

%.$(way_)o : %.hs
	$(HC_PRE_OPTS)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HC_POST_OPTS)
			 
%.$(way_)o : %.lhs	 
	$(HC_PRE_OPTS)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HC_POST_OPTS)
			 
%.$(way_)hc : %.lhs	 
	$(RM) $@
	$(HC) $(HC_OPTS) -C $< -o $@
			 
%.$(way_)hc : %.hs	 
	$(RM) $@
	$(HC) $(HC_OPTS) -C $< -o $@
			 
%.$(way_)o : %.$(way_)hc 
	$(HC_PRE_OPTS)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HC_POST_OPTS)

%.$(way_)o : %.hc 
	$(HC_PRE_OPTS)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HC_POST_OPTS)

%.$(way_)s : %.$(way_)hc 
	$(HC_PRE_OPTS)
	$(HC) $(HC_OPTS) -S $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HC_POST_OPTS)

%.$(way_)hc : %.lhc
	@$(RM) $@
	$(UNLIT) $< $@
	@chmod 444 $@


# Here's an interesting rule!
# The .hi file depends on the .o file,
# so if the .hi file is dated earlier than the .o file (commonly the case,
# when interfaces are stable) this rule just makes sure that the .o file,
# is up to date.  Then it does nothing to generate the .hi file from the 
# .o file, because the act of making sure the .o file is up to date also
# updates the .hi file (if necessary).

%.$(way_)hi : %.$(way_)o
	@if [ ! -f $@ ] ; then \
	    echo Panic! $< exists, but $@ does not. \
	    exit 1; \
	else exit 0 ; \
	fi							

%.$(way_)hi : %.$(way_)hc
	@if [ ! -f $@ ] ; then \
	    echo Panic! $< exists, but $@ does not. \
	    exit 1; \
	else exit 0 ; \
	fi

#-----------------------------------------------------------------------------
# Happy Suffix Rules
#
.PRECIOUS: %.hs

%.hs : %.ly
	$(HAPPY) $(HAPPY_OPTS) $<

#-----------------------------------------------------------------------------
# Lx Suffix Rules
#

%.hs : %.lx
	$(LX) $(LX_OPTS) $<

#-----------------------------------------------------------------------------
# Green-card Suffix Rules
#

%.hs : %.gc
	$(GREENCARD) $(GREENCARD_OPTS) $< -o $@

%.lhs : %.gc
	$(GREENCARD) $(GREENCARD_OPTS) $< -o $@

#-----------------------------------------------------------------------------
# C-related suffix rules

%.$(way_)o : %.$(way_)s
	@$(RM) $@
	$(AS) $(AS_OPTS) -o $@ $< || ( $(RM) $@ && exit 1 )

%.$(way_)o : %.c
	@$(RM) $@
	$(CC) $(CC_OPTS) -c $< -o $@

%.$(way_)o : %.S
	@$(RM) $@
	$(CC) $(CC_OPTS) -c $< -o $@

#%.$(way_)s : %.c
#	@$(RM) $@
#	$(CC) $(CC_OPTS) -S $< -o $@

%.c : %.flex
	@$(RM) $@
	$(FLEX) -t $(FLEX_OPTS) $< > $@ || ( $(RM) $@ && exit 1 )
%.c : %.lex
	@$(RM) $@
	$(FLEX) -t $(FLEX_OPTS) $< > $@ || ( $(RM) $@ && exit 1 )

# stubs are automatically generated and compiled by GHC
%_stub.$(way_)o: %.o
	@:

#-----------------------------------------------------------------------------
# Yacc stuff

%.tab.c %.tab.h : %.y
	@$(RM) $*.tab.h $*.tab.c y.tab.c y.tab.h y.output
	$(YACC) $(YACC_OPTS) $<
	$(MV) y.tab.c $*.tab.c
	@chmod 444 $*.tab.c
	$(MV) y.tab.h $*.tab.h
	@chmod 444 $*.tab.h


#-----------------------------------------------------------------------------
# Runtest rules for calling $(HC) on a single-file Haskell program

%.runtest : %.hs
	$(TIME) $(RUNTEST) $(HC) $(RUNTEST_OPTS) $<

#-----------------------------------------------------------------------------
# Doc processing suffix rules
#
# ToDo: make these more robust
#
%.ps : %.dvi
	@$(RM) $@
	dvips $< -o $@

%.tex : %.tib
	@$(RM) $*.tex $*.verb-t.tex
	$(TIB) $*.tib
	expand $*.tib-t.tex | $(VERBATIM) > $*.tex
	@$(RM) $*.tib-t.tex

%.ps : %.fig
	@$(RM) $@
	fig2dev -L ps $< $@

%.tex : %.fig
	@$(RM) $@
	fig2dev -L latex $< $@

#-----------------------------------------------------------------------------
# SGML suffix rules
#
%.sgml : %.vsgml
	@$(RM) $@
	expand $< | $(SGMLVERB) > $@

%.dvi : %.sgml
	@$(RM) $@
	$(SGML2DVI) $(SGML2DVI_OPTS) $<

%.ps : %.sgml
	@$(RM) $@
	$(SGML2PS) $(SGML2PS_OPTS) $<

%.html : %.sgml
	@$(RM) $@
	$(SGML2HTML) $(SGML2HTML_OPTS) $<

%.ps : %.sgml
	@$(RM) $@
	$(SGML2PS) $(SGML2PS_OPTS) $<

%.rtf : %.sgml
	@$(RM) $@
	$(SGML2RTF) $(SGML2RTF_OPTS) $<

#-----------------------------------------------------------------------------
# Literate suffix rules

%.prl : %.lprl
	@$(RM) $@
	$(UNLIT) $(UNLIT_OPTS) $< $@
	@chmod 444 $@

%.c : %.lc
	@$(RM) $@
	$(UNLIT) $(UNLIT_OPTS) $< $@
	@chmod 444 $@

%.h : %.lh
	@$(RM) $@
	$(UNLIT) $(UNLIT_OPTS) $< $@
	@chmod 444 $@

#-----------------------------------------------------------------------------
# Win32 resource files
#
# The default is to use the GNU resource compiler.
#

%.o : %.rc
	@$(RM) $@
	windres $< $@
