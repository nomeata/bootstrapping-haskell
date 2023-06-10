#################################################################################
#
#	 $Id: suffix.mk,v 1.5 1998/12/03 15:24:08 simonm Exp $
#
#		GHC-specific suffix rules
#
#################################################################################

#-----------------------------------------------------------------------------
# Ugen suffix rules. 
#
# Hack, the implicit rule assumes the ugen files
# resides in a directory parser/
#

parser/U_%.hs : parser/%.c
	@:

parser/%.h parser/%.c : parser/%.ugn
	@$(RM) $@ parser/$*.c parser/$*.hs parser/U_$*.hs parser/$*.h
	$(UGEN) $< || $(RM) parser/$*.h parser/$*.c parser/$*.hs
	@$(MV) -f parser/$*.hs parser/U_$*.hs
	@chmod 444 parser/$*.h parser/$*.c parser/U_$*.hs

