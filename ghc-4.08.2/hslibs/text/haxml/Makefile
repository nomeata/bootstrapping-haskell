SOFTWARE = HaXml
VERSION  = 0.9

# Important: if you wish to use HaXml with the Hugs interpreter, you must
# pre-process the source files with cpp before loading them.  Use the
# "make forHugs" or "make cpp" targets to do this automatically.  If you
# ever want to revert to the original versions, use "make uncpp".

# Note, if you are using an old version of Hugs, you might need to
# remove the -D__HASKELL98__ from this CPP line.
CPP = gcc -E -D__HASKELL98__ -D__HUGS__ -x c

HSFILES  = lib/HPutStrLn.hs lib/IsPrefixOf.hs lib/IsSuffixOf.hs \
           lib/XmlCombinators.hs lib/XmlHtmlGen.hs \
           lib/XmlLex.hs lib/XmlHtmlPP.hs lib/XmlPP.hs \
           lib/XmlHtmlParse.hs lib/ParseSTLib.hs lib/XmlParse.hs \
           lib/Pretty.lhs lib/SymTab.hs lib/XmlTypes.hs
LIBHS    = lib/Xml2Haskell.hs lib/XmlLib.hs lib/Haskell2Xml.hs
TOOLHS   = tools/DtdToHaskell.hs tools/DtdToTypeDefPP.hs \
	   tools/Xtract.hs tools/XtractLex.hs tools/XtractParse.hs
DRIFTHS  = tools/DrIFT/*.hs tools/DrIFT/*.lhs
DRIFTAUX = tools/DrIFT/Makefile tools/DrIFT/README tools/DrIFT/example
AUX      = Makefile docs/* examples/* README

ALLHS    = $(HSFILES) $(LIBHS) $(TOOLHS) $(DRIFTHS)
ALLFILES = $(ALLHS) $(DRIFTAUX) $(AUX)

ifeq "${HC}" "nhc98"
  INC = -I../lib
else
  INC = -I../lib -i../lib
endif

all: DtdToHaskell Canonicalise Xtract DrIFT
libs: XmlLib Haskell2Xml Xml2Haskell
forHugs: cpp


# library APIs

XmlLib: $(HSFILES) $(LIBHS)
	cd lib; hmake XmlLib.hs
Haskell2Xml: $(HSFILES) $(LIBHS)
	cd lib; hmake Haskell2Xml.hs
Xml2Haskell: $(HSFILES) $(LIBHS)
	cd lib; hmake Xml2Haskell.hs


# standalone tools

DtdToHaskell: $(HSFILES) $(LIBHS) $(TOOLHS)
	cd tools; hmake DtdToHaskell $(INC)
	mv tools/DtdToHaskell .

Canonicalise: $(HSFILES) $(LIBHS) examples/Canonicalise.hs
	cd examples; hmake Canonicalise $(INC)
	mv examples/Canonicalise .

Xtract: $(HSFILES) $(LIBHS) $(TOOLHS)
	cd tools; hmake Xtract $(INC)
	mv tools/Xtract .

DrIFT: $(DRIFTHS)
	cd tools/DrIFT; hmake DrIFT
	mv tools/DrIFT/DrIFT .


# packaging a distribution

srcDist: $(ALLFILES)
	rm -f $(SOFTWARE)-$(VERSION).tar $(SOFTWARE)-$(VERSION).tar.gz
	mkdir $(SOFTWARE)-$(VERSION)
	tar cf - $(ALLFILES) | ( cd $(SOFTWARE)-$(VERSION); tar xf - )
	tar cf $(SOFTWARE)-$(VERSION).tar $(SOFTWARE)-$(VERSION)
	rm -rf $(SOFTWARE)-$(VERSION)
	gzip $(SOFTWARE)-$(VERSION).tar

zipDist: $(ALLFILES)
	rm -f $(SOFTWARE)-$(VERSION).zip
	mkdir $(SOFTWARE)-$(VERSION)
	tar cf - $(ALLFILES) | ( cd $(SOFTWARE)-$(VERSION); tar xf - )
	zip -r $(SOFTWARE)-$(VERSION).zip $(SOFTWARE)-$(VERSION)
	rm -rf $(SOFTWARE)-$(VERSION)


# do pre-processing once and for all (needed only for Hugs)
CPPFILES = examples/Canonicalise.hs tools/DtdToHaskell.hs tools/Xtract.hs \
		tools/DtdToTypeDefPP.hs lib/Haskell2Xml.hs lib/IsSuffixOf.hs \
		lib/XmlLex.hs tools/XtractLex.hs lib/XmlHtmlParse.hs \
		lib/ParseSTLib.hs lib/XmlParse.hs tools/XtractParse.hs \
		lib/XmlLib.hs lib/Xml2Haskell.hs
cpp:
	for file in $(CPPFILES); do \
		mv $$file $$file.cpp; \
		$(CPP) $$file.cpp | sed -e '/^\#/d' >$$file; \
	done
	touch cpp; rm -f uncpp

# revert from pre-processed to original sources.
uncpp:
	for file in $(CPPFILES); do \
		mv $$file.cpp $$file; \
	done
	touch uncpp; rm -f cpp


# clear up rubbish
clean:
	cd lib;         rm -f *.hi *.o
	cd tools;       rm -f *.hi *.o
	cd tools/DrIFT; rm -f *.hi *.o
realclean: clean
	rm -f DtdToHaskell Canonicalise Xtract DrIFT
