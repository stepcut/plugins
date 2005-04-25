# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)

# cut down reimplementation of $fptools/mk directory

MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

.PHONY: build all

all: build headers

build:
	cd src && $(MAKE)

headers: build
	cp src/eval/Eval/Haskell_stub.h EvalHaskell.h

#
# installing
#

# TODO put these in subdirs
install:
	$(INSTALL_DATA_DIR) $(LIBDIR)/include
	$(INSTALL_DATA) EvalHaskell.h $(LIBDIR)/include
	@(cd src && $(MAKE) install)

#
# and register the library with ghc package system
# Use this target if installing by hand. May need to be performed as root
#
register:
	env LIBDIR=${LIBDIR} $(GHC_PKG) -u < src/altdata/altdata.conf.in
	env LIBDIR=${LIBDIR} $(GHC_PKG) -u < src/hi/hi.conf.in
	env LIBDIR=${LIBDIR} $(GHC_PKG) -u < src/plugins/plugins.conf.in
	env LIBDIR=${LIBDIR} $(GHC_PKG) -u < src/eval/eval.conf.in
	env LIBDIR=${LIBDIR} $(GHC_PKG) -u < src/printf/printf.conf.in

# and unregister the packages
unregister:
	$(GHC_PKG) -r printf
	$(GHC_PKG) -r eval
	$(GHC_PKG) -r plugins
	$(GHC_PKG) -r hi
	$(GHC_PKG) -r altdata

#
# regress check. TODO check expected output
# 
check:
	@if [ ! -f EvalHaskell.h ] ; then	\
		echo "run 'make' first" ;	\
		exit 1 ;\
	fi
	@( d=/tmp/plugins.tmp.$$$$ ; mkdir $$d ; export TMPDIR=$$d ;\
	   for i in `find examples ! -name CVS -type d -maxdepth 2 -mindepth 2` ; do \
		printf "=== testing %-50s ... " "$$i" ;	\
		( cd $$i ; if [ -f dont_test ] ; then \
		 	echo "ignored."	;\
		  else ${MAKE} -sk && ${MAKE} -ksi check |\
			sed '/^Compil/d;/^Load/d;/Read/d;/Expan/d;/Savi/d;/Writ/d' ;\
		       ${MAKE} -sk clean ;\
		  fi ) 2> /dev/null ;\
	   done ; rm -rf $$d )
	

#
# making clean
#

CLEAN_FILES += *.conf.*.old *~
EXTRA_CLEANS+=*.conf.inplace* *.conf.in *.h autom4te.cache \
	      config.h config.mk config.log config.status configure

clean:
	cd docs && $(MAKE) clean
	cd src  && $(MAKE) clean
	rm -rf $(CLEAN_FILES)
	find examples -name '*.a' -exec rm {} \;
	find examples -name '*~' -exec rm {} \;
	find examples -name 'a.out' -exec rm {} \;
	find examples -name '*.hi' -exec rm {} \;
	find examples -name '*.o' -exec rm {} \;
	find examples -name '*.core' -exec rm {} \;
	find examples -name 'package.conf' -exec rm {} \;
	rm -rf examples/hmake/lib-plugs/plugs
	rm -rf examples/hmake/one-shot/runplugs
	rm -f EvalHaskell.h

distclean: clean
	rm -rf $(EXTRA_CLEANS)

include config.mk
