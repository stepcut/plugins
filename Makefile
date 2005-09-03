# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)

# cut down reimplementation of $fptools/mk directory

#
# installing
#

# TODO put these in subdirs
install:
	$(INSTALL_DATA_DIR) $(LIBDIR)/include
	$(INSTALL_DATA) EvalHaskell.h $(LIBDIR)/include
	$(INSTALL_DATA_DIR) $(MANDIR)/man1
	$(INSTALL_DATA) docs/hs-plugins.1 $(MANDIR)/man1
	cd src && ./setup install

#
# regress check. TODO check expected output
# 
check:
	@if [ ! -f EvalHaskell.h ] ; then	\
		echo "run 'make' first" ;	\
		exit 1 ;\
	fi
	@( d=/tmp/plugins.tmp.$$$$ ; mkdir $$d ; export TMPDIR=$$d ;\
	   for i in `find examples ! -name CVS -type d -maxdepth 2 -mindepth 2 | sort` ; do \
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
	      config.h config.mk config.log config.status

clean:
	cd docs && $(MAKE) clean
	runhaskell Setup.hs clean 2> /dev/null || true
	rm -rf $(CLEAN_FILES)
	find examples -name '*.a' -exec rm {} \;
	find examples -name '*~' -exec rm {} \;
	find examples -name 'a.out' -exec rm {} \;
	find examples -name '*.hi' -exec rm {} \;
	find examples -name '*.o' -exec rm {} \;
	find examples -name '*.core' -exec rm {} \;
	find examples -name 'package.conf' -exec rm {} \;
	rm -f examples/makewith/io/TestIO.conf
	rm -f examples/makewith/unsafeio/Unsafe.conf
	rm -rf examples/hmake/lib-plugs/plugs
	rm -rf examples/hmake/one-shot/runplugs
	rm -f EvalHaskell.h
	rm -rf $(EXTRA_CLEANS)

-include config.mk
