#
# Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)
#

include $(TOP)/config.mk

MAIN		= $(UPKG).hs
LIBRARY 	= libHS$(PKG).a
GHCI_LIBRARY 	= HS$(PKG).o
OBJS		= $(UPKG).o $(UPKG)/*.o

HC_OPTS		= -package-name $(PKG)
HC_OPTS		+= -O -Wall -Werror -fno-warn-missing-signatures $(GHC_EXTRA_OPTS)

CLEANS 		+= $(LIBRARY) $(GHCI_LIBRARY)
CLEAN_FILES     += *.conf.inplace* *.conf.*.old *.conf.in *.h *.in

.PHONY: clean all alt_objs inplace-pkg-conf happy banner

all : $(LIBRARY) inplace-pkg-conf $(PKG).conf.in

# libraries
$(LIBRARY): banner $(COBJ) $(XOBJ) $(YOBJ) objs
	@$(RM) -f $@
	@$(AR) cq $@ $(OBJS)
	@$(RANLIB) $@

banner:
	@echo "=========== building $(PKG) ============="

# happy files
$(YOBJ): $(YSRC)
	$(HAPPY) $(HAPPY_OPTS) -o $@ $(YSRC)

# alex files
$(XOBJ): $(XSRC)
	$(ALEX) $(ALEX_OPTS) -o $@ $(XSRC)

# objects
objs::
	$(GHC) $(HC_OPTS) --make -no-hs-main -no-link $(MAIN)

$(COBJ): $(CSRC)
	$(GHC) -c $(CSRC) -o $@

# package.confs and friends
# ghc-6.2.2 needs TOP as env var.
inplace-pkg-conf: $(LIBRARY)
	@rm -f $(GHCI_LIBRARY)
	@cpp -DTOP=$(TOP) -DGLASGOW_HASKELL=$(GLASGOW_HASKELL) -DCABAL=$(CABAL) -undef < $(PKG).conf.in.cpp |  sed -e 's/""//g' -e 's/\[ *,/[ /g' -e '/^#/d' > $(PKG).conf.inplace.in
	@(cd $(TOP) ;\
	 if [ ! -f plugins.conf.inplace ]; then echo [] > plugins.conf.inplace; fi;\
	 env TOP=$(TOP) $(GHC_PKG) -g -f plugins.conf.inplace -u < src/$(PKG)/$(PKG).conf.inplace.in)

# installation pkg.confs
$(PKG).conf.in : $(PKG).conf.in.cpp
	@cpp -DLIBDIR=$(LIBDIR) -DGLASGOW_HASKELL=$(GLASGOW_HASKELL) -DCABAL=$(CABAL) -DINSTALLING -Uunix < $(PKG).conf.in.cpp | sed -e 's/""//g' -e 's/\[ *,/[ /g' -e '/^#/d' > $@

#
# todo. need to re-ranlib the library
#
.PHONY: install install-me
install-me:
	$(INSTALL_DATA_DIR) $(LIBDIR)/imports/$(UPKG)
	@for i in $(TOP)/src/$(PKG)/*.hi ; do \
		echo $(INSTALL_DATA) $$i $(LIBDIR)/imports/ ; \
		$(INSTALL_DATA) $$i $(LIBDIR)/imports/ ; \
	done
	@for i in $(TOP)/src/$(PKG)/$(UPKG)/*.hi ; do \
		echo $(INSTALL_DATA) $$i $(LIBDIR)/imports/$(UPKG)/ ; \
		$(INSTALL_DATA) $$i $(LIBDIR)/imports/$(UPKG)/ ; \
	done
	$(INSTALL_DATA) $(TOP)/src/$(PKG)/libHS$(PKG).a  $(LIBDIR)
	$(RANLIB) $(LIBDIR)/libHS$(PKG).a
	$(INSTALL_DATA) $(TOP)/src/$(PKG)/HS$(PKG).o     $(LIBDIR)
	$(INSTALL_DATA) $(TOP)/src/$(PKG)/$(PKG).conf.in $(LIBDIR)

clean:
	rm -f $(CLEAN_FILES)
	find . -name '*.a' -exec rm {} \;
	find . -name '*.in' -exec rm {} \;
	find . -name '*~' -exec rm {} \;
	find . -name 'a.out' -exec rm {} \;
	find . -name '*.hi' -exec rm {} \;
	find . -name '*.o' -exec rm {} \;
	find . -name '*.old' -exec rm {} \;
	find . -name '*.core' -exec rm {} \;
	find . -name '*_stub.c' -exec rm {} \;
	find . -name '*_stub.h' -exec rm {} \;

