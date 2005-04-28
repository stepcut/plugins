#
# Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)
#

MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

include $(TOP)/config.mk

# If $(way) is set then we define $(way_) and $(_way) from it in the
# obvious fashion.
ifeq "$(way)" "p"
  way_ := $(way)_
  _way := _$(way)
endif

#
# building the profiled way
#
ifeq "$(way)" "p"
PROF_OPTS	= -prof -auto-all -Icbits
LD_OPTS		+= $(PROF_OPTS)
HC_OPTS		+= $(PROF_OPTS)
HC_OPTS 	+= -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(way_)o
endif

MAIN		= $(UPKG).hs
LIBRARY 	= libHS$(PKG)$(_way).a
GHCI_LIBRARY 	= $(patsubst lib%.a,%.o,$(LIBRARY))
OBJS		= $(UPKG).o $(UPKG)/*.o

HC_OPTS		+= -package-name $(PKG)
HC_OPTS		+= -O -Wall -Werror -fno-warn-missing-signatures $(GHC_EXTRA_OPTS)
HC_OPTS         += -threaded

CLEANS 		+= $(LIBRARY) $(GHCI_LIBRARY)
CLEAN_FILES     += *.conf.inplace* *.conf.*.old *.conf.in *.h *.in

OBJS=	$(addsuffix .$(way_)o,$(basename $(ALL_SRCS)))

.PHONY: clean all alt_objs plugins.conf.inplace happy banner

all : $(LIBRARY) $(TOP)/plugins.conf.inplace $(PKG).conf.in

# libraries
$(LIBRARY): depend $(COBJ) $(XOBJ) $(YOBJ) $(OBJS)
	@$(RM) -f $@
	@$(AR) cq $@ $(OBJS) $(COBJ) $(STUBOBJS)
	@$(RANLIB) $@

$(GHCI_LIBRARY) : $(OBJS)
	$(LD_X) -r -o $@ $(OBJS) $(COBJ) $(STUBOBJS)

#
# Dependency generation
#
depend: $(ALL_SRCS)
	@echo -n "Rebuilding dependencies ... "
	@$(GHC) -cpp $(HC_OPTS) $(PKG_OPTS) $(HTOOLKIT) -M -optdep-f \
		-optdepdepend $(ALL_SRCS) || rm depend
	@echo "done."

%.$(way_)hi : %.$(way_)o
	@:

%.$(way_)o: %.hs
	$(GHC) $(HC_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi

# happy files
$(YOBJ): $(YSRC)
	$(HAPPY) $(HAPPY_OPTS) -o $@ $(YSRC)

# alex files
$(XOBJ): $(XSRC)
	$(ALEX) $(ALEX_OPTS) -o $@ $(XSRC)

$(COBJ): $(CSRC)
	$(GHC) -c $(CSRC) -o $@

# package.confs and friends
# ghc-6.2.2 needs TOP as env var.
$(TOP)/plugins.conf.inplace: $(PKG).conf.in.cpp $(LIBRARY) $(GHCI_LIBRARY)
	@cpp -DTOP=$(TOP) -DGLASGOW_HASKELL=$(GLASGOW_HASKELL) -DCABAL=$(CABAL) -undef < $(PKG).conf.in.cpp |  sed -e 's/""//g' -e 's/\[ *,/[ /g' -e '/^#/d' > $(PKG).conf.inplace.in
	@(cd $(TOP) ;\
	 if [ ! -f $(TOP)/plugins.conf.inplace ]; then echo [] > $(TOP)/plugins.conf.inplace; fi;\
	 env TOP=$(TOP) $(GHC_PKG) --force -f $@ -u < src/$(PKG)/$(PKG).conf.inplace.in)

# installation pkg.confs
$(PKG).conf.in : $(PKG).conf.in.cpp
	@cpp -DLIBDIR=$(LIBDIR) -DGLASGOW_HASKELL=$(GLASGOW_HASKELL) -DCABAL=$(CABAL) -DINSTALLING -Uunix < $(PKG).conf.in.cpp | sed -e 's/""//g' -e 's/\[ *,/[ /g' -e '/^#/d' > $@

#
# todo. need to re-ranlib the library
#
.PHONY: install install-me
install-me:
	$(INSTALL_DATA_DIR) $(LIBDIR)/imports/$(UPKG)
	@for i in $(TOP)/src/$(PKG)/*.$(way_)hi ; do \
		echo $(INSTALL_DATA) $$i $(LIBDIR)/imports/ ; \
		$(INSTALL_DATA) $$i $(LIBDIR)/imports/ ; \
	done
	@for i in $(TOP)/src/$(PKG)/$(UPKG)/*.$(way_)hi ; do \
		echo $(INSTALL_DATA) $$i $(LIBDIR)/imports/$(UPKG)/ ; \
		$(INSTALL_DATA) $$i $(LIBDIR)/imports/$(UPKG)/ ; \
	done
	$(INSTALL_DATA) $(TOP)/src/$(PKG)/libHS$(PKG)$(_way).a  $(LIBDIR)
	$(RANLIB) $(LIBDIR)/libHS$(PKG).a
	$(INSTALL_DATA) $(TOP)/src/$(PKG)/HS$(PKG).o     $(LIBDIR)
	$(INSTALL_DATA) $(TOP)/src/$(PKG)/$(PKG).conf.in $(LIBDIR)

clean:
	rm -f $(CLEAN_FILES)
	find . -name '*.a' -exec rm {} \;
	find . -name depend -exec rm {} \;
	find . -name '*.in' -exec rm {} \;
	find . -name '*~' -exec rm {} \;
	find . -name 'a.out' -exec rm {} \;
	find . -name '*.hi' -exec rm {} \;
	find . -name '*.p_hi' -exec rm {} \;
	find . -name '*.o' -exec rm {} \;
	find . -name '*.p_o' -exec rm {} \;
	find . -name '*.old' -exec rm {} \;
	find . -name '*.core' -exec rm {} \;
	find . -name '*_stub.c' -exec rm {} \;
	find . -name '*_stub.h' -exec rm {} \;

