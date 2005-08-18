# how to build the default projects

include $(TOP)/config.mk
include $(TOP)/examples/check.mk

BIN= 		prog/a.out
OBJ=		prog/Main.o
SRC=		prog/Main.hs

BINDIR=		prog
REALBIN=	./a.out

API_OBJ=	api/API.o

INCLUDES=   	-i$(TOP)/examples/$(TEST)/api
GHCFLAGS=   	-Onot -cpp -fglasgow-exts

.SUFFIXES : .o .hs .hi .lhs .hc .s

all: $(BIN)

$(BIN) : $(PRIOR_OBJS) $(API_OBJ) $(SRC) $(EXTRA_OBJS)
	@rm -f $@
	@$(GHC) --make -o $@ $(INCLUDES) $(PKGFLAGS) $(GHCFLAGS) $(EXTRAFLAGS) $(API) $(SRC)

# Standard suffix rules
.o.hi:
	@:
.hs.o:
	@$(GHC) $(INCLUDES) $(PKGFLAGS) $(GHCFLAGS) $(EXTRAFLAGS) -c $<

clean:
	find . -name '*~' -exec rm {} \;
	rm -rf *.{o,hi,dep}
	rm -rf */*.{hi,o,old} */a.out
	rm -rf */*core
	rm -rf */*.a
	rm -rf */package.conf
	rm -rf *.a

