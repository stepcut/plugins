PKG         = plugins
UPKG        = Plugins

PATHS       = System System/Eval System/Plugins

TOP=../..
include $(TOP)/config.mk

ifeq ($(CABAL),1)
YOBJ    = System/$(UPKG)/ParsePkgConfCabal.hs
YSRC    = System/$(UPKG)/ParsePkgConfCabal.y
OTHER   = System/$(UPKG)/ParsePkgConfLite.hs
else
YOBJ    = System/$(UPKG)/ParsePkgConfLite.hs
YSRC    = System/$(UPKG)/ParsePkgConfLite.y
OTHER   = System/$(UPKG)/ParsePkgConfCabal.hs
endif

STUBOBJS   =System/Eval/Haskell_stub.$(way_)o

ALL_SRCS= $(filter-out $(OTHER), \
                $(wildcard $(patsubst ./%, %, \
                        $(patsubst %, %/*.hs, System System/$(UPKG) System/Eval))))

include ../build.mk

HC_OPTS         += -package-conf $(TOP)/plugins.conf.inplace 
HC_OPTS         += -package altdata -package hi
HC_OPTS         += -O -funbox-strict-fields 
HC_OPTS         += -Wall -fno-warn-missing-signatures

ifeq ($(GLASGOW_HASKELL),602)
HC_OPTS         += -package posix
endif

install: install-me

-include depend
