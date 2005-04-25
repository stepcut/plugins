PKG         = plugins
UPKG        = Plugins

TOP=../..
include $(TOP)/config.mk

ifeq ($(CABAL),1)
YOBJ    = $(UPKG)/ParsePkgConfCabal.hs
YSRC    = $(UPKG)/ParsePkgConfCabal.y
else
YOBJ    = $(UPKG)/ParsePkgConfLite.hs
YSRC    = $(UPKG)/ParsePkgConfLite.y
endif

include ../build.mk

HC_OPTS         += -package-conf $(TOP)/plugins.conf.inplace 
HC_OPTS         += -package altdata -package hi
HC_OPTS         += -O -funbox-strict-fields 
HC_OPTS         += -Wall -fno-warn-missing-signatures

install: install-me
