PKG			= altdata
UPKG			= AltData
PATHS                   = AltData

ALL_SRCS=$(wildcard $(patsubst ./%, %, $(patsubst %, %/*.hs, . $(UPKG))))

TOP=../..
include ../build.mk

install: install-me

-include depend
