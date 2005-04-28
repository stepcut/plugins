PKG	= hi
UPKG	= Hi

CSRC  = $(UPKG)/hschooks.c
COBJ  = $(UPKG)/hschooks.o

ALL_SRCS=$(wildcard $(patsubst ./%, %, $(patsubst %, %/*.hs, . $(UPKG))))

TOP=../..
include ../build.mk

HC_OPTS	+= -I$(UPKG)

install: install-me

-include depend
