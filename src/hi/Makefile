PKG	= hi
UPKG	= Hi

CSRC  = Language/$(UPKG)/hschooks.c
COBJ  = Language/$(UPKG)/hschooks.o

ALL_SRCS=$(wildcard $(patsubst ./%, %, $(patsubst %, %/*.hs, Language Language/$(UPKG))))

TOP=../..
include ../build.mk

HC_OPTS	+= -ILanguage/$(UPKG)

install: install-me

-include depend
