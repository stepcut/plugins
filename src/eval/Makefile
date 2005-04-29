PKG	= eval
UPKG	= Eval

ALL_SRCS=$(wildcard $(patsubst ./%, %, $(patsubst %, %/*.hs, . $(UPKG))))

STUBOBJS   =Eval/Haskell_stub.$(way_)o

TOP=../..
include ../build.mk

HC_OPTS		+= -package-conf $(TOP)/plugins.conf.inplace 
HC_OPTS		+= -package plugins 

GHC6_3_HC_OPTS  += -package template-haskell

install: install-me

-include depend
