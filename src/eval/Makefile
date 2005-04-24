PKG	= eval
UPKG	= Eval

TOP=../..
include ../build.mk

HC_OPTS		+= -package-conf $(TOP)/plugins.conf.inplace 
HC_OPTS		+= -package plugins 

GHC6_3_HC_OPTS  += -package template-haskell

install: install-me
