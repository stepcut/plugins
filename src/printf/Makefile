PKG	= printf
UPKG	= Printf

YOBJ	= $(UPKG)/Parser.hs
YSRC	= $(UPKG)/Parser.y

XOBJ	= $(UPKG)/Lexer.hs
XSRC	= $(UPKG)/Lexer.x

TOP=../..
include ../build.mk

HC_OPTS		+= -package-conf $(TOP)/plugins.conf.inplace 
HC_OPTS		+= -package eval

install: install-me
