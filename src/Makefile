
# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

.PHONY: all build altdata hi plugins eval  printf
.PHONY:	install i_altdata i_hi i_plugins i_eval i_printf

build: altdata hi plugins eval printf
	 
altdata:
	@cd altdata && $(MAKE)
hi:
	@cd hi && $(MAKE)
plugins: altdata hi
	@cd plugins && $(MAKE)
eval: plugins
	@cd eval && $(MAKE)
printf: plugins
	@cd printf && $(MAKE)

install: i_altdata i_hi i_plugins i_eval i_printf
	@true

i_altdata:
	@cd altdata && $(MAKE) install
i_hi:
	@cd hi      && $(MAKE) install
i_plugins:
	@cd plugins && $(MAKE) install
i_eval:
	@cd eval    && $(MAKE) install
i_printf:
	@cd printf  && $(MAKE) install

all: build

TOP=..
include build.mk
