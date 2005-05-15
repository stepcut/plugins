
# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

.PHONY: all build altdata hi plugins
.PHONY:	install i_altdata i_hi i_plugins

build: altdata hi plugins
	 
altdata:
	@cd altdata && $(MAKE)
hi:
	@cd hi && $(MAKE)
plugins: altdata hi
	@cd plugins && $(MAKE)

install: i_altdata i_hi i_plugins
	@true

i_altdata:
	@cd altdata && $(MAKE) install
i_hi:
	@cd hi      && $(MAKE) install
i_plugins:
	@cd plugins && $(MAKE) install

all: build

TOP=..
include build.mk
