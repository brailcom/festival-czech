# Makefile for festival-czech
#
# Copyright (C) 2004, 2005, 2006 Brailcom, o.p.s.
#
# Author: Milan Zamazal <pdm@brailcom.org>
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


package := festival-czech
version := 0.2.1

festival_path = /usr/share/festival

INSTALL_PROGRAM = install

distfiles := *.scm *.out

.PHONY: all install install-strip uninstall clean distclean mostlyclean \
	maintainer-clean TAGS info dvi dist check

all: czech-lexicon.out

%.out: %.scm
	festival --batch '(lex.compile "$<" "$@")'

install: all
	$(INSTALL_PROGRAM) -m 644 $(distfiles) $(festival_path)/

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

uninstall: all
	for f in $(distfiles); do rm $(festival_path)/$$f; done

mostlyclean:

clean: mostlyclean

distclean: clean
	rm -rf $(package)-$(version) *.tar *.tar.gz

maintainer-clean: distclean
	rm -f *.out

TAGS:

info:

dvi:

dist: all distclean
	mkdir $(package)-$(version)
	cp $(distfiles) COPYING ChangeLog FAQ INSTALL* Makefile NEWS* README* $(package)-$(version)/
	make -C $(package)-$(version) all
	chmod 755 $(package)-$(version)
	chmod 644 $(package)-$(version)/*
	tar cf $(package)-$(version).tar $(package)-$(version)
	gzip -9 $(package)-$(version).tar

check:

