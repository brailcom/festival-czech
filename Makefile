# Makefile for festival-czech
#
# Copyright (C) 2004 Brailcom, o.p.s.
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.


.PHONY: all install install-strip uninstall clean distclean mostlyclean \
	maintainer-clean TAGS info dvi dist check

all: czech-lexicon.out

%.out: %.scm
	festival --batch '(lex.compile "$<" "$@")'

czech-words-all: czech-words
	./gen-all-words.sh

install:

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

uninstall:

mostlyclean:

clean: mostlyclean

distclean: clean

maintainer-clean: distclean
	rm -f *.out

TAGS:

info:

dvi:

dist:

check:

