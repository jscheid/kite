
#  Makefile -- for Kite, a WebKit inspector front-end
#
#  Copyright (C) 2012 Julian Scheid
#
#  This file is not part of GNU Emacs.
#
#  Kite is free software: you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Kite is distributed in the hope that it will be useful, but WITHOUT
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
#  License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Kite.  If not, see <http://www.gnu.org/licenses/>.
#

all: byte-compile

ELISP_SOURCES = \
	kite-breakpoint.el	\
	kite-color.el		\
	kite-console.el		\
	kite-debug.el		\
	kite-dom-css.el		\
	kite-dom.el		\
	kite-files.el		\
	kite-global.el		\
	kite-memory.el		\
	kite-modeline.el	\
	kite-net2.el		\
	kite-object.el		\
	kite-scratch.el		\
	kite-sourcemap.el	\
	kite-util.el		\
	kite.el

ELC = $(ELISP_SOURCES:.el=.elc)

EL_DEPS = $(ELISP_SOURCES:.el=.P)

kite.html: kite.md
	pandoc -s -S --toc -c kite.css -o $@ $<

kite.info: kite.texinfo
	makeinfo $<

kite.texinfo: kite.md
	pandoc -t texinfo -o $@ $<

dir: kite.info
	install-info $< $@

kite.tar.gz: $(ELISP_SOURCES) dir kite.info
	tar czf $@ $^

clean:
	rm -f $(ELC) $(EL_DEPS) kite.html kite.info kite.texinfo dir kite.tar.gz

test:
	$(EMACS) $(BATCH) $(DEPS)  -l ert -l kite-tests.el -f ert-run-tests-batch-and-exit

# make rules used during development

EMACS = emacs
BATCH = -batch -q -no-site-file
DEPS = -l ./kite-load-path.el
COMPILE = -f batch-byte-compile
SHELL = bash
SED = sed
XARGS = xargs

# How to compile
%.elc: %.el %.P
	$(EMACS) $(BATCH) $(DEPS) $(COMPILE) $<

byte-compile: $(ELC)

# Automatic dependencies

.DELETE_ON_ERROR:
%.P: %.el
	set -o pipefail; \
	(echo "$<c:" \
	    && $(SED) -n "s/.*(require '\(kite[a-z-]*\)).*/\1.elc/p" < $^) \
	    | $(XARGS) > $@

-include $(EL_DEPS)
