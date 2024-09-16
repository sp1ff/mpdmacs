dist_lisp_LISP = mpdmacs.el
EXTRA_DIST = README.org
CLEANFILES = .pkg-tmp mpdmacs-$(PACKAGE_VERSION).tar
SUBDIRS = doc test
AM_ELCFLAGS = --eval '(require (quote bytecomp))'

package: mpdmacs-$(PACKAGE_VERSION).tar $(srcdir)/README.org

srclisp=$(dist_lisp_LISP:%.el=$(srcdir)/%.el)

mpdmacs-$(PACKAGE_VERSION).tar: $(srclisp) $(srcdir)/README.org
	mkdir -p .pkg-tmp/mpdmacs-$(PACKAGE_VERSION)/ && \
	cp $(srclisp) .pkg-tmp/mpdmacs-$(PACKAGE_VERSION)/ && \
	cp $(srcdir)/README.org .pkg-tmp/mpdmacs-$(PACKAGE_VERSION)/ && \
	cd .pkg-tmp && tar cf $@ mpdmacs-$(PACKAGE_VERSION)/ && \
	cd .. && mv -v .pkg-tmp/mpdmacs-$(PACKAGE_VERSION).tar . && \
	rm -rf .pkg-tmp

dist-hook: package

