dist_lisp_LISP = mpdmacs.el
SUBDIRS = test
AM_ELCFLAGS = --eval '(require (quote bytecomp))'

dist-check:
	$(EMACS) --batch --eval '(checkdoc-file "$(srcdir)/mpdmacs.el")'
