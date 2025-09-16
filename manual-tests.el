;; -*- lexical-binding: t; -*-
(toggle-debug-on-error)
(require 'mpdmacs)
(setq elmpd-log-level 'debug
      mpdmacs-host "localhost"
      mpdmacs-prot 6600
      mpdmacs-socket nil)
(mpdmacs-mode)
