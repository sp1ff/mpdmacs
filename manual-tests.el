(toggle-debug-on-error)
(require 'mpdmacs)
(setq elmpd-log-level 'debug
      mpdmacs-host "192.168.1.6"
      mpdmacs-prot 6600
      mpdmacs-socket nil)
(mpdmacs-mode)
