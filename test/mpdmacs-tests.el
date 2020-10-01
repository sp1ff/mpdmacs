;;; mpdmacs-tests.el --- ERT tests for mpdmacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)

(ert-deftest mpdmacs-test-smoke-test ()
  "Smoke test--load `mpdmacs' & check that no connection is made &c."

  (require 'mpdmacs)

  ;; Haven't called `mpdmacs-enable', so should have connection. No
  ;; hooks should be run & no keybindings changed &c.
  (should (not mpdmacs--connection))
  (should (not (where-is-internal #'mpdmacs-next)))
  (should-error (mpdmacs-next :type 'void-variable)))

(ert-deftest mpdmacs-test-updates ()
  "Test updating functionality."

  (let* ((elmpd-log-level 'debug)
         (elmpd-log-buffer-name "*mpdmacs-test-updates*")
         (msgs '(("currentsong\n" . "foobarsplat
Artist: De-Phazz
Title: Plastic Love Memory (No. Nine)
Track: 9
Date: 2002
OK
")
                 ("listplaylists\n" . "playlist: foo
Last-Modified: 2020-03-13T17:20:16Z
playlist: saturday-afternoons-in-santa-cruz
Last-Modified: 2020-09-20T15:57:37Z
OK
")
                 ("status\n" . "repeat: 0
random: 1
single: 0
consume: 1
playlist: 1002
playlistlength: 367
mixrampdb: 0.000000
state: stop
xfade: 5
song: 115
songid: 6588
nextsong: 137
nextsongid: 6615
OK
")
                 ("idle player stored_playlist options mixer\n" . "")
                 ("noidle\n" . "OK\n")
                 ("replay_gain_status\n" . "replay_gain_mode: off
OK
")
                 ("idle player stored_playlist options mixer\n" . "")))
         (server
            (make-network-process
             :name "mpdmacs-test-updates"
             :server t
             :host 'local
             :service t
             :family 'ipv4
             :sentinel
             (lambda (proc event)
               (if (and (> (length event) 8)
	                      (string= (substring event 0 9) "open from"))
                   (process-send-string proc "OK MPD 256.256.256\n")))
             :filter
             (lambda (proc text)
               (let ((in (caar msgs))
                     (out (cdar msgs)))
                 (setq msgs (cdr msgs))
                 (should (string= in text))
                 (process-send-string proc out)))))
         (mpdmacs-port (process-contact server :service))
         (mpdmacs-host "localhost")
         (mpdmacs-socket nil))

    (require 'mpdmacs)
    (mpdmacs-enable)
    (sit-for 1)
    (while (accept-process-output))
    (delete-process server)
    (should (not (process-live-p server)))
    (message "\n%s\n" (with-current-buffer "*mpdmacs-test-updates*" (buffer-string)))

    (should (string= mpdmacs--current-song-label "De-Phazz - Plastic Love Memory (No. Nine)"))
    (should (equal mpdmacs--stored-playlists '("foo" "saturday-afternoons-in-santa-cruz")))))

(provide 'mpdmacs-tests)

;;; mpdmacs-tests.el ends here
