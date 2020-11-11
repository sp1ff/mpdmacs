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
                 (idle "player" "stored_playlist" "options" "mixer" "sticker")
                 ("noidle\n" . "OK\n")
                 ("replay_gain_status\n" . "replay_gain_mode: off
OK
")
                 (idle "player" "stored_playlist" "options" "mixer" "sticker")))
         (idle-state nil)
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
               ;; This is touchy: generally, when our client issues a
               ;; command, it will be a single line, and the client
               ;; will not do anything else until we have processed
               ;; that command & written the response.

               ;; There are two special cases where that does not
               ;; hold:

               ;; 1. the "idle" command: the client expects no
               ;; response, and may follow up with a "noidle" quickly
               ;; enough that it appears in the same message to
               ;; us. I.e. `text' could look like "idle foo bar
               ;; splat\nnoidle\n". Nb. the client will never send
               ;; anything more than that, since it expects an OK in
               ;; response to the noidle command.

               ;; 2. command lists: this is unlikely, but depending on the
               ;; underlying transport and its configuration, if the client
               ;; issues a lengthy command list, it may show up here in pieces; i.e.
               ;; `text' could be something like "command_list_begin\nfoo\ba",
               ;; followed by "r\nspla" and finally "t\nend_command_list\n". This
               ;; method would have to recognize that it's accumulating a lengthy
               ;; command list & only act when it's complete.
               ;;
               ;; Now that I think about this some more, this is true
               ;; of *any* command if it's long enough relative to the
               ;; underlying transport-- I could probably construct a
               ;; socket with a sufficiently pathological
               ;; configuration that "idle player stored_playlist
               ;; options mixer sticker\n" would be broken up into
               ;; multiple messages.

               ;; WARNING: This logic is a first-pass
               ;; implementation. It is needlessly complex & not
               ;; well-tested.
               (let ((in (caar msgs))
                     (out (cdar msgs)))
                 (if (and (symbolp in) (eq in 'idle))
                     (if idle-state
                         ;; We've seen the initial "idle" & it was
                         ;; fine-- now we're expecting a "noidle".
                         (progn
                           (should (string= "noidle\n" text))
                           ;; We're done-- advance our state.
                           (setq idle-state t
                                 msgs (cdr msgs)))
                       ;; We're expecting an "idle" message (to which we
                       ;; produce no response), and the subsequent
                       ;; "noidle" message may or not be waiting for us
                       ;; in `text' as well.
                       (let* ((lines (split-string text "\n" t))
                              (line (car lines)))
                         (should
                          (string= (concat "idle " (mapconcat 'identity out " ")) line))
                         ;; If we're here, we got the expected "idle"
                         ;; message-- anything else waiting for us?
                         (if lines
                             (let* ((lines (cdr lines))
                                    (line (car lines)))
                               (should (eq (length lines) 1))
                               (should (string= "noidle" line))
                               ;; Done
                               (setq idle-state t
                                     msgs (cdr msgs)))
                           ;; The "noidle" didn't show up in this
                           ;; message-- record the fact that we're
                           ;; expecting it next.
                           (setq idle-state t))))
                   ;; The standard case-- check the input, send the response.
                   (setq msgs (cdr msgs))
                   (should (string= in text))
                   (process-send-string proc out))))))
         (mpdmacs-port (process-contact server :service))
         (mpdmacs-host "localhost")
         (mpdmacs-socket nil))

    (require 'mpdmacs)
    (mpdmacs-mode)
    (sit-for 1)
    (while (accept-process-output))
    (delete-process server)
    (should (not (process-live-p server)))
    (message "\n%s\n" (with-current-buffer "*mpdmacs-test-updates*" (buffer-string)))

    (should (string= mpdmacs--current-song-label "De-Phazz - Plastic Love Memory (No. Nine)"))
    (should (equal mpdmacs--stored-playlists '("foo" "saturday-afternoons-in-santa-cruz")))))

(provide 'mpdmacs-tests)

;;; mpdmacs-tests.el ends here
