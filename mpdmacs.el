;;; mpdmacs.el --- A lightweight MPD client  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (elmpd "0.1"))
;; Keywords: comm
;; URL: https://github.com/sp1ff/mpdmacs

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

;; "Music Player Daemon (MPD) is a flexible, powerful, server-side
;; application for playing music."  <https://www.musicpd.org/>.
;; mpdmacs is a lightweight MPD client for Emacs.

;; See also mpdel <https://gitea.petton.fr/mpdel/mpdel >, as well as
;; the mpc package which ships with Emacs beginning with version 23.2.

;; mpdmacs requires elmpd <https://github.com/sp1ff/elmpd>, a
;; lightweight asynchronous library for building MPD clients.  My goal
;; is to have Emacs never freeze or pause while running this package.

;; Communications Model:

;; mpd encourages "throwaway" connections; i.e. a pattern in which a
;; connection to the server is made, one or a few commands are issued,
;; and the connection then closed down (the default server-side
;; timeout is 60 seconds).

;; The one exception is the "idle" command, which clients can use
;; to receive notifications of server-side changes.  When a client
;; issues the "idle" command, server-side timeouts are disabled.

;; mpdmacs uses a single `elmpd-connection' (which itself uses a
;; socket over which it will talk to MPD along with a callback for
;; servicing server-side changes).  The connection will immediately be
;; put into "idle" mode.  Commands issued through this package will
;; cause a "noidle" command to be issued on that connection before the
;; commands are issued; after the commands complete, the connection
;; will again be placed into "idle" mode.

;;; Code:

(require 'cl-lib)
(require 'elmpd)

(defconst mpdmacs-version "0.1.0")

(defgroup mpdmacs nil
  "A lightweight, ergonomic MPD client for Emacs."
  :group 'comm)

(defcustom mpdmacs-connection-name "mpdmacs"
  "Default connection name.

The name of any particular connection will be modified to make it
unique."
  :group 'mpdmacs
  :type 'string)

(defcustom mpdmacs-host "localhost"
  "Default `mpd' server name."
  :group 'mpdmacs
  :type 'string)

(defcustom mpdmacs-port 6600
  "Default `mpd' port."
  :group 'mpdmacs
  :type 'int)

(defcustom mpdmacs-socket "/var/run/mpd/socket"
  "Default local (Unix) socket on which mpd is listening."
  :group 'mpdmacs
  :type 'string)

(defcustom mpdmacs-password nil
  "Default `mpd' password (nil to indicate none)."
  :group 'mpdmacs
  :type 'string)

(defvar mpdmacs-mode-line-update-function #'mpdmacs--update-mode-line)

(defvar mpdmacs-load-hook '())

(defvar mpdmacs-unload-hook '())

;; We'll re-use the elmpd logging feature, but with our own, dedicated facility.
(defun mpdmacs-log (level fmt &rest objects)
  "Log FMT at level LEVEL & facility 'mpdmacs."
  (apply 'elmpd-log level 'mpdmacs fmt objects))

(defun mpdmacs--update-mode-line ()
  "Add the current track to the mode-line."
  (setq global-mode-string (if mpdmacs--current-track mpdmacs--current-track "N/A")))

;;; State

(defvar mpdmacs--connection nil
  "Persistent \"idle\" connection.

`mpdmacs' will setup a long-lived connection to monitor updates
on the server.")

(defvar mpdmacs--current-track nil
  "Short string describing the current track (if any).

Takes the form of ARTIST - TITLE.  Updated automatically
whenever `mpdmacs--connection' is informed of a change in player
state.")

(defvar mpdmacs--stored-playlists nil
  "List of last-known stored playlists.

Updated automatically whenever `mpdmacs--connection' is informed
of a change in the stored playlists.")

(defvar mpdmacs--player-options
  '((repeat           . unknown)
    (random           . unknown)
    (single           . unknown)
    (consume          . unknown)
    (state            . unknown)
    (crossfade        . unknown)
    (replay-gain-mode . unknown))
  "Association list mapping player options to values.

Updated automatically by `mpdmacs--connection' when it is
informed of any change on the server side.")

(defun mpdmacs--update-current-track ()
  "Update `mpdmacs--current-track'."
  (elmpd-send
   mpdmacs--connection
   "currentsong"
   (lambda (_conn ok text)
     (if (not ok)
         (progn
           (mpdmacs-log 'error "Failed to get current track: %s" text)
           (setq mpdmacs--current-track "N/A"))
       (let ((artist
              (if (string-match "^Artist: \\(.*\\)" text)
                  (substring text (match-beginning 1) (match-end 1))))
             (title
              (if (string-match "^Title: \\(.*\\)" text)
                  (substring text (match-beginning 1) (match-end 1)))))
         (setq
          mpdmacs--current-track
          (cond
           ((and artist title)
            (format "%s - %s" artist title))
           (artist (format "%s - N/A" artist))
           (title (format "N/A - %s" title))
           (t "")))
         (mpdmacs-log 'debug "Updated current track to %s" mpdmacs--current-track)))
     (if mpdmacs-mode-line-update-function (funcall mpdmacs-mode-line-update-function)))))

(defun mpdmacs--update-stored-playlists ()
  "Update `mpdmacs--stored-playlists'."
  (elmpd-send
   mpdmacs--connection
   "listplaylists"
   (lambda (_conn ok text)
     (if (not ok)
         (mpdmacs-log 'error "`Failed to get playlists: %s" text)
       (setq
        mpdmacs--stored-playlists
        (sort
         (cl-mapcar
          (lambda (x) (substring x 10))
          (cl-remove-if-not
           (lambda (x) (string-prefix-p "playlist: " x))
           (split-string text "\n" t)))
         'string-lessp))))))

(defun mpdmacs--intern-boolean-value (text)
  "Convert TEXT for boolean value to a symbol.

MPD maintains assorted values defined to be boolean in nature.
This is a convenience function for mapping the wire value (text
in the case of MPD) to LISP symbols.  If TEXT cannot be
interpreted as per the MPD protocol, the symbol 'unknown will be
returned."

  (cond
   ((string= text "0") nil)
   ((string= text "1") t)
   (t 'unknown)))

(defun mpdmacs--update-player-options ()
  "Updated `mpdmacs--player-options'."
  (elmpd-send
   mpdmacs--connection
   "status"
   (lambda (_conn ok text)
     (if (not ok)
         ;; TODO(sp1ff): set all options to 'unknown?
         (mpdmacs-log 'error "Failed to get player status: %s" text)
       (cl-mapc
        (lambda (line)
          (cond
           ((string-prefix-p "repeat: " line)
            (setf (alist-get 'repeat mpdmacs--player-options) (mpdmacs--intern-boolean-value (substring line 8))))
           ((string-prefix-p "random: " line)
            (setf (alist-get 'random mpdmacs--player-options) (mpdmacs--intern-boolean-value (substring line 8))))
           ((string-prefix-p "single: " line)
            (setf (alist-get 'single mpdmacs--player-options)
                  (let ((value (substring line 8)))
                    (cond
                     ((string= value "0") nil)
                     ((string= value "1") t)
                     ((string= value "oneshot") 'oneshot)
                     (t 'unknown)))))
           ((string-prefix-p "consume: " line)
            (setf (alist-get 'consume mpdmacs--player-options) (mpdmacs--intern-boolean-value (substring line 9))))
           ((string-prefix-p "state: " line)
            (setf (alist-get 'state mpdmacs--player-options)
                  (let ((value (substring line 7)))
                    (cond
                     ((string= "play" value) 'play)
                     ((string= "stop" value) 'stop)
                     ((string= "pause" value) 'pause)
                     (t 'unknown)))))
           ((string-prefix-p "xfade: " line)
            ;; TODO(sp1ff): this is awful-- should validate the input string
            (setf (alist-get 'crossfade mpdmacs--player-options) (string-to-number (substring line 7))))))
        (split-string text "\n" t))
       ;; replay gain status isn't included in the output of the
       ;; "status" command, so we have to go back to the server for
       ;; that:
       (elmpd-send
        mpdmacs--connection
        "replay_gain_status"
        (lambda (_conn ok text)
          ;; We expect a single line (of the form "replay_gain_mode:
          ;; XXX", but just in case the protocol changes out from
          ;; under us, be ready to process multiple lines.
          (if (not ok)
              ;; TODO(sp1ff): set this option to 'unknown?
              (mpdmacs-log 'error "Failed to get replay-gain-mode: %s" text)
            (cl-mapc
             (lambda (line)
               (cond
                ((string-prefix-p "replay_gain_mode: " line)
                 (setf (alist-get 'replay-gain-mode mpdmacs--player-options)
                       (let ((value (substring line 18)))
			 (cond
                          ((string= value "off") 'off)
                          ((string= value "track") 'track)
                          ((string= value "album") 'album)
                          ((string= value "auto") 'auto)
                          (t 'unknown)))))))
             (split-string text "\n" t)))))))))

(defun mpdmacs--watcher (_conn subsys)
  "Idle-mode callback; SUBSYS is a list of subsystems..

This function will be invoked when any subsystem in which we have
registered an interest changes on the server.  The particular
subsystems will be listed in SUBSYS (a list of symbols)."
  (cl-mapc
   (lambda (x)
     (cond
      ((eq x 'player)
       (mpdmacs-log 'debug "idle event: player")
       (mpdmacs--update-current-track))
      ((eq x 'stored)
       (mpdmacs-log 'debug "idle event: stored_playlists")
       (mpdmacs--update-stored-playlists))
      ((eq x 'options)
       (mpdmacs-log 'debug "idle event: options")
       (mpdmacs--update-player-options))
      (t
       (mpdmacs-log 'error (format "Unknown idle event %s" x)))))
   subsys))

;;; Commands

(defun mpdmacs-play ()
  "Sart `mpd' playback."
  (interactive)
  (elmpd-send mpdmacs--connection "play"))

(defun mpdmacs-replay ()
  "Re-play the current song from the beginning."
  (interactive)
  (elmpd-send mpdmacs--connection "seekcur 0"))

(defun mpdmacs-stop ()
  "Stop `mpd' playback."
  (interactive)
  (elmpd-send mpdmacs--connection "stop"))

(defun mpdmacs-next ()
  "Skip ahead to the next track."
  (interactive)
  (elmpd-send mpdmacs--connection "next"))

(defun mpdmacs-previous ()
  "Skip back to the previous track."
  (interactive)
  (elmpd-send mpdmacs--connection "previous"))

(defun mpdmacs-clear ()
  "Clear the play queue."
  (interactive)
  (elmpd-send mpdmacs--connection "clear"))

(defvar mpdmacs-keymap (make-sparse-keymap)
  "Keymap for `mpdmacs' commands.")

(define-key mpdmacs-keymap "P"         #'mpdmacs-play)
(define-key mpdmacs-keymap (kbd "DEL") #'mpdmacs-replay)
(define-key mpdmacs-keymap "s"         #'mpdmacs-stop)
(define-key mpdmacs-keymap ">"         #'mpdmacs-next)
(define-key mpdmacs-keymap "<"         #'mpdmacs-previous)
(define-key mpdmacs-keymap "c"         #'mpdmacs-clear)

;;;###autoload
(defun mpdmacs-enable ()
  "Enable mpdmacs'."
  (interactive)
  (setq
   mpdmacs--connection
   (elmpd-connect
    :name mpdmacs-connection-name
    :host mpdmacs-host
    :port mpdmacs-port
    :local mpdmacs-socket
    :subsystems '((player stored options) . mpdmacs--watcher)))
  (mpdmacs--update-current-track)
  (mpdmacs--update-stored-playlists)
  (mpdmacs--update-player-options)
  (if mpdmacs-load-hook (run-hooks mpdmacs-load-hook))
  (mpdmacs-log 'info "mpdmacs loaded."))

(defun mpdmacs-unload ()
  "Unload `mpdmacs'."
  (interactive)
  (if mpdmacs-unload-hook (run-hooks mpdmacs-unload-hook))
  (delete-process (elmpd-connection--fd mpdmacs--connection))
  (setq mpdmacs--connection nil)
  (mpdmacs-log 'info "mpdmacs unloaded."))

(provide 'mpdmacs)

;;; mpdmacs.el ends here
