;;; erc-view-log.el --- Major mode for viewing ERC logs

;; Copyright (C) 2010 Antoine Levitt
;; Copyright (C) 2010 Thomas Riccardi

;; Author: Antoine Levitt
;;         Thomas Riccardi <riccardi.thomas@gmail.com>
;; URL: http://github.com/Niluge-KiWi/erc-view-log/raw/master/erc-view-log.el
;; Homepage: http://github.com/Niluge-KiWi/erc-view-log/
;; Keywords: ERC viewer logs colors

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

;;; Commentary:
;; Set colors on an ERC log file
;; `erc-fill-mode' is partially supported (in multi-line messages, lines
;; after the first one are not highlighted)

;; Installation:
;;    (require 'erc-view-log)
;;    (add-to-list 'auto-mode-alist `(,(format "%s/.*\\.log" (regexp-quote (expand-file-name erc-log-channels-directory))) . erc-view-log-mode))

;; Options:
;; - erc-view-log-nickname-face-function:
;;    A function that returns a face, given a nick, to colorize nicks.
;;    Can be nil to use standard ERC face.
;; - erc-view-log-my-nickname-match:
;;    Either a regexp or a list of nicks, to match the user's nickname.
;;    For the list, each nick should be unique and should not contain any regexps.

;; TODO:
;; - use vlf.el for large logs? has to be adapted (no more major mode, and handle full lines...)

;;; Code:

(require 'erc)
(require 'cl-macs)
(require 'rx)


(defcustom erc-view-log-nickname-face-function
  nil
  "A function that returns a face, given a nick. nil to use default ERC face."
  :type 'function
  :group 'erc)

(defcustom erc-view-log-my-nickname-match
  erc-nick
  "A match for the user's nickname: either a regexp, or a list of nicks."
  :type '(choice (regexp :tag "A regexp that matches the user's nick.")
		 (list :tag "A list of used nicks. Each nick should be unique and should not contain any regexps."))
  :group 'erc)

(defcustom erc-view-log-timestamp-position
  (cl-case erc-insert-timestamp-function
    (erc-insert-timestamp-right 'right)
    (erc-insert-timestamp-left 'left)
    (t 'both))
  "Position of timestamps in log files.
This position may be changed with `erc-insert-timestamp-function'."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Both sides" both))
  :group 'erc)


;; Warning: do not use group constructions ("\\(some regexp\\)") inside the following regexps
(defvar erc-view-log-timestamp-regexp
  (rx (and "[" (repeat 2 digit) ":" (repeat 2 digit) "]"))
  "Regexp to match stamps with `erc-timestamp-format' format.")

(defvar erc-view-log-timestamp-left-regexp
  "\\[.*\\]\n"
  "Regexp to match stamps with `erc-timestamp-format-left' format.")

(defvar erc-view-log-timestamp-right-regexp
  erc-view-log-timestamp-regexp
  "Regexp to match stamps with `erc-timestamp-format-right' format.")

(defvar erc-view-log-nickname-regexp
  erc-valid-nick-regexp
  "Regexp to match nicknames.")

(defvar erc-view-log-message-regexp
  ".*?"
  "Regexp to match messages.")

(defvar erc-view-log-current-nick-regexp
  "\\*\\*\\* Users on .*: .*?"
  "Regexp to match current nicks lines.")

(defvar erc-view-log-notice-regexp
  "\\*\\*\\* .*?"
  "Regexp to match notices.")

(defvar erc-view-log-action-regexp
  (format "\\* %s .*?" erc-valid-nick-regexp)
  "Regexp to match actions.")

(defvar erc-view-log-prompt-regexp
  erc-prompt
  "Regexp to match prompts.")


(defun erc-log-nick-get-face (nick)
  "Returns a face for the given nick."
  (if erc-view-log-nickname-face-function
      (apply erc-view-log-nickname-face-function (list nick))
    'erc-nick-default-face))

(defun erc-log-get-my-nick-regexp ()
  "Returns a regexp that matches the user's nick according to custom erc-view-log-my-nickname-match."
  (if (listp erc-view-log-my-nickname-match)
      (regexp-opt erc-view-log-my-nickname-match)
    erc-view-log-my-nickname-match))


;; warning: only works if erc-timestamp-format doesn't contains the pattern "<a_nickname>"
(defun erc-view-log-get-keywords ()
  "Returns the font-lock-defaults."
  `(
    ;; own message line
    (,(erc-view-log-finalize-regexp
       (erc-view-log-get-message-line-regexp (erc-log-get-my-nick-regexp)))
     (1 'erc-timestamp-face nil t)
     (2 'erc-timestamp-face nil t)
     (3 'erc-default-face)
     (4 'erc-my-nick-face)
     (5 'erc-default-face)
     (6 'erc-input-face) ;; own message
     )
    ;; standard message line
    (,(erc-view-log-finalize-regexp
       (erc-view-log-get-message-line-regexp erc-view-log-nickname-regexp))
     (1 'erc-timestamp-face nil t)
     (2 'erc-timestamp-face nil t)
     (3 'erc-default-face)
     (4 (erc-log-nick-get-face (match-string 3)))
     (5 'erc-default-face)
     (6 'erc-default-face) ;; other message
     )
    ;; current nicks line
    (,(erc-view-log-finalize-regexp
       (format "\\(?3:%s\\)" erc-view-log-current-nick-regexp))
     (1 'erc-timestamp-face nil t)
     (2 'erc-timestamp-face nil t)
     (3 'erc-current-nick-face)
     )
    ;; notice line
    (,(erc-view-log-finalize-regexp
       (format "\\(?3:%s\\)" erc-view-log-notice-regexp))
     (1 'erc-timestamp-face nil t)
     (2 'erc-timestamp-face nil t)
     (3 'erc-notice-face)
     )
    ;; action line
    (,(erc-view-log-finalize-regexp
       (format "\\(?3:%s\\)" erc-view-log-action-regexp))
     (1 'erc-timestamp-face nil t)
     (2 'erc-timestamp-face nil t)
     (3 'erc-action-face)
     )
    ;; command line
    (,(erc-view-log-finalize-regexp
       (format "\\(?3:%s\\) \\(?4:/.*?\\)" erc-view-log-prompt-regexp))
     (1 'erc-timestamp-face nil t)
     (2 'erc-timestamp-face nil t)
     (3 'erc-prompt-face)
     (4 'erc-command-indicator-face)
     )))

(defun erc-view-log-get-message-line-regexp (nick-re)
  "Return regexp to match a full user message.
NICK-RE is regexp to match a nick of the message author.
User message is a message sent to other users (excluding \"/me\").
It consists of a user nick surrounded by angle brackets and a
message body."
  (rx-to-string
   `(and (group-n 3 "<")
         (group-n 4 (regex ,nick-re))
         (group-n 5 ">")
         blank
         (group-n 6 (regex ,erc-view-log-message-regexp)))))

(defun erc-view-log-finalize-regexp (re)
  "Return regexp consisting of RE and additional structures.
Add groups with timestamps to the beginning and to the end of RE
depending on `erc-view-log-timestamp-position' and make it match
a whole line.
Return resulting regexp."
  (cl-multiple-value-bind (left right)
      (cl-case erc-view-log-timestamp-position
        (left  (list erc-view-log-timestamp-regexp nil))
        (right (list nil erc-view-log-timestamp-regexp))
        (both  (list erc-view-log-timestamp-left-regexp
                     erc-view-log-timestamp-right-regexp)))
    (concat "^"
            (and left  (format "\\(?1:%s\\)?[[:blank:]]*" left))
            re
            (and right (format "[[:blank:]]*\\(?2:%s\\)?" right))
            "$")))


;; undefine some syntax that's messing up with our coloring (for instance, "")
(defvar erc-view-log-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    st)
  "Syntax table used while in `erc-view-log-mode'.")


(defun erc-view-log-reload-file ()
  "Reload the current logfile."
  (interactive)
  (revert-buffer t t t)
  ;; revert-buffer removes read-only state
  (setq buffer-read-only t))

(defun erc-view-log-previous-mention (&optional arg)
  "Move point to previous mention of one's nick.
If ARG is set, move to previous message from one's nick."
  (interactive "P")
  (re-search-backward
   (format (if (null arg) "%s" "^[^<]*<%s>")
           (regexp-opt erc-view-log-my-nickname-match))))

(defun erc-view-log-next-mention (&optional arg)
  "Move point to next mention of one's nick.
If ARG is set, move to next message from one's nick."
  (interactive "P")
  (re-search-forward
   (format (if (null arg) "%s" "^[^<]*<%s>")
           (regexp-opt erc-view-log-my-nickname-match))))

;; Create the keymap for this mode.
(defvar erc-view-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'erc-view-log-reload-file)
    (define-key map "p" 'erc-view-log-previous-mention)
    (define-key map "n" 'erc-view-log-next-mention)
    map)
  "Keymap for `erc-view-log-mode'.")

(define-derived-mode erc-view-log-mode fundamental-mode
  "ERC View Log"
  "Major mode for viewing ERC logs.
Colorizes the log file as if it were a standard ERC buffer.

Special commands:

\\{erc-view-log-mode-map}

Turning on `erc-view-log-mode' runs the hook `erc-view-log-mode-hook'."
  (setq font-lock-defaults `(,(erc-view-log-get-keywords)))
  (setq buffer-read-only t)
  ;; workaround for emacs bug #11943: 24.1.50; Emacs unusably slow when looking at large files
  ;;(setq bidi-paragraph-direction 'left-to-right)
  ;; even faster workaround
  (setq bidi-display-reordering nil)
  )


(provide 'erc-view-log)

;;; erc-view-log.el ends here
