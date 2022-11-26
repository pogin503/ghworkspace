;;; tool-bar+.el --- Extensions to standard library tool-bar.el
;;
;; Filename: tool-bar+.el
;; Description: Extensions to standard library tool-bar.el
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004, Drew Adams, all rights reserved.
;; Created: Tue Oct 05 17:02:16 2004
;; Version: 21.0
;; Last-Updated: Sun Oct 10 21:41:05 2004
;;           By: dradams
;;     Update #: 163
;; Keywords: tool-bar, convenience, mouse, button, frame
;; Compatibility: GNU Emacs 21.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: Extensions to standard library tool-bar.el
;;
;;
;;  New commands defined here:
;;
;;    `show-tool-bar-for-one-command', `tool-bar-here-mode',
;;    `tool-bar-pop-up-mode'.
;;
;;
;;  New key bound here: [menu-bar pop-up-tool-bar]
;;
;;
;;  Usage:
;;
;;    Load this library: (require 'tool-bar+).
;;    Turn on tool-bar pop-up mode: M-x tool-bar-pop-up-mode.
;;
;;    Click "Buttons" in the menu-bar to access the tool-bar when you
;;    need it. This displays the tool-bar buttons just long enough for
;;    one command: after you click a tool-bar button, the tool-bar
;;    disappears again.
;;
;;    The advantage of `tool-bar-pop-up-mode' is that you do not lose
;;    frame real estate to the tool-bar -- you have it when you need
;;    it, at the cost of an extra click ("Buttons").
;;
;;    In addition to defining minor mode `tool-bar-pop-up-mode', this
;;    library defines minor mode `tool-bar-here-mode', which is the
;;    same as the global `tool-bar-mode' except that it affects only
;;    the current frame.
;;
;;    The advantage of `tool-bar-here-mode' is (again) that it saves
;;    real estate on frames other than the ones with the tool-bar.
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;  (require 'tool-bar+) ; load this library
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2004/10/10 dadams
;;
;;     Added condition-case in show-tool-bar-for-one-command.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;; Local Tool Bar Mode -------------------------

(define-minor-mode tool-bar-here-mode
  "Toggle use of the tool bar on this frame only.
With numeric ARG, display the tool bar if and only if ARG is positive.

See `tool-bar-add-item' and `tool-bar-add-item-from-menu' for
conveniently adding tool bar items."
  :init-value nil :global t :group 'mouse :group 'frames
  (and (display-images-p)
       (let ((lines (if tool-bar-here-mode 1 0)))
         ;; Alter existing frame...
         (modify-frame-parameters (selected-frame) (list (cons 'tool-bar-lines
                                                               lines))))
       (if (and tool-bar-here-mode
                (display-graphic-p)
                (= 1 (length (default-value 'tool-bar-map)))) ; not yet set up
           (tool-bar-setup))))

(make-variable-frame-local 'tool-bar-here-mode)


;;; Pop-Up Tool Bar Mode ------------------------

;; If either of the normal tool-bar modes is turned on, then
;; `tool-bar-popup-mode' is not available.
(define-key global-map [menu-bar pop-up-tool-bar]
  '(menu-item
    "Buttons" show-tool-bar-for-one-command
    :visible (and tool-bar-pop-up-mode (not tool-bar-mode) (not
                                                            tool-bar-here-mode))
    :enable  (and tool-bar-pop-up-mode (not tool-bar-mode) (not
                                                            tool-bar-here-mode))
    :help "Use tool bar for one command"))

(setq menu-bar-final-items (append menu-bar-final-items (list
                                                         'pop-up-tool-bar)))

(define-minor-mode tool-bar-pop-up-mode
  "Toggle tool-bar pop-up.
With numeric ARG, turn on tool-bar pop-up if and only if ARG is positive.

Note: Command `tool-bar-pop-up-mode' functions as a toggle only
      if neither `tool-bar-mode' nor `tool-bar-here-mode' is on.

      If either of those modes is on, then command
      `tool-bar-pop-up-mode' turns them both off and turns
      `tool-bar-pop-up-mode' on."
  :init-value nil :global t :group 'mouse :group 'frames
  (when (or tool-bar-mode tool-bar-here-mode)
    (setq tool-bar-pop-up-mode t)
    (tool-bar-mode -99)
    (tool-bar-here-mode -99)))

;; Note: This treats mouse events specially: it scrolls the buffer
;; text (window) down to compensate for the disappearance of the
;; tool-bar.  That is, it replaces the tool-bar with an equivalent
;; number of lines of buffer text.
;;
;; This is so that the place where you click the mouse when the
;; tool-bar is visible corresponds to the place where the mouse is
;; after the tool-bar disappears. Otherwise, the buffer text moves up,
;; relative to the mouse, and a region is selected (without ever
;; physically moving the mouse).
;;
(defun show-tool-bar-for-one-command ()
  "Pop up the tool bar so you can click a button.
The tool bar stays visible until one command is executed
\(whether or not it was initiated by clicking a button)."
  (interactive)
  (unless tool-bar-pop-up-mode
    (error "You must turn on `tool-bar-pop-up-mode' to use this command"))
  (let (evnt tb-lines)
    (unwind-protect
        (progn
          (tool-bar-here-mode 99)       ; Show tool-bar
          (setq evnt (read-event))
          (push evnt unread-command-events))
      (when (and (consp evnt)
                 (member (event-basic-type (car evnt)) '(mouse-1 mouse-2
                                                                 mouse-3)))
        (setq tb-lines (cdr (assq 'tool-bar-lines (frame-parameters
                                                   (selected-frame)))))
        (condition-case nil
            (when tb-lines (scroll-down tb-lines))
          (error (tool-bar-here-mode -99)))) ; E.g. "Beginning of buffer"
      (tool-bar-here-mode -99))))       ; Hide tool-bar


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tool-bar+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tool-bar+.el ends here