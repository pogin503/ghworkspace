;; -----------------------------------------------------------------------
;; Elliot's .emacs file
;; -----------------------------------------------------------------------

;; This file is divided into the five groups that configure emacs:
;;
;; I)   A prelude which sets up the package system and installs use-package.
;; II)  A list of use-package statements.
;; III) A list of things which should be factored out into packages.
;; IV)  A list mode hooks.
;; V)   Customize.

;; -----------------------------------------------------------------------
;; I) Prelude (Configuration for Configuration)
;; -----------------------------------------------------------------------
;;
;; The first thing we do is set up the package system and then ensure that
;; use-package is installed and set up. We do this before anything else,
;; including loading per-operating system/per-machine configuration so we can
;; make use of the packaging system in them.
;;
;; ------------------------------------- [ Initialize the package system ]
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize nil)
;; In Emacs 25.1, use `package-install-selected-packages'.

;; ------------------------------------------- [ Bootstrap `use-package' ]
;; This ensures `use-package' is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Ensures that we have the features `use-package' requires:
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; ----------------------------------------------- [ Top Level Load Path ]
(add-to-list 'load-path (expand-file-name "~/.elliot-unix/emacs/site-lisp"))

;; ------------------------------------------------- [ Load private data ]
(load "~/.elliot-unix/emacs/private-details")

;; ----------------------------------------- [ Whether we're using Apple ]
(if (string-match "apple" system-configuration)
    (load-file (expand-file-name "~/.elliot-unix/System/Darwin/emacs"))
  ;; If we're not on OSX, hide the menu.
  (menu-bar-mode -1))

;; --------------------------------------- [ Local machine configuration ]
;; This loads configuration for this specific machine or network that
;; isn't put in the git repository.
(let ((conf-file (expand-file-name "~/.elliot-unix/local/emacs")))
  (if (file-exists-p conf-file)
      (load-file conf-file)))

;; -----------------------------------------------------------------------
;; II) Packages Used
;; -----------------------------------------------------------------------
;;
;; Loads and configures all the packages that I could use.
;;
;; ------------------------------------------------------------ [ abbrev ]
(use-package abbrev
  :diminish abbrev-mode)

;; -------------------------------------------------- [ browse-kill-ring ]
(use-package browse-kill-ring
  :bind (("C-c k" . browse-kill-ring))
  :config
  (setq browse-kill-ring-show-preview nil))

;; ------------------------------------------------- [ comment-separator ]
(use-package comment-separator
  :bind (("C-c s s" . separator-insert-line)
         ("C-c s h" . separator-insert-header-section)
         ("C-c s b" . separator-insert-header-big))
  :commands (separator-insert-line
             separator-insert-header-section
             separator-insert-header-big)
  :init (setq cs-fill-column 73)
  ;; Personal package not in elpa
  :load-path "site-lisp/")

;; ----------------------------------------------------------- [ company ]
(use-package company
  :commands (my-indent-or-complete)
  :bind ("TAB" . my-indent-or-complete)
  :diminish ""
  :config
  (setq company-backends
        '(company-bbdb company-nxml company-css company-eclim
                       company-semantic company-xcode company-cmake
                       company-capf
                       (company-dabbrev-code company-gtags company-etags
                                             company-keywords)
                       company-oddmuse company-files company-dabbrev))
  (global-company-mode)
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)

  (defun my-indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (indent-according-to-mode))))

;; ------------------------------------------------------ [ dummy-h-mode ]
(use-package dummy-h-mode
  :mode ("\\.h$" . dummy-h-mode)
  :config (setq dummy-h-mode-default-major-mode 'c++-mode))

;; --------------------------------------------------------- [ echo-bell ]
(use-package echo-bell
  :defer t
  :config
  (progn
    (setq echo-bell-string "ding"
          echo-keystrokes 2)
    (echo-bell-mode t)))

;; -------------------------------------------------------------- [ emms ]
(use-package emms
  :commands (emms-streams)
  :config
  (progn
    (setq emms-playlist-buffer-name "*Music*"
          emms-stream-bookmarks-file "~/.elliot-unix/emacs/emms-streams"
          emms-stream-default-action "play")
    (emms-standard)
    (emms-default-players)
    (require 'emms-streams)
    (define-key emms-stream-mode-map (kbd "s") 'emms-stop)
    (require 'emms-playing-time)
    (emms-playing-time 1)))

;; --------------------------------------------------------------- [ erc ]
(use-package erc
  :commands (erc)
  :config
  (progn
    (setq erc-button-buttonize-nicks nil
          erc-format-nick-function 'erc-format-@nick
          erc-hide-list '("JOIN" "PART" "QUIT")
          erc-interpret-mirc-color t
          erc-modules '(autojoin button completion fill irccontrols list
                                 match menu move-to-prompt netsplit networks
                                 noncommands readonly ring scrolltobottom
                                 services stamp spelling track)
          erc-nick-uniquifier "_"
          erc-prompt-for-nickserv-password nil
          erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324"
                                    "329" "332" "333" "353" "477")
          erc-track-position-in-mode-line (quote after-modes))

    ;; Let's not worry about accidentally spamming channels; disable middle
    ;; mouse yank.
    (defun erc-disabled-mouse-yank ()
      (interactive)
      (message "Mouse yank disabled in ERC buffers to avoid spam. Use C-y."))
    (define-key erc-mode-map [mouse-2] 'erc-disabled-mouse-yank)
    (define-key erc-mode-map [M-mouse-2] 'erc-disabled-mouse-yank)

    ;; Lots of default messages say the whole hostname of a user. Instead,
    ;; use short forms.
    (erc-define-catalog-entry 'english 'JOIN
                              "%n has joined channel %c")
    (erc-define-catalog-entry 'english 'NICK
                              "%n is now known as %N")
    (erc-define-catalog-entry 'english 'MODE
                              "%n has change mode for %t to %m")
    (erc-define-catalog-entry 'english 'QUIT
                              "%n has quit: %r")
    (erc-define-catalog-entry 'english 'TOPIC
                              "%n has set the topic for %c: \"%T\"")

    ;; TODO(erg): Use the following to make an automated kickban command.
    (defadvice erc-cmd-IGNORE (after ignore-replys-to (&optional user) activate)
      "After every ignore, copy the list `erc-ignore-list' to
      `erc-ignore-reply-list'. When I ignore someone, I want them *gone*."
      (erc-with-server-buffer (setq erc-ignore-reply-list erc-ignore-list)))

    (defadvice erc-cmd-UNIGNORE (after ignore-replys-to (&optional user)
                                       activate)
      "In case of mistakes made with /ignore."
      (erc-with-server-buffer (setq erc-ignore-reply-list erc-ignore-list)))

    (defun erc-cmd-OPME ()
      "tell chanserv to op me (from: http://paste.lisp.org/display/97466)"
      (interactive)
      (erc-message "PRIVMSG"
                   (format "chanserv op %s %s"
                           (erc-default-target)
                           (erc-current-nick)) nil))

    (defun erc-cmd-DEOPME ()
      "deop myself (from: http://paste.lisp.org/display/97466)"
      (interactive)
      (erc-cmd-DEOP (format "%s" (erc-current-nick))))))

;; -------------------------------------------------- [ find-things-fast ]
(use-package find-things-fast
  :bind (("<f1>" . ftf-find-file)
         ("<f2>" . ftf-grepsource)
         ("<f4>" . ftf-gdb)
         ("<f5>" . ftf-compile))
  :config
  (setq ftf-filetypes
        '("*.dart" "*.tmpl" "*.js" "*.gni" "*.gn" "*.mojom" "*.h" "*.hpp"
          "*.cpp" "*.c" "*.cc" "*.cpp" "*.inl" "*.grd" "*.idl" "*.m" "*.mm"
          "*.py" "*.sh" "*.cfg" "*SConscript" "SConscript*" "*.scons"
          "*.vcproj" "*.vsprops" "*.make" "*.gyp" "*.gypi" "*.el" "*.md")))

;; ---------------------------------------------------------- [ flyspell ]
(use-package flyspell
  ;; Carry our personal dictionary around in our dotfiles repository.
  :diminish flyspell-mode
  :init
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (setq ispell-extra-args '("--sug-mode=bad-spellers")
        ispell-personal-dictionary "~/.elliot-unix/emacs/aspell.en.pws"))

;; --------------------------------------------------- [ git-commit-mode ]
(use-package git-commit
  :init (global-git-commit-mode)
  :config (setq git-commit-summary-max-length 72))

;; ---------------------------------------------------- [ gitconfig-mode ]
(use-package gitconfig-mode
  :mode "gitconfig\\'")

;; ----------------------------------------------------------- [ gn-mode ]
(if (file-exists-p "/work/chrome/src/tools/gn/misc/emacs/gn-mode.el")
    (use-package gn-mode
      :mode "\\.gn\\'"
      :load-path "/work/chrome/src/tools/gn/misc/emacs/"))

;; ---------------------------------------------------------- [ gyp-mode ]
(if (file-exists-p "/work/chrome/src/tools/gyp/tools/emacs/gyp.el")
    (use-package gyp
      :mode ("\\.gypi?\\'" . gyp-mode)
      :load-path "/work/chrome/src/tools/gyp/tools/emacs/"))

;; ----------------------------------------------------------- [ ibuffer ]
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
      '(("default"
         ("version control" (or (mode . svn-status-mode)
                                (mode . svn-log-edit-mode)
                                (name . "^\\*Annotate")
                                (name . "^\\*git-")
                                (name . "^\\*magit:")
                                (name . "^\\*magit-")
                                (name . "^\\*svn-")
                                (name . "^\\*vc-")
                                (name . "^\\*vc\\*$")))
         ("emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")
                      (name . "^\\*ELP Profiling Results\\*$")
                      (name . "^TAGS\\(<[0-9]+>\\)?$")
                      (name . "^\\*Help\\*$")
                      (name . "^\\*info\\*$")
                      (name . "^\\*Occur\\*$")
                      (name . "^\\*grep\\*$")
                      (name . "^\\*Apropos\\*$")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*Backtrace\\*$")
                      (name . "^\\*Packages\\*$")
                      (name . "^\\*Process List\\*$")
                      (name . "^\\*gud\\*$")
                      (name . "^\\*Man")
                      (name . "^\\*WoMan")
                      (name . "^\\*Kill Ring\\*$")
                      (name . "^\\*Completions\\*$")
                      (name . "^\\*tramp")
                      (name . "^\\*shell\\*$")
                      (mode . term-mode)
                      (name . "^\\*compilation\\*$")
                      (mode . Custom-mode)))
         ("EMMS" (or  (name . "^\\*Music\\*$")
                      (name . "^\\*EMMS")
                      (mode . emms-browser-mode)))
         ("IRC" (or (name . "^\\*Finger")
                    (mode . erc-mode)))
         ("emacs source" (or (mode . emacs-lisp-mode)
                             (filename . "\\.el\\.gz$")))
         ("agenda" (or (name . "^\\*Calendar\\*$")
                       (name . "^diary$")
                       (name . "^\\*Agenda")
                       (name . "^\\*org-")
                       (name . "^\\*Org")
                       (mode . org-mode)
                       (mode . muse-mode)))
         ("latex" (or (mode . latex-mode)
                      (mode . LaTeX-mode)
                      (mode . bibtex-mode)
                      (mode . reftex-mode)))
         ("dired" (or (mode . dired-mode))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))

  ;; Order the groups so the order is : [Default], [agenda], [emacs]
  (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                   activate)
    (setq ad-return-value (nreverse ad-return-value))))

;; --------------------------------------------------------------- [ ido ]
(use-package ido
  :config
  (ido-mode t)
  (setq ido-auto-merge-work-directories-length -1
        ido-default-buffer-method 'selected-window
        ido-default-file-method 'selected-window
        ido-enable-flex-matching t))

;; ---------------------------------------------------------- [ js2-mode ]
(use-package js2-mode
  :mode "\\.js$"
  :config (setq js2-basic-offset 2))

;; ------------------------------------------------------- [ kepago-mode ]
(use-package kepago-mode
  ;; Publicly available at https://github.com/eglaysher/rldev-el
  :mode "\\.ke$"
  :load-path "site-lisp/")

;; ---------------------------------------------------------- [ kfn-mode ]
(use-package kfn-mode
  ;; Publicly available at https://github.com/eglaysher/rldev-el
  :mode "\\.kfn$"
  :load-path "site-lisp/")

;; ------------------------------------------------------------- [ magit ]
(use-package magit
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-log-buffer-file)
         ("C-c f" . magit-grep))
  :config (magit-auto-revert-mode))

;; ----------------------------------------------------- [ markdown-mode ]
(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config (add-hook 'markdown-mode-hook 'visual-line-mode))

;; ---------------------------------------------------------- [ midnight ]
(use-package midnight
  :defer 5
  :config
  (setq clean-buffer-list-kill-buffer-names
        '("*Annotate " "*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*"
          "*info*" "*vc*" "*vc-diff*" "*diff*"))
  (setq clean-buffer-list-kill-never-buffer-names
        '("#mojo" "#chromium" "emacs" "*scratch*" "*Messages*")))

;; --------------------------------------------------------------- [ org ]
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c o" . org-jump-to-project-todo))
  :init
  (progn
    (defvar my-org-project-name "")
    (put 'my-org-project-name 'safe-local-variable #'stringp))
  :config
  (progn
    (setq org-agenda-files '("~/org/")
          org-hide-leading-stars t
          org-log-done 'time
          org-odd-levels-only t)

    (defun org-jump-to-project-todo ()
      (interactive)
      (cond ((not (string= "" my-org-project-name))
             (find-file-other-window
              (expand-file-name (concat "~/org/" my-org-project-name ".org"))))
            (t (message (concat "No project set! Set `my-org-project-name' in "
                                "dir-locals.el to enable this.")))))))

;; --------------------------------------------------------- [ powerline ]
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'contour
        powerline-display-buffer-size nil
        powerline-display-hud nil
        powerline-display-mule-info nil)

  ;; Use 'LIGHT SHADE' (U+2591) which should be in unpatched fonts.
  (setq powerline-utf-8-separator-left 9618
        powerline-utf-8-separator-right 9618)

  (setq-default mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw "%*" nil)
                        (powerline-raw
                         (if (window-dedicated-p) "@" " ") nil)
                        (powerline-buffer-id nil 'l)
                        (powerline-raw " ")
                        (funcall separator-left mode-line face1)
                        (powerline-major-mode face1 'l)
                        (powerline-process face1)
                        (powerline-minor-modes face1 'l)
                        (powerline-narrow face1 'l)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 face2)
                        (powerline-vc face2 'r)
                        (when (bound-and-true-p nyan-mode)
                          (powerline-raw (list (nyan-create)) face2 'l))))
             (rhs (list (funcall separator-right face2 face1)
                        ;; TODO(erg): Add long running crud here.
                        (powerline-raw global-mode-string face1 'r)
                        (unless window-system
                          (powerline-raw (char-to-string #xe0a1) face1 'l))
                        (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                          (powerline-raw erc-modified-channels-object face1 'l))
                        (powerline-raw " " face1)
                        (funcall separator-right face1 mode-line)
                        (powerline-raw " (%03l," nil)
                        (propertize "%02c" 'face
                                    (if (>= (current-column) 80)
                                        'error
                                      nil))
                        (powerline-raw ")" nil 'r)
                        (when powerline-display-hud
                          (powerline-hud face2 face1)))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

;; ----------------------------------------- [ pager-default-keybindings ]
(require 'pager-default-keybindings)

;; ---------------------------------------------------- [ sticky-windows ]
(use-package sticky-windows
  :bind (("C-x 0" . sticky-window-delete-window)
         ("C-x 1" . sticky-window-delete-other-windows)
         ("C-x 9" . sticky-window-keep-window-visible))
  :load-path "site-lisp/")

;; ---------------------------------------------------------- [ uniquify ]
(use-package uniquify
  ;; Use angle brackets in buffer names (ie "BUILD.gn<chrome>")
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; --------------------------------------------------------- [ yasnippet ]
(use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :init
  (progn
    (setq yas-indent-line 'fixed
          yas-snippet-dirs '("~/.elliot-unix/emacs/snippets"))
    (yas-global-mode)))
;; --------------------------------------------------------------- [ yow ]
(use-package yow
  ;; Tradition: It is important to remember from where we came. As people,
  ;; we sometimes do things for no other reason but to assert that we are
  ;; part of a tribe. Have some blatant group affiliation signaling.
  :commands (yow)
  :load-path "site-lisp/"
  :config (setq yow-file "~/.elliot-unix/emacs/yow.lines"))

;; -----------------------------------------------------------------------
;; III) Things to Refactor
;; -----------------------------------------------------------------------
;;
;; This section contains snippets that really should be turned into packages
;; and then stuffed in the above.
;;
;; --------------------------------------------------------- [ find-file ]
;; This should be the same as the default value of `cc-other-file-alist' except
;; for the addition of Objective-C ".m" and ".mm" files.
(setq cc-other-file-alist
      '(("\\.cc\\'"  (".h" ".hh"))
        ("\\.hh\\'"  (".cc" ".C"))

        ("\\.c\\'"   (".h"))
        ("\\.h\\'"   (".cc" ".c" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

        ("\\.m\\'"    (".h"))
        ("\\.mm\\'"    (".h"))

        ("\\.C\\'"   (".H"  ".hh" ".h"))
        ("\\.H\\'"   (".C"  ".CC"))

        ("\\.CC\\'"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH\\'"  (".CC"))

        ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
        ("\\.h\\+\\+\\'" (".c++"))

        ("\\.cpp\\'" (".hpp" ".hh" ".h"))
        ("\\.hpp\\'" (".cpp"))

        ("\\.cxx\\'" (".hxx" ".hh" ".h"))
        ("\\.hxx\\'" (".cxx"))))

;; ---------------------------------------------------- [ Helper methods ]
(defun intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on.

This method, when bound to C-x C-c, allows you to close an emacs frame the
same way, whether it's the sole window you have open, or whether it's
a \"child\" frame of a \"parent\" frame.  If you're like me, and use emacs in
a windowing environment, you probably have lots of frames open at any given
time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
frame, and to remember to do C-x C-x to close the main frame (and if you're
not careful, doing so will take all the child frames away with it).  This
is my solution to that: an intelligent close-frame operation that works in
all cases (even in an emacs -nw session).

Stolen from http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html."
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))

(defun diary-entry (filename)
  "Creates a new jekyll post in the diary directory."
  (interactive "MEntry name: ")
  (if (not (boundp 'erg-diary-directory))
      (error "`erg-diary-directory' is not set."))
  (let ((path (concat erg-diary-directory
                      "_posts/"
                      (format-time-string "%Y-%m-%d") "-"
                      (downcase (replace-regexp-in-string " " "-" filename))
                      ".md")))
    (switch-to-buffer (find-file-noselect path))))

(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one.

TODO: Switch around exactly what DWIM. It should switch to an
existing buffer in the current frame if it exists.

Stolenated from: www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/"
  (interactive)
  (require 'term)
  (with-ftf-project-root
   (let ((is-term (string= "term-mode" major-mode))
         (is-running (term-check-proc (buffer-name)))
         (term-cmd "/bin/bash")
         (anon-term (get-buffer "*ansi-term*")))
     (if is-term
         (if is-running
             (if (string= "*ansi-term*" (buffer-name))
                 (call-interactively 'rename-buffer)
               (if anon-term
                   (switch-to-buffer "*ansi-term*")
                 (ansi-term term-cmd)))
           (kill-buffer (buffer-name))
           (ansi-term term-cmd))
       (if anon-term
           (if (term-check-proc "*ansi-term*")
               (switch-to-buffer "*ansi-term*")
             (kill-buffer "*ansi-term*")
             (ansi-term term-cmd))
         (ansi-term term-cmd))))))

;; ------------------------------------------- [ my-start-scripting-mode ]
(defun my-start-scripting-mode (file-extension hash-bang)
  ;; Build a startup template for this mode.
  (require 'tempo)
  (tempo-define-template (concat file-extension "startup")
                         (list (concat hash-bang "\n\n")))
  (push (cons (concat "\\." file-extension "$")
              (intern (concat "tempo-template-" file-extension "startup")))
        auto-insert-alist)

  ;; Make the script executable on save
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p
            nil t))

;; ---------------------------------------------- [ Personal Keybindings ]
(bind-key "<f3>" 'visit-ansi-term)

(bind-key "C-c d" 'diary-entry)
(bind-key "C-x C-c" 'intelligent-close)
(bind-key "M-g c" 'goto-char)
(bind-key "S-<end>" '(lambda () (interactive) (other-window 1)))
(bind-key "S-<home>" '(lambda () (interactive) (other-window -1)))

;; Middle clicking on the mode line shouldn't close all other frames.
(global-set-key [mode-line mouse-2] 'ignore)

;;; Unbind the stupid minimize that I always hit.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-/"))
(global-unset-key (kbd "M-SPC"))

;; --------------------------------------- [ Coding system configuration ]
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.sjs\\'" 'shift_jis)

;; --------------------------------------------------- [ auto-mode-alist ]
(setq auto-mode-alist
      (append '(("cl_description" . commit-message-mode)
                ("\\.mm?$" . objc-mode)
                ("\\.mak$" . makefile-mode)
                ("\\.conf$" . conf-mode)
                ("\\.uncompressed$" . hexl-mode)
                ("\\.scons$" . python-mode)
                ("SCons\\(cript\\|truct\\)" . python-mode)
                ) auto-mode-alist))

;; Ignore haskell interface files.
(add-to-list 'completion-ignored-extensions ".hi")

;; yasnippet usage.
(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas/expand-snippet (buffer-string) (point-min) (point-max)))

(define-derived-mode commit-message-mode text-mode "CommitMsg"
  "A quick hack to set the fill line to 72 characters."
  (setq fill-column 72))

(add-hook 'suspend-hook 'do-auto-save) ;; Auto-Save on ^Z

(put 'eval-expression 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p) ;; Make all yes-or-no questions as y-or-n

;; -----------------------------------------------------------------------
;; IV) Mode Hooks (aka the other way to make loading emacs fast)
;; -----------------------------------------------------------------------
;; ------------------------------------------------ [ my-start-prog-mode ]
(defun my-prog-startup()
  ;; In programming modes, M-i should do ido based imenu switching, to let us
  ;; quickly jump around a buffer.
  (local-set-key (kbd "M-i") 'idomenu)

  ;; All trailing whitespace needs to be highlighted so it can die.
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'my-prog-startup)

;; ----------------------------------------------------- [ LaTeX startup ]
(defun my-LaTeX-startup ()
  "Change the default LaTeX environment."
  (interactive)
  ;; Set up {fly,i}spell to do the right thing
  (flyspell-mode t)
  (setq-local ispell-parser 'tex))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-startup)   ;; AUCTex latex mode
(add-hook 'latex-mode-hook 'my-LaTeX-startup)   ;; Emacs latex mode

;; --------------------------------------------------- [ Haskell startup ]
(defun my-haskell-startup ()
  "Change the default haskell environment."
  (interactive)

  (ftf-add-filetypes '("*.hs"))

  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (turn-on-haskell-decl-scan))

(add-hook 'haskell-mode-hook 'my-haskell-startup)

;; ---------------------------------------------------- [ Help Mode Hook ]
(defun my-help-mode-hook ()
  (interactive)
  ;; This one needs some explanation. `describe-personal-keybindings' is the
  ;; only help-mode using buffer which overflows lines. So truncate them since
  ;; otherwise it messes up the tabular data there.
  (setq truncate-lines t))

(add-hook 'help-mode-hook 'my-help-mode-hook)

;; -------------------------------------------------- [ Makefile startup ]
(defun my-makefile-startup ()
  "Setup Makefile."
  (interactive)
  ;; Override yasnippet in makefile mode.
  (bind-key "TAB" 'tab-to-tab-stop makefile-mode-map))

(add-hook 'makefile-mode-hook 'my-makefile-startup)

;; ------------------------------------------------------ [ Perl Startup ]
(defun my-perl-startup ()
  "Setup perl."
  (interactive)
  (local-set-key (kbd "<f4>") 'perldb)
  (setq tab-width 8)
  (setq indent-tabs-mode nil)  ; Autoconvert tabs to spaces
  (my-start-scripting-mode "pl" "#!/usr/bin/perl"))

(add-hook 'perl-mode-hook 'my-perl-startup)

;; ---------------------------------------------- [ Shell script startup ]
(defun my-shellscript-startup ()
  "Setup shell script mode."
  (interactive)
  (my-start-scripting-mode "sh" "#!/bin/bash"))

(add-hook 'sh-mode-hook 'my-shellscript-startup)

;; ---------------------------------------------------- [ Python startup ]
(defun my-python-startup ()
  "Setup Python style."
  (interactive)
  (local-set-key (kbd "<f4>") 'pdb)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)  ; Autoconvert tabs to spaces
  (my-start-scripting-mode "py" "#!/usr/bin/python"))

(add-hook 'python-mode-hook 'my-python-startup)

;; ------------------------------------------- [ my-common-c-ish-startup ]
(defun my-common-c-ish-startup ()
  (interactive)

  (require 'google-c-style)
  (google-set-c-style)

  (local-set-key (kbd "C-o") 'ff-get-other-file))

(add-hook 'c-mode-common-hook 'my-common-c-ish-startup)

;; ------------------------------------------------- [ Term Mode Startup ]
(defun my-term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun my-term-send-forward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

(defun my-term-mode-startup ()
  (interactive)

  ;; Stuff that term.el says you should set but doesn't set for you by default.
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq-local mouse-yank-at-point t)
  (setq-local transient-mark-mode nil)
  (auto-fill-mode -1)
  (setq tab-width 8 )

  ;; Translate some keys which would otherwise screw up.
  (define-key term-raw-map (kbd "C-<left>") 'my-term-send-backward-word)
  (define-key term-raw-map (kbd "C-<right>") 'my-term-send-forward-word)
  (define-key term-raw-map (kbd "M-p") 'term-send-up)
  (define-key term-raw-map (kbd "M-n") 'term-send-down))

(add-hook 'term-mode-hook 'my-term-mode-startup)

;; ------------------------------------------------ [ Emacs Lisp Startup ]
(defun my-elisp-startup ()
  (interactive)

  ;; Byte compile this file as soon as its saved.
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
        '(lambda () (byte-compile-file buffer-file-name))
        nil t))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-startup)

;; -----------------------------------------------------------------------
;; IV) Customize
;; -----------------------------------------------------------------------
;;
;; I resisted using customize for a long time, but it seems like the best way
;; to set certain variables. We'll see how long this lasts.
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-alist
   (quote
    (("chrome\\/.*\\.cc$" .
      ["chrome-source-file.cc" my/autoinsert-yas-expand])
     ("chrome\\/.*\\.h$" .
      ["chrome-header-file.h" my/autoinsert-yas-expand])
     ("rlvm\\/.*\\.cc$" .
      ["rlvm-source-file.cc" my/autoinsert-yas-expand])
     ("rlvm\\/.*\\.h$" .
      ["rlvm-header-file.h" my/autoinsert-yas-expand])
     ("chrome\\/.*\\.gn$" .
      ["chrome-gn-file.gn" my/autoinsert-yas-expand])
     ("_posts\\/.*\\.md$" .
      ["jekyll-post.md" my/autoinsert-yas-expand]))))
 '(auto-insert-directory "~/.elliot-unix/emacs/insert/")
 '(auto-insert-mode t)
 '(auto-insert-query nil)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/saves"))))
 '(blink-cursor-mode nil)
 '(byte-compile-warnings nil)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(custom-enabled-themes (quote (dark-forest)))
 '(custom-safe-themes t nil nil "If I don't set this, this variable becomes an ever growing alist.")
 '(delete-old-versions t)
 '(enable-local-variables :safe)
 '(fill-column 79)
 '(grep-use-null-device nil nil nil "Needed to prevent git grep from erroring the first time I call it.")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lua-indent-level 2)
 '(major-mode (quote text-mode))
 '(next-screen-context-lines 4)
 '(package-selected-packages
   (quote
    (gitconfig-mode hexrgb company-auctex auctex ascii js2-mode use-package typescript-mode solidity-mode yasnippet yaml-mode scss-mode rainbow-mode pager-default-keybindings ninja-mode markdown-mode magit lua-mode idomenu haskell-mode google-c-style go-mode find-things-fast emms echo-bell dummy-h-mode diminish dart-mode company browse-kill-ring bison-mode powerline)))
 '(perl-continued-brace-offset -2)
 '(perl-continued-statement-offset 2)
 '(perl-indent-level 2)
 '(perl-tab-always-indent nil nil nil "Indent if at left margin, else tab")
 '(python-indent-offset 2)
 '(require-final-newline t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(vc-follow-symlinks nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Inconsolata")))))
