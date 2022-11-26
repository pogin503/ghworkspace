(require 'font-lock)

;; モード名の設定
(setq major-mode 'hamlet-mode)

;; モードラインのモード名の設定
(setq mode-name "hamlet-mode")

;; キーマップの設定
(steq hamlet-local-map (make-sparse-keymap))

(defconst hamlet-mode-version "0.1")

(defgroup hamlet-mode nil
  "A html-template major mode."
  :group 'languages)

(defvar hamlet-mode)
