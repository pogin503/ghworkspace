(defvar omm-mode-line-conf-list 
  (setq omm-mode-line-conf-list mode-line-format))
;;omm-mode-line-conf-list

(defun omm-stryle ()
  (progn
	(line-number-mode nil)
	(scroll-bar-mode -1)
	(tool-bar-mode -1)
	(menu-bar-mode -1)
	(setq mode-line-format nil)))

;;* tool-menu+.elx
