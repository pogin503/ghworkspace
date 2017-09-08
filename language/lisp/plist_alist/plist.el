;;; plist --- plist
;; This program is free software
;;; Commentary:
;;; Code:

(plist-get '(foo 4) 'foo)
;=> 4
(plist-get '(foo 4 bad) 'foo)
;=> 4
(plist-get '(foo 4 bad) 'bad)
;=> nil
(plist-get '(foo 4 bad) 'bar)
;; ===
;=> nil
(setq __pfoo '(:bar "BAR" :hoge "HOGE"))
;=> (:bar BAR :hoge HOGE)
(plist-get __pfoo :bar)
;=> "BAR"

(setq my-plist '(bar t foo 4))
;=> (bar t foo 4)
(setq my-plist (plist-put my-plist 'foo 69))
;=> (bar t foo 69)
(setq my-plist (plist-put my-plist 'quux '(a)))
;=> (bar t foo 69 quux (a))
(plist-put my-plist :piyo "PIYO")
;=> (bar t foo 69 quux (a) :piyo PIYO)

(setq olist `(:scroll-bar ,(eq scroll-bar-mode t)
	      :tool-bar ,(eq tool-bar-mode t)
	      :menu-bar ,(eq menu-bar-mode t)
	      :linum ,(if (boundp 'linum-mode)
			  (list t (eq linum-mode t))
			(list nil nil))
	      :elscreen ,(if (fboundp 'elscreen-mode)
			     (list t (eq elscreen-mode t))
			   (list nil nil))
	      :tabbar ,(if (fboundp 'tabbar-mode)
			   (list t (eq tabbar-mode t))
			 (list nil nil))
	      ))

(require 'ht)
(setq olist1 (ht
	      (:scroll-bar (eq scroll-bar-mode t))
	      (:tool-bar (eq tool-bar-mode t))
	      (:menu-bar (eq menu-bar-mode t))
	      (:elscreen (if (fboundp 'elscreen-mode)
			     (list t (eq elscreen-mode t))
			   (list nil nil)))
	      (:tabbar (if (fboundp 'tabbar-mode)
			   (list t (eq tabbar-mode t))
			 (list nil nil)))))
(ht-get olist1 :scroll-bar)
(ht-get olist1 :menu-bar)
(ht-get olist1 :elscreen)
olist1
#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:scroll-bar nil :tool-bar nil :menu-bar t))


(provide 'plist)
;;; plist ends here
