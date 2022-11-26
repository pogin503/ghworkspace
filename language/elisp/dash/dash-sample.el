;;; dash-sample.el --- dash-sample

;; Copyright (C) 2014  Ryo

;; Author: Ryo <OginoRyo@pogi-pogi-net.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'dash)

(-when-let (match-index (string-match "d" "abcd")) (+ match-index 2)) ;; => 5
(--when-let (member :b '(:a :b :c)) (cons :d it)) ;; => '(:d :b :c)
(--when-let (even? 3) (cat it :a)) ;; => nil


(provide 'dash-sample)
;;; dash-sample.el ends here
