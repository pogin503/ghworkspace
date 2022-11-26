;;; sample-request --- sample-request
;; This program is free software
;;; Commentary:
;;; Code:
(require 'request)
(request
 "http://httpbin.org/get"
 :params '(("key" . "value") ("key2" . "value2"))
 :parser 'json-read
 :success (function*
           (lambda (&key data &allow-other-keys)
             (message "I sent: %S" (assoc-default 'args data)))))
(provide 'sample-request)
;;; sample-request ends here
