;;; json --- json
;;; Commentary:
;;; Code:

(require 'json)

(json-read-from-string "true") ;=>
(json-encode t)
(json-read-from-string "4.5")
(json-read-from-string "\"foo\"")
(json-read-from-string "[true, 4.5]")
(json-read-from-string "{\"foo\": true}")
(let ((json-object-type 'plist))
  (json-read-from-string "{\\"foo\\": true}"))

(let ((json-key-type 'string))
  (json-read-from-string "{\\"foo\\": true}"))

(let ((json-object-type 'hash-table))
  (json-read-from-string "{\"foo\": true}"))

(json-encode '(1 2 3))

(json-encode '(:foo 1 :bar 2 :baz 3))

(let  ((buffer (url-retrieve-synchronously "https://google.ca")))
  )

;;; json ends here
