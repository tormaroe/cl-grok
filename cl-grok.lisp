(in-package #:cl-grok)

; https://www.elastic.co/guide/en/logstash/current/plugins-filters-grok.html
; https://github.com/Beh01der/node-grok/blob/master/lib/index.js
; https://gist.github.com/jimmcslim/4c1b45e1f5a61888d8ec

;;(let* ((grok-patterns (grok:load-default))
;;       (my-pattern (grok:make-pattern grok-patterns "%{WORD:method} %{NUMBER:bytes} %{NUMBER:duration}"))
;;       (matches (grok:parse my-pattern "hello 123 456")))
;;    ; Assert that matches is an alist containing :method :bytes :duration
;;    ))

(defvar *line-pattern-regex* "^([A-Z0-9_]+)\\s+(.+)")

(defun load-patterns (stream &optional (pattern-list ()))
  ""
  (loop for line = (read-line stream nil)
        while line 
        collect (line-pattern-to-cons line) into pattern-list
        finally (return (remove-if #'null pattern-list))))

(defun load-patterns-from-file (filepath &optional (pattern-list ()))
  ""
  (with-open-file (stream filepath)
    (load-patterns stream pattern-list)))

(defun load-default (&optional (pattern-list ()))
  ""
  (let ((stream (make-string-input-stream cl-grok.patterns:*default*)))
    (load-patterns stream pattern-list)))

(defun line-pattern-to-cons (line)
  (cl-ppcre:register-groups-bind (name pattern) 
      (*line-pattern-regex* line :sharedp nil) 
    (cons name pattern)))

(defun get-named-pattern (name pattern-list)
  (cdr (assoc name pattern-list :test #'equal)))

;;; Example: (define "DATA" ".*?" ()) ==> (("DATA" . ".*?"))
(defun define (name pattern pattern-list)
  ""
  (acons name pattern pattern-list))

(defun make-filter (pattern pattern-list) ; LATER: + &optional (options ())
  ""
  )

(defun match (input pattern) ; CONSIDER returning a closure from make-filter instead of having match
  ""
  )