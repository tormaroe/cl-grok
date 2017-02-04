(in-package #:cl-grok)

; https://www.elastic.co/guide/en/logstash/current/plugins-filters-grok.html
; https://github.com/Beh01der/node-grok/blob/master/lib/index.js
; https://gist.github.com/jimmcslim/4c1b45e1f5a61888d8ec

(defvar *line-pattern-regex* "^([A-Z0-9_]+)\\s+(.+)")

(defun load-patterns (stream &optional (pattern-list ()))
  (loop for line = (read-line stream nil)
        while line 
        collect (line-pattern-to-cons line) into pattern-list
        finally (return (remove-if #'null pattern-list))))

(defun load-patterns-from-file (filepath &optional (pattern-list ()))
  (with-open-file (stream filepath)
    (load-patterns stream pattern-list)))

(defun load-default (&optional (pattern-list ()))
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
  (acons name pattern pattern-list)) ;; Do we need this function?

(defun make-filter (pattern pattern-list) ; LATER: + &optional (options ())
  (lambda (input)
    (do-filter pattern pattern-list)))

(defun do-filter (pattern pattern-list)
  '(("name" . "tormaroe"))) ; .. WIP

;; Strategy v1 (no recursion):
;
; * Find all grok patterns /%{(.+):?(.+)?}/ (named or otherwise) in filter pattern
; * Keep names. For un-named patterns, use datatype. Need to keep track of which are named or not (later).
; * Replace grok patterns by datatype pattern definition from pattern-list and make it a capture. Use the struct (see below).
; * Run the scan, which results in an array of indexes and an array of lengths.
; * Extract all matches
; * Pair matches with names in the structs
; * Transform the struct list to something suitable to return (the a-list)
;
; Make a struct to keep information about a pattern (%{..})
;  syntax
;  semantics
;  position-in-sequence
;  value
;  named-p
; This is where we can add recursive information in v2
;
; Remember that a match may be optional (%{...}?). In that case it should get the value nil if not matched. Make sure this works ok with cl-ppcre.