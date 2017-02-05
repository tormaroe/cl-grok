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
  (cdr (assoc name pattern-list :test #'equal))) ; Is this needed?

;;; Example: (define "DATA" ".*?" ()) ==> (("DATA" . ".*?"))
(defun define (name pattern pattern-list)
  (acons name pattern pattern-list)) ;; Do we need this function?

(defun make-filter (pattern pattern-list) ; LATER: + &optional (options ())
  (lambda (input)
    (do-filter pattern pattern-list)))

(defun do-filter (pattern pattern-list)
  (let* ((infos (mapcar (lambda (info) (extract-syntax-and-semantic pattern info)) 
                        (locate-grok-patterns pattern)))
         (pattern2 (apply-patterns pattern infos pattern-list)))
  '(("name" . "tormaroe")))) ; .. WIP

(defstruct <patterninfo>
  pattern-start ; index of '%' in original pattern
  pattern-end   ; index after '}' in original pattern
  part-starts   ; array with start index for syntax and semantic
  part-ends     ; array with end index for syntax and semantic
  syntax        ; Datatype
  semantic      ; Field name (optional) 
  )

(defun locate-grok-patterns (pattern &key (start 0) (acc ()))
  (multiple-value-bind (m-start m-end r-starts r-ends)
      (cl-ppcre:scan "%{([a-zA-Z0-9-_]+):?([a-zA-Z0-9-_]+)?}" pattern :start start)
    (if (null m-start)
      acc
      (locate-grok-patterns pattern
                            :start m-end
                            :acc (cons (make-<patterninfo> :pattern-start m-start 
                                                           :pattern-end m-end
                                                           :part-starts r-starts
                                                           :part-ends r-ends) 
                                       acc)))))

(defun extract-syntax-and-semantic (pattern patterninfo)
  (flet ((extract-part (n)
            (subseq pattern (aref (<patterninfo>-part-starts patterninfo) n)
                            (aref (<patterninfo>-part-ends patterninfo) n))))
    (setf (<patterninfo>-syntax patterninfo) 
          (extract-part 0))
    (when (not (null (aref (<patterninfo>-part-starts patterninfo) 1)))
      (setf (<patterninfo>-semantic patterninfo) 
            (extract-part 1)))
    patterninfo))

(defun apply-patterns (p pix plst)
  (if (null pix)
    p
    (let ((info (car pix)))
      (apply-patterns 
        (format nil "~a(~a)~a"
          (subseq p 0 (<patterninfo>-pattern-start info))
          (get-named-pattern (<patterninfo>-syntax info) plst)
          (subseq p (<patterninfo>-pattern-end info) (length p)))
        (cdr pix)
        plst))))

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