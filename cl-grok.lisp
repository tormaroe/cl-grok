(in-package #:cl-grok)

; https://www.elastic.co/guide/en/logstash/current/plugins-filters-grok.html
; https://github.com/Beh01der/node-grok/blob/master/lib/index.js
; https://gist.github.com/jimmcslim/4c1b45e1f5a61888d8ec


;;; VARIABLE NAME CONVENTIONS:
;;
;;  p       : string containing a pattern
;;  dpl     : "Defined Pattern List", an a-list of named grok patterns
;;  info    : an instance of <patterninfo>
;;  infos   : a list of <patterninfo>

(defvar *line-pattern-regex* "^([A-Z0-9_]+)\\s+(.+)")

(defun load-patterns (stream &optional (dpl ()))
  (loop for line = (read-line stream nil)
        while line 
        collect (line-pattern-to-cons line) into dpl
        finally (return (remove-if #'null dpl))))

(defun load-patterns-from-file (filepath &optional (dpl ()))
  (with-open-file (stream filepath)
    (load-patterns stream dpl)))

(defun load-default (&optional (dpl ()))
  (let ((stream (make-string-input-stream cl-grok.patterns:*default*)))
    (load-patterns stream dpl)))

(defun line-pattern-to-cons (line)
  (cl-ppcre:register-groups-bind (name pattern) 
      (*line-pattern-regex* line :sharedp nil) 
    (cons name pattern)))

(defun get-named-pattern (name dpl)
  (cdr (assoc name dpl :test #'equal))) ; Is this needed?

;;; Example: (define "DATA" ".*?" ()) ==> (("DATA" . ".*?"))
(defun define (name pattern dpl)
  (acons name pattern dpl)) ;; Do we need this function?

(defun make-filter (pattern dpl) ; LATER: + &optional (options ())
  (lambda (input)
    (do-filter input pattern dpl)))

(defun do-filter (input p dpl)
  (let* ((infos (mapcar (lambda (info) 
                          (extract-syntax-and-semantic p info)) 
                        (locate-grok-patterns p)))
         (modified-pattern (apply-patterns p infos dpl)))
    (format t "DEBUG: Filter input ~S using grok pattern ~S~%" input p)
    (format t "DEBUG: ~S~%" infos)
    (format t "DEBUG: Modified filter is ~S~%" modified-pattern)
    (multiple-value-bind (m-start m-end r-starts r-ends)
        (cl-ppcre:scan modified-pattern input)
      (format t "DEBUG: RESULT>> ~S ~S ~S ~S~%" m-start m-end r-starts r-ends)
      ;; TODO: iterate over r-starts/r-ends, apply to infos
      ;;       or should we scan to strings directly.., don't need indexes?

      )
    '(("name" . "tormaroe")))) ; .. WIP

(defstruct <patterninfo>
  pattern-start ; index of '%' in original pattern
  pattern-end   ; index after '}' in original pattern
  part-starts   ; array with start index for syntax and semantic
  part-ends     ; array with end index for syntax and semantic
  syntax        ; Datatype
  semantic      ; Field name (optional) 
  )

(defun locate-grok-patterns (p &key (start 0) (acc ()))
  (multiple-value-bind (m-start m-end r-starts r-ends)
      (cl-ppcre:scan "%{([a-zA-Z0-9-_]+):?([a-zA-Z0-9-_]+)?}" p :start start)
    (if m-start
      (let ((info (make-<patterninfo> :pattern-start m-start 
                                      :pattern-end m-end
                                      :part-starts r-starts
                                      :part-ends r-ends)))
        (locate-grok-patterns p :start m-end :acc (cons info acc)))
      acc)))

(defun extract-syntax-and-semantic (p info)
  (flet ((extract-part (n)
            (subseq p (aref (<patterninfo>-part-starts info) n)
                      (aref (<patterninfo>-part-ends info) n))))
    (setf (<patterninfo>-syntax info) 
          (extract-part 0))
    (when (not (null (aref (<patterninfo>-part-starts info) 1)))
      (setf (<patterninfo>-semantic info) 
            (extract-part 1)))
    info))

(defun apply-patterns (p infos dpl)
  (aif (car infos)
    (apply-patterns 
      (format nil "~a(~a)~a"
        (subseq p 0 (<patterninfo>-pattern-start it))
        (get-named-pattern (<patterninfo>-syntax it) dpl)
        (subseq p (<patterninfo>-pattern-end it) (length p)))
      (cdr infos)
      dpl)
    p))

;; Strategy v1 (no recursion):
;
; * Find all grok patterns /%{(.+):?(.+)?}/ (named or otherwise) in filter pattern
; * Keep names. For un-named patterns, use datatype. Need to keep track of which are named or not (later).
; * Replace grok patterns by datatype pattern definition from dpl and make it a capture. Use the struct (see below).
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