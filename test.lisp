;;;
;;; Run this file to perform a bunch of regression tests against cl-grok.
;;; Script assumes you have QuickLisp available to load cl-grok. 
;;; 
;;; Example: 
;;;           $ sbcl --load test.lisp --quit
;;;

(format t "** RUNNING TESTS~%")

(format t "*  LOADING CL-GROK~%")
(ql:quickload :cl-grok)

(format t "*  LOADING DEFAULT~%")
(let ((default-patterns (cl-grok:load-default)))

  (format t "*  ASSERTING A DEFAULT PATTERN~%")
  (let ((IP-pattern (cl-grok:get-named-pattern "IP" default-patterns)))
    (assert (equal "(?:%{IPV6}|%{IPV4})" IP-pattern) (IP-pattern))

    (format t "*  MAKING A FILTER~%")
    (let ((filter (cl-grok:make-filter "%{IP:address}" 
                                       default-patterns)))

      ; TODO: Assert suff about pattern

      ; TODO: Run a match using filter
      (let ((match (funcall filter "My home is 127.0.0.1")))

        ; TODO: Assert the result 
        (assert (equal "127.0.0.1" (cdr (assoc "address" match :test #'equal)))
                (match)))

  )))

(format t "** END OF TESTS~%")