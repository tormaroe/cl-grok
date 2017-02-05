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

(setf cl-grok:*debug* t)


(format t "*  LOADING DEFAULT~%")
(let ((default-patterns (cl-grok:load-default)))

  (defun test-match (&key text pattern expected)
    (let* ((filter (cl-grok:make-filter pattern default-patterns))
           (match (funcall filter text)))
      (assert (equal expected match) (match))
      (format t "~%")))

  ;; Test a simple, named match
  (test-match :text      "My name is tormaroe"
              :pattern   "My name is %{USERNAME:name}"
              :expected  '(("name" . "tormaroe")))

  ;; Test a simple, un-named match
  (test-match :text      "My name is tormaroe"
              :pattern   "My name is %{USERNAME}"
              :expected  '(("USERNAME" . "tormaroe")))

  ;; Two simple matches
  (test-match :text      "tormaroe said: Hello, world!"
              :pattern   "^%{USERNAME} said: %{DATA:message}$"
              :expected  '(("USERNAME" . "tormaroe")
                           ("message" . "Hello, world!")))


  )
(format t "** END OF TESTS~%")