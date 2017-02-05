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

(defun test-match (&key text pattern dpl expected)
  (let* ((filter (cl-grok:make-filter pattern dpl))
         (match (funcall filter text)))
    (assert (equal expected match) (match))))

(format t "*  LOADING DEFAULT~%")
(let ((default-patterns (cl-grok:load-default)))

  (test-match :text "My name is tormaroe"
              :pattern "^My name is %{USERNAME:name}"
              :dpl default-patterns
              :expected '(("name" . "tormaroe")))



  )
(format t "** END OF TESTS~%")