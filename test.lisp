;;;
;;; Run this file to perform a bunch of regression tests against cl-grok.
;;; Script assumes you have QuickLisp available to load cl-grok. 
;;; 
;;; Example: 
;;;           $ sbcl --load test.lisp --quit
;;;

(format t "** RUNNING TESTS~%")

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

  ;; Complex pattern type, IPV4
  (test-match :text      "My IP is 127.0.0.1"
              :pattern   "My IP is %{IPV4:IP}"
              :expected  '(("IP" . "127.0.0.1")))

  ;; Complex and recursive pattern type, IP
  (test-match :text      "My IP is 127.0.0.1"
              :pattern   "My IP is %{IP}"
              :expected  '(("IP" . "127.0.0.1")))

  ;; Complex and recursive pattern type, IP + some other captures
  ;  This has issues. %{IP} for some reason has lots of registered captures in the result, making us loose the second field.
  (test-match :text      "My IP is 127.0.0.1 and my name is tormaroe"
              :pattern   "My IP is %{IP} and my name is %{USERNAME:name}"
              :expected  '(("IP" . "127.0.0.1")
                           ("name" . "tormaroe")))

  )

(format t "** END OF TESTS~%")