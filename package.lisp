;;;; package.lisp

(defpackage #:cl-grok.patterns
  (:use #:cl)
  (:export #:*default*))

(defpackage #:cl-grok
  (:use #:cl #:anaphora)
  (:export #:load-default
           #:get-named-pattern
           #:make-filter
           #:*debug*))

