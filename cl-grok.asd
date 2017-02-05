;;;; cl-grok.asd

(asdf:defsystem #:cl-grok
  :description "Regular expression template library inspired by logstash grok filter module"
  :author "Torbjørn Marø <torbjorn.maro@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-ppcre #:cl-heredoc #:anaphora)
  :serial t
  :components ((:file "package")
               (:file "cl-grok.patterns")
               (:file "cl-grok")))

