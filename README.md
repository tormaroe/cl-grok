cl-grok is a regular expression template library for Common Lisp inspired by the logstash grok filter module.

## Install

Use QuickLisp: `(ql:quickload :cl-grok)`

## Quick start

```
;; Define an input string we want to parse
(defvar input "203.35.135.165 [2016-03-15T12:42:04+11:00] GET memz.co/cloud/")

;; Define a pattern to parse it
(defvar p "%{IP:client} \\[%{TIMESTAMP_ISO8601:timestamp}\\] %{WORD:method} %{URIHOST:site}%{URIPATHPARAM:url}")

;; Load the default set of grok patterns, defining IP, WORD, etc.
(defvar ps (cl-grok:load-default))

;; Create a filter from the pattern
(defvar filter (cl-grok:make-filter input ps))

;; Parse input using the filter
(cl-grok:match input filter)
```

The last line will return the following assosiation list:

```
(("client" . "203.35.135.165")
 ("timestamp" . "2016-03-15T12:42:04+11:00")
 ("method" . "GET")
 ("site" . "memz.co")
 ("url" . "/cloud/"))
```

## API

[Function]
load-patterns stream &optional pattern-list

## License

Copyright (c) 2017 Torbjørn Marø

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.