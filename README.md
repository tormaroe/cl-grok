cl-grok is a regular expression template library for Common Lisp inspired by the [logstash grok filter module|https://www.elastic.co/guide/en/logstash/current/plugins-filters-grok.html].

## Install

Use QuickLisp: `(ql:quickload :cl-grok)`

*This is not yet true - initial version is still under development :)*

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
(funcall filter input)
```

The last line will return the following assosiation list:

```
(("client" . "203.35.135.165")
 ("timestamp" . "2016-03-15T12:42:04+11:00")
 ("method" . "GET")
 ("site" . "memz.co")
 ("url" . "/cloud/"))
```

## Example: Parse a log file

Given a file *foo.log** containing the following lines:

```
2017-02-02T23:04:12.345Z INFO Program starting
2017-02-02T23:04:12.345Z DEBUG ...
2017-02-02T23:04:12.345Z INFO ...
2017-02-02T23:04:12.345Z DEBUG ...
2017-02-02T23:04:15.305Z FATAL Program terminating unexpectantly
```

.. we can write a small program to iterate over the lines and apply a grok filter to parse out and display the information we are interested in:

```
...
```

The output would be like this:

```
...
```

## API

**[Function]**<br>
**load-patterns stream &optional pattern-list**<br>
Loads named grok patterns from a stream. Each pattern should be separated by a newline character, and formatted as an uppercase name followed by a space and the grok pattern. An association list of named patterns will be returned. If `pattern-list` is provided, the loaded patterns will be added to that list.

**[Function]**<br>
**load-default &optional pattern-list**<br>
Loads the most common grok patterns, borrowed from Logstash.

**[Function]**<br>
**load-patterns-from-file filepath &optional pattern-list**<br>
Loads named grok patterns from the file designed by `filepath`.

**[Function]**<br>
**make-filter pattern pattern-list**<br>
Returns a filter function for a grok pattern, based on the named patterns in `pattern-list`.

## License

Copyright (c) 2017 Torbjørn Marø

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.