(defpackage #:tanstaafl
  (:use #:common-lisp)
  (:nicknames #:t)
  (:export
    #:generate
    #:write-documentation))

#+sbcl (require :sb-introspect)
