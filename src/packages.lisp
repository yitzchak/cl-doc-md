(defpackage #:tanstaafl
  (:use #:common-lisp)
  (:nicknames #:t)
  (:export
    #:*documentation-in-markdown*
    #:generate
    #:write-documentation))

#+sbcl (require :sb-introspect)
