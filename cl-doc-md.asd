(asdf:defsystem #:cl-doc-md
  :description "MarkDown documentation extractor."
  :version "0.1"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (
    :alexandria
    :closer-mop
    :iterate
    :trivial-documentation)
  :components
    ((:module src
      :serial t
      :components
        ((:file "packages")
         (:file "doc")))))
