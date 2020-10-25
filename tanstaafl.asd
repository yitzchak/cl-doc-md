(asdf:defsystem #:tanstaafl
  :description "MarkDown documentation extractor."
  :version "0.1"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:alexandria
               :closer-mop
               :multilang-documentation
               :puri)
  :components ((:module src
                :serial t
                :components ((:file "packages")
                             (:file "utils")
                             (:file "formatter")
                             (:file "clhs")
                             (:file "write-documentation")
                             (:file "generate")))))
