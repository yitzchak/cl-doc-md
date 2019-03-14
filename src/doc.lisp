(in-package #:doc-md)

(defun normalize-symbol-case (name)
  (case (readtable-case *readtable*)
    (:upcase (string-upcase name))
    (:downcase (string-downcase name))
    (:invert
      (cond
        ((every #'upper-case-p name) (string-downcase name))
        ((every #'lower-case-p name) (string-upcase name))
        (t name)))
    (otherwise name)))

(defun mangle-symbol-case (name)
  (case (readtable-case *readtable*)
    (:upcase (string-downcase name))
    (:downcase (string-upcase name))
    (:invert
      (cond
        ((every #'upper-case-p name) (string-downcase name))
        ((every #'lower-case-p name) (string-upcase name))
        (t name)))
    (otherwise name)))

(defgeneric generate-documentation (object))

(defun sexpr-to-text (value)
  (string-trim '(#\Newline)
    (with-output-to-string (s)
      (pprint value s))))

(defmethod generate-documentation ((s symbol))
  (when-let ((class (find-class s nil)))
    (closer-mop:finalize-inheritance class))
  (let ((*package* (symbol-package s)))
    (iter
      (for definition in (trivial-documentation:symbol-definitions s))
      (for kind next (getf definition :kind))
      (for documentation next (getf definition :documentation))
      (format t "~%## ~@[~A ~]`~S`~%"
        (case kind
          (:symbol "*Symbol*")
          (:constant "*Constant*")
          (:variable "*Variable*")
          (:function "*Function*")
          (:generic-function "*Generic Function*")
          (:macro "*Macro*")
          (:structure "*Structure*")
          (:class "*Class*")
          (:type "*Type*"))
        s)
      (case kind
        (:class
          (when-let ((precedence-list
                        (remove-if
                          (lambda (c)
                            (or (equal t c)
                                (not (eql :external (nth-value 1 (find-symbol (symbol-name c) (symbol-package c)))))))
                          (getf definition :precedence-list))))
            (format t "~%### Superclasses~%~{~%- `~S`~}~%" precedence-list))
          (when-let ((slots (getf definition :slots)))
            (format t "~%### Slots~%~{~%- `~S` &mdash; ~A~}~%" slots))
          (when-let ((initargs (getf definition :initargs)))
            (format t "~%### Initial Arguments~%~{~%- `~S`~}~%" initargs)))
        ((:constant :variable)
         (format t "~%### Definition~%~%```common-lisp~%~S~%```~%" (getf definition :value)))
        ((:function :generic-function :macro)
          (format t "~%### Syntax~%~%```common-lisp~%~S~%```~%" (cons s (getf definition :lambda-list)))))
      (when-let ((documentation (getf definition :documentation)))
        (format t "~%### Description~%~%~A~%" (string-trim '(#\Newline) documentation))))))

(defmethod generate-documentation ((p package))
  (let ((name (mangle-symbol-case (package-name p))))
  (with-open-file (*standard-output* (make-pathname :directory '(:relative "docs") :name name :type "md")
                          :direction :output :if-exists :supersede)
    (format t "# Package ~A~%" name)
    (iter
      (for symbol in-package p external-only t)
      (terpri)
      (generate-documentation symbol)))))

(defun generate (package-name)
  (let ((package (find-package (normalize-symbol-case package-name)))
        (*print-case* :downcase))
    (when package
      (generate-documentation package))))
