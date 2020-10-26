(in-package #:tanstaafl)


(defvar *documentation-in-markdown* nil)


(defun do-write-documentation (stream object doc-type &optional (leader (make-string 2 :initial-element #\Newline)))
  (let ((doc (or (documentation object doc-type)
                 #+clasp (when (eql 'function doc-type)
                           (sys:get-annotation object 'documentation 'method)))))
    (when doc
      (format stream (if *documentation-in-markdown*
                       "~a~a"
                       "~a~:/t:pre/")
              leader doc))))


(defgeneric write-documentation (stream object doc-type detail-level))


(defmethod write-documentation (stream (object package) doc-type detail-level)
  (declare (ignore doc-type))
  (format stream "# ~:/t:text/ [package]" (package-name object))
  (do-write-documentation stream object t)
  (format stream "~@[~%~%## Nicknames~%~%~(~{~@/t:text/~^, ~}~)~]" (package-nicknames object))
  (unless (zerop detail-level)
    (format stream "~@[~%~%## Exports~%~%~{~<~%~1,80:;~@/t:text/~^, ~>~}~]"
                   (exported-symbols object))))


(defun write-symbol-system (stream sym detail-level)
  #+asdf3.1
  (alexandria:when-let* ((system-name (asdf/package-inferred-system::package-name-system (package-name (symbol-package sym))))
                         (system (asdf:find-system system-name nil)))
    (if (asdf:system-homepage system)
      (format stream "~%~%From system [~A](~A)" system-name (asdf:system-homepage system))
      (format stream "~%~%From system ~A" system-name))
    (alexandria:when-let ((license (asdf:system-license system)))
      (format stream " (~A)" license))
    (alexandria:when-let ((author (asdf:system-author system)))
      (format stream " by ~A" author))))


(defmethod write-documentation (stream (object symbol) (doc-type (eql 'variable)) detail-level)
  (when (boundp object)
    (format stream "~%~%## ~:[Dynamic~;Constant~] Variable" (constantp object))
    (do-write-documentation stream object t)
    (format stream "~%~%~@/t:code/" (symbol-value object))))


(defmethod write-documentation (stream (object symbol) (doc-type (eql 'function)) detail-level)
  (when (fboundp object)
    (format stream "~%~%## ~A"
            (cond
              ((special-operator-p object)
                "Special")
              ((macro-function object)
                "Macro")
              ((typep (fdefinition object) 'standard-generic-function)
                "Generic Function")
              (t
                "Function")))
    (do-write-documentation stream object 'function)
    (format stream "~%~%~@/t:code/" (cons object (lambda-list object)))))


(defmethod write-documentation (stream (object symbol) (doc-type (eql 'type)) detail-level)
  (alexandria:when-let ((cls (find-class object nil)))
    (closer-mop:finalize-inheritance cls)
    (format stream "~%~%## ~:[Class~;Structure~]" (subtypep cls (find-class 'structure-object)))
    (do-write-documentation stream object 'type)
    (format stream "~%~%### Precedence List~%~%~{~@/t:text/~^, ~}"
            (mapcar #'class-name (closer-mop:class-precedence-list cls)))
    (when (closer-mop:class-slots cls)
      (format stream "~%~%### Slots~%")
      (dolist (slot (closer-mop:class-slots cls))
        (format stream "~%- ~/t:text/~@[ \\[~/t:text/\\]~]"
                (closer-mop:slot-definition-name slot)
                (unless (eql t (closer-mop:slot-definition-type slot))
                  (closer-mop:slot-definition-type slot)))
        (do-write-documentation stream slot t " &mdash; ")
        (format stream "~{~%    - :initarg ~/t:text/~}~@[~%    - :allocation ~/t:text/~]"
                (closer-mop:slot-definition-initargs slot)
                (unless (eql :instance (closer-mop:slot-definition-allocation slot))
                  (closer-mop:slot-definition-allocation slot)))))
    (alexandria:when-let ((methods (find-methods cls)))
      (format stream "~%~%### Methods~%")
      (dolist (method methods)
        (let ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method)))
              (lambda-list (method-specialized-lambda-list method))
              (*indent-level* 1))
          (format stream "~%- ~/t:text/"
                  (if (listp name) (second name) name))
          (do-write-documentation stream method t " &mdash; ")
          (format stream "~%~%~@/t:code/~%~%"
                  (if (listp name)
                    `(setf (,(second name) ,@(cdr lambda-list)) ,(first lambda-list))
                    `(,name ,@lambda-list))))))))



(defmethod write-documentation (stream (object symbol) (doc-type (eql nil)) detail-level)
  (format stream "~%~%# ~/t:text/ \\[symbol\\]" object)
  (write-symbol-system stream object detail-level)
  (alexandria:when-let ((clhs-link (multilang-documentation:documentation object :clhs)))
    (format stream "~%~%See also: [CLHS](~A)" clhs-link))
  (write-documentation stream object 'variable detail-level)
  (write-documentation stream object 'function detail-level)
  (write-documentation stream object 'type detail-level))

