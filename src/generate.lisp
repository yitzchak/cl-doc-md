(in-package #:tanstaafl)


(defun generate (package-name path)
  (let ((package (find-package package-name)))
    (when package
      (ensure-directories-exist path)
      (with-open-file (stream (merge-pathnames (make-pathname :name (sanitize (package-name package))
                                                              :type "md")
                                               path)
                              :direction :output :if-exists :supersede)
        (write-documentation stream package nil 1)
      (dolist (sym (exported-symbols package))
        (write-documentation stream sym nil 1))))))

