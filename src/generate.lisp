(in-package #:tanstaafl)


(defun generate (package-name path)
  (let* ((package (find-package package-name))
        (root (merge-pathnames (make-pathname :directory (list :relative (string-downcase (package-name package))))
                               path)))
    (when package
      (ensure-directories-exist root)
      (with-open-file (stream (merge-pathnames (make-pathname :name "index"
                                                              :type "md")
                                               root)
                              :direction :output :if-exists :supersede)
        (write-documentation stream package nil 1))
      (do-external-symbols (sym package)
        (with-open-file (stream (merge-pathnames (make-pathname :name (string-downcase (symbol-name sym))
                                                                :type "md")
                                                 root)
                                :direction :output :if-exists :supersede)
          (write-documentation stream sym nil 1))))))

