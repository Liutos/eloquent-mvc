(in-package :fw)

(defun make-element (row columns element-type
                     &key (null-value nil))
  (ecase element-type
    (:alist (mapcar #'(lambda (value column)
                        (cons column (or value null-value)))
                    row columns))
    (:plist (mapcan #'(lambda (value column)
                        (list (intern (string-upcase column) :keyword)
                              (or value null-value)))
                    row columns))))

(defun sql-select (&rest args)
  "封装CLSQL:SELECT"
  (let ((element-type (my-getf args :element-type :alist))
        (null-value (my-getf args :null-value nil)))
    (multiple-value-bind (rows columns)
        (apply #'clsql:select `(,@args :allow-other-keys t))
      (when (null rows)
        (return-from sql-select nil))
      (mapcar #'(lambda (row)
                  (make-element row columns element-type
                                :null-value null-value))
              rows))))
