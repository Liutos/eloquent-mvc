(in-package :fw)

(defun sql-select (&rest args)
  "封装CLSQL:SELECT"
  (let ((null-value (my-getf args :null-value nil)))
    (multiple-value-bind (rows columns)
        (apply #'clsql:select `(,@args :allow-other-keys t))
      (when (null rows)
        (return-from sql-select nil))
      (mapcar #'(lambda (row)
                  (mapcar #'(lambda (value column)
                              (cons column (or value null-value)))
                          row columns))
              rows))))
