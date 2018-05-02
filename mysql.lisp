(in-package :fw)

(defun connect-to-mysql (&key database host password port user)
  (declare (ignorable port))
  (clsql:connect (list host database user password))
  (clsql:query "SET NAMES utf8"))

(defun make-element (row columns element-type
                     &key (null-value nil))
  (cond ((eq element-type :alist)
         (mapcar #'(lambda (value column)
                     (cons column (or value null-value)))
                 row columns))
        ((eq element-type :plist)
         (mapcan #'(lambda (value column)
                     (list (intern (string-upcase column) :keyword)
                           (or value null-value)))
                 row columns))
        ((and (listp element-type)
              (eq (first element-type) :instance))
         (make-element-as-instance (second element-type) columns row))))

(defun make-element-as-instance (class columns row)
  "根据从数据库中返回的列尝试构造一个实例对象"
  (fw::my-make-instance class columns row
                        :slot-name-convertor 'table-field-to-key))

(defun sql-select (&rest args)
  "封装CLSQL:SELECT"
  (let ((element-type (my-getf args :x-element-type :alist))
        (null-value (my-getf args :x-null-value nil))
        (singlep (my-getf args :x-singlep nil)))
    (multiple-value-bind (rows columns)
        (apply #'clsql:select `(,@args :allow-other-keys t))
      (when (null rows)
        (return-from sql-select nil))
      (let ((rows
             (mapcar #'(lambda (row)
                         (make-element row columns element-type
                                       :null-value null-value))
                     rows)))
        (if singlep (first rows) rows)))))

(defun table-field-to-key (field)
  "将数据库表的列名转换为更具lisp风格的关键字符号"
  (check-type field string)
  (cl-arrows:-> field
                cl-change-case:param-case
                string-upcase
                (intern :keyword)))
