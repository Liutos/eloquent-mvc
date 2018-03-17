(in-package :fw)

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
  (let ((chars '()))
    (dotimes (i (length field))
      (let ((c (char field i)))
        (cond ((upper-case-p c)
               (when (not (char= (first chars) #\-))
                 (push #\- chars))
               (push c chars))
              ((char= c #\_)
               (when (not (char= (first chars) #\-))
                 (push #\- chars)))
              (t (push (char-upcase c) chars)))))
    (intern (concatenate 'string (nreverse chars)) :keyword)))
