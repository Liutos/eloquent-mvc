(in-package :fw)

(defun camel-case-formatter (name)
  (let ((i 0)
        (length (length name))
        (result '()))
    (loop
       (when (>= i length)
         (return))
       (let ((c (char name i)))
         (cond ((or (char= c #\-) (char= c #\_))
                (when (< i (1- length))
                  (incf i)
                  (let ((nc (char name i)))
                    (push (char-upcase nc) result)))
                (incf i))
               (t
                (push (char-downcase c) result)
                (incf i)))))
    (concatenate 'string (nreverse result))))

(defun simple-to-json (obj
                       &key
                         (key-formatter 'camel-case-formatter)
                         (null-value :null))
  (let* ((clz (class-of obj))
         (slots (closer-mop:class-direct-slots clz))
         (names (mapcar #'(lambda (slot)
                            (closer-mop:slot-definition-name slot))
                        slots)))
    (jonathan:with-object
     (dolist (name names)
       (jonathan:write-key-value
        (funcall key-formatter (symbol-name name))
        (or (slot-value obj name) null-value))))))
