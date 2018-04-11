(in-package :fw)

(defparameter *default-slot-name-convertor* 'simple/convert-slot-name)

(defclass <simple-to-json> ()
  ()
  (:documentation "用于简化对JONATHAN:%TO-JSON方法的特化逻辑的编写"))

(defmethod jonathan:%to-json ((obj <simple-to-json>))
  (simple-to-json obj))

(defun my-make-instance (class initial-keys initial-values &key (slot-name-convertor *default-slot-name-convertor*))
  "生成一个CLASS类的实例对象并按照INITIAL-CONTENTS填充它"
  (check-type class symbol)
  (check-type initial-keys list)
  (check-type initial-values list)
  (let ((initargs
         (mapcan #'(lambda (key value)
                     (list (funcall slot-name-convertor key) value))
                 initial-keys initial-values)))
    (format t "~S~%" initargs)
    (apply #'make-instance class `(,@initargs :allow-other-keys t))))

(defun simple/convert-slot-name (string)
  (intern (string-upcase string) :keyword))

(defun simple-to-json (obj
                       &key
                         (allow-keys nil)
                         (key-formatter 'cl-change-case:camel-case)
                         (null-value :null))
  "将一个实例对象OBJ的每一个槽以JSON对象的方式序列化"
  (check-type allow-keys list)
  (check-type key-formatter (or function symbol))
  (let* ((clz (class-of obj))
         (slots (closer-mop:class-direct-slots clz))
         (names (mapcar #'(lambda (slot)
                            (closer-mop:slot-definition-name slot))
                        slots)))
    (jonathan:with-object
     (dolist (name names)
       (when (or (null allow-keys)
                 (position name allow-keys))
         (jonathan:write-key-value
          (funcall key-formatter (symbol-name name))
          (or (slot-value obj name) null-value)))))))
