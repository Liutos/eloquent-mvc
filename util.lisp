;;; 在这里提供的都是一些除了package.lisp之外不需要依赖这个项目中的其它文件即可实现的功能
(in-package :fw)

(defmacro define-my-class ((name &rest my-options &key auto-reader auto-initarg) direct-super-classes direct-slots &rest options)
  "更方便使用的defclass"
  (declare (ignorable my-options))
  (labels ((append-initarg (slot-form)
             (if auto-initarg
                 (append slot-form (list :initarg (make-initarg-name slot-form)))
                 slot-form))
           (append-reader (slot-form)
             (if auto-reader
                 (append slot-form (list :reader (make-reader-name slot-form)))
                 slot-form))
           (make-initarg-name (slot-form)
             (let ((slot-name (first slot-form)))
               (intern (symbol-name slot-name) :keyword)))
           (make-reader-name (slot-form)
             (let ((slot-name (first slot-form)))
               (intern (format nil "~A-OF" (symbol-name slot-name))))))
    (let ((slots
           (mapcar #'(lambda (slot)
                       (unless (listp slot)
                         (setf slot (list slot)))
                       (setf slot (append-initarg slot))
                       (setf slot (append-reader slot))
                       slot)
                   direct-slots)))
      `(defclass ,name ,direct-super-classes
         ,slots
         ,@options))))

(defun alist-keys (alist)
  "提取出ALIST中的所有元素的CAR部分"
  (mapcar #'car alist))

(defun assoc-string (item alist &key key (after #'cdr))
  (let ((c (assoc item alist :key key :test #'string=)))
    (if (or (null c) (null after))
        c
        (funcall after c))))

(defun octets-from-vector-stream (s &key (external-format :utf-8))
  "用于从经过HTTP-BODY:PARSE解析的multipart/form-data数据中提取出字节内容"
  (check-type s flexi-streams:vector-stream)
  (let* ((size (* 1024 1024))
         (buffer (make-array size :element-type '(unsigned-byte 8)))
         (count (read-sequence buffer s)))
    (setf buffer (subseq buffer 0 count))
    (cond ((null external-format)
           buffer)
          (t (flexi-streams:octets-to-string buffer
                                             :external-format external-format)))))

(defun get-current-ts ()
  (let* ((now (local-time:now))
         (unix-part (local-time:timestamp-to-unix now))
         (nsec-part (local-time:nsec-of now)))
    (+ (* 1000 unix-part)
       (truncate nsec-part 1000000))))

(defun ksort (alist)
  "模仿PHP的ksort函数"
  (sort alist
        #'string<
        :key #'car))

(defun make-query-string (alist &key keys)
  "根据ALIST构造出query string"
  (apply
   #'str:join
   "&"
   (list (mapcar #'(lambda (kv)
                     (destructuring-bind (k . v) kv
                       (str:join "=" (list k v))))
                 (remove-if #'(lambda (kv)
                                (and keys
                                     (null (position (first kv) keys :test #'string=))))
                            alist)))))

(defun my-getf (place indicator &optional default)
  (let ((i (position indicator place)))
    (if (numberp i)
        (nth (1+ i) place)
        default)))

(defun override-plist (p1 p2)
  (let ((new-p1 (copy-list p1)))
    (alexandria:doplist (k v p2)
      (setf (getf new-p1 k) v))
    new-p1))
