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

(defun assoc-string (item alist &key key (after #'cdr))
  (let ((c (assoc item alist :key key :test #'string=)))
    (if (or (null c) (null after))
        c
        (funcall after c))))

(defun get-current-ts ()
  (let* ((now (local-time:now))
         (unix-part (local-time:timestamp-to-unix now))
         (nsec-part (local-time:nsec-of now)))
    (+ (* 1000 unix-part)
       (truncate nsec-part 1000000))))

(defun my-getf (place indicator &optional default)
  (let ((i (position indicator place)))
    (if (numberp i)
        (nth (1+ i) place)
        default)))
