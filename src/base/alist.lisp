(in-package #:eloquent.mvc.base)

(defun compute-value (raw
                      &key
                        default
                        path
                        requirep
                        type)
  "Validate and convert RAW to another form according to the keyword arguments."
  (when (and requirep (null raw))
    (error 'alist-bind-error
           :message (format nil "缺少`~A`参数" path)))
  (let ((value raw))
    (when type
      (ecase type
        (:integer (etypecase value
                    (integer)
                    (null)
                    (string (if (string/= value "")
                                (setf value (parse-integer value))
                                (setf value nil)))))))
    (when (and default (null value))
      (setf value default))
    value))

;;; export

(define-condition alist-bind-error ()
  ((message
    :initarg :message
    :reader alist-bind-error-message
    :type string))
  (:report (lambda (c stream)
             (format stream "~A" (alist-bind-error-message c)))))

(defmacro alist-bind (bindings alist-expr &body body)
  "Bind variables to values extracted from ALIST, and evaluate the BODY."
  (alexandria:with-gensyms (alist)
    (labels ((wrap-value (expr &rest args)
               `(compute-value ,expr ,@args))
             (make-binding (binding)
               (unless (stringp (cadr binding))
                 (let ((var (first binding)))
                   (setf binding
                         `(,var
                           ,(string-downcase (symbol-name var))
                           ,@(rest binding)))))

               (destructuring-bind (var field &rest args) binding
                 `(,var ,(apply #'wrap-value
                                `(eloquent.mvc.base:alist-get ,alist ,field)
                                :path field
                                args)))))
      (let ((bindings (mapcar #'make-binding bindings)))
        `(let* ((,alist ,alist-expr)
                ,@bindings)
           ,@body)))))

(defun alist-get (alist key)
  "Finds the cons in ALIST whose key is KEY and returns the associated cdr and T as secondary value, or returns NIL and NIL if there is no such a cons. Cons can be added using SETF."
  (check-type alist (trivial-types:association-list string))
  (check-type key string)
  (let ((cons (assoc key alist :test #'string=)))
    (and cons (cdr cons))))

(defun (setf alist-get) (value alist key)
  "Sets the cdr of cons in ALIST whose key is KEY to VALUE, or adds such a cons to ALIST if there is no such one."
  (check-type key string)
  (check-type alist (trivial-types:association-list string))
  (let ((cons (assoc key alist :test #'string=)))
    (setf (cdr cons) value)))

(defun decode-form-to-alist (text group-delimiter value-delimiter
                        &key trim-key-p)
  "Return an assoiate-list consist of groups separated by group-delimiter in text, each group contains two components separated by value-delimiter in text.

If in a group, the content at the right-hand-side of value-delimiter is empty, then it will be an empty string in the associate-list."
  (check-type text string)
  (check-type group-delimiter character)
  (check-type value-delimiter character)
  (flet ((parse-key (key)
           (if trim-key-p
               (string-trim '(#\ ) key)
               key)))
    (mapcar #'(lambda (group)
                (optima:match (split group value-delimiter :remove-empty-subseqs t)
                  ((list key) (cons (parse-key key) ""))
                  ((list key value) (cons (parse-key key) value))))
            (split text group-delimiter
                   :remove-empty-subseqs t))))

(defun decode-json-to-alist (text)
  "Return an alist parsing from TEXT."
  (check-type text string)
  (let ((cl-json:*identifier-name-to-key* #'identity)
        (cl-json:*json-identifier-name-to-lisp* #'identity))
    (cl-json:decode-json-from-string text)))

(defun encode-alist-to-json (alist)
  "Return a string serialized from ALIST."
  (check-type alist (trivial-types:association-list string))
  (let ((cl-json:*identifier-name-to-key* #'identity)
        (cl-json:*lisp-identifier-name-to-json* #'identity))
    (cl-json:encode-json-to-string alist)))
