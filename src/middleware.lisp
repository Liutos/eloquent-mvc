(defpackage #:eloquent.mvc.middleware
  (:use #:cl)
  (:export #:*middlewares*
           #:parse))

(in-package #:eloquent.mvc.middleware)

(define-condition symbol-not-export-error (error)
  ((package
    :documentation "The name of the package should export the symbol"
    :initarg :package
    :type string)
   (symbol
    :documentation "The name of the un-exported symbol"
    :initarg :symbol
    :type string))
  (:report (lambda (c stream)
             (with-slots (package symbol) c
               (format stream "Symbol ~A is not exported from package ~A" symbol package)))))

(defun check-export (symbol)
  "Signal a condition if the SYMBOL is not exported from its package."
  (check-type symbol symbol)
  (let ((package (symbol-package symbol))
        (name (symbol-name symbol)))
    (let ((exportp (nth-value 1 (find-symbol name package))))
      (unless (eql exportp :external)
        (error 'symbol-not-export-error :package package :symbol name)))))

;;; export

(defvar *middlewares* nil)

(defun parse (file)
  "Read the middlewares specified in FILE."
  (check-type file pathname)
  (with-open-file (stream file)
    (let ((middlewares (read stream)))
      (dolist (symbol middlewares)
        (check-export symbol))
      middlewares)))
