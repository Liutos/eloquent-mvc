(in-package :fw)

(annot:defannotation handle/get (path f)
    (:arity 2)
  `(progn
     ,f
     (add-route :get ,path ',(second f))))

(annot:defannotation handle/post (path f)
    (:arity 2)
  `(progn
     ,f
     (add-route :post ,path ',(second f))))

(annot:defannotation handler (method path f)
    (:arity 3)
  (let ((name (second f)))
    `(progn
       ,f
       (add-route ,method ,path ',name))))

(defmacro define-request-handler ((name
                                   &key for when
                                   &aux (method when) (path for)) args &body body)
  "Define a function named NAME with parameters specified by ARGS and
execution logics by BODY, and tell the framework to call this function
when a HTTP whose verb is WHEN and path is FOR comes in."
  `(progn
     (defun ,name ,args ,@body)
     (fw::add-route ,method ,path ',name)))
