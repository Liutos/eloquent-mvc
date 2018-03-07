(in-package :fw)

(defvar *routes* '())

(defun add-route (method path f)
  (let (pattern)
    (if (find #\: path)
        (let ((regex (cl-ppcre:regex-replace-all
                      ":[^/]+" path "([^/]+)")))
          (setf pattern (cl-ppcre:parse-string regex)))
        (setf pattern path))

    (push (list method pattern f path)
          *routes*)))

(defun extract-from-path (env route)
  (let ((path (path-of env))
        (pattern (fourth route)))
    (let ((group-pattern (cl-ppcre:regex-replace-all ":[^/]+" pattern "([^/]+)"))
          (names (cl-ppcre:all-matches-as-strings ":[^/]+" pattern)))
      (let ((matches (nth-value 1 (cl-ppcre:scan-to-strings group-pattern path))))
        (map 'list #'(lambda (name match)
                       (cons (subseq name 1) match))
             names matches)))))

(defun find-route (method path)
  (find-if #'(lambda (route)
               (flet ((path= (expect real)
                        (etypecase expect
                          (list (cl-ppcre:scan expect real))
                          (string (string= expect real)))))
                 (and (eq (first route) method)
                      (path= (second route) path))))
           *routes*))

(defun route-not-found (env)
  (let ((method (method-of env))
        code)
    (if (or (eq method :get)
            (eq method :head))
        (setf code 404)
        (setf code 405))
    (make-http-response "" code nil)))

(defun show-routes ()
  (dolist (route *routes*)
    (format t "~A~A~A~A~A~%"
            (first route)
            #\Tab
            (fourth route)
            #\Tab
            (third route))))
