(in-package #:eloquent.mvc.config)

(defun parse (filename)
  "Read content from a file specified by FILENAME, parse it and return the parsing result."
  (declare (type pathname filename))
  (let* ((env `(:user-homedir-pathname ,(user-homedir-pathname)))
         (text (cl-emb:execute-emb filename :env env))
         (content (cl-yaml:parse text)))
    (make-instance '<config>
                   :content content)))
