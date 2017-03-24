(in-package #:eloquent.mvc.config)

(defgeneric parse (config-path)
  (:documentation "Reads file content from CONFIG-PATH, one or many, and returns an instance contains configurations."))

(defmethod parse ((config-paths list))
  "Read content from many files specified in CONFIG-PATHS, embed one into another, and returns a single instance contains all the configurations."
  (labels ((aux (base rest)
             (cond ((null rest) base)
                   (t (let* ((filename (first rest))
                             (config (parse filename)))
                        (setf (config-base config) base)
                        (aux config (rest rest)))))))
    (aux nil config-paths)))

(defmethod parse ((filename pathname))
  "Read content from a file specified by FILENAME, parse it and return the parsing result."
  (let* ((env `(:user-homedir-pathname ,(user-homedir-pathname)))
         (text (cl-emb:execute-emb filename :env env))
         (content (cl-yaml:parse text)))
    (make-instance '<config>
                   :content content)))
