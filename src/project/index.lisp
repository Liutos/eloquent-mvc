(in-package #:eloquent.mvc.project)

(define-condition directory-dup-error (file-error)
  ()
  (:report (lambda (c stream)
             (format stream "Directory \"~A\" is already existing"
                     (file-error-pathname c)))))

(defun make-project (directory
                     &key
                       depends-on)
  "Create a directory whose structure is satisfied with Eloquent-MVC."
  (check-type directory pathname)
  (when (uiop:directory-exists-p directory)
    (error 'directory-dup-error :pathname directory))

  (pushnew '#:eloquent-mvc depends-on)
  (quickproject:make-project
   directory
   :author (uiop:getenv "USERNAME")
   :depends-on depends-on
   :template-directory (merge-pathnames "src/project/template/"
                                        (asdf:system-source-directory :eloquent-mvc))))
