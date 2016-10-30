(in-package #:eloquent.mvc.project)

(defun make-project (pathname
                     &key
                       depends-on)
  "Create a directory whose structure is satisfied with Eloquent-MVC"
  (pushnew '#:eloquent-mvc depends-on)
  (quickproject:make-project
   pathname
   :author (uiop:getenv "USERNAME")
   :depends-on depends-on
   :template-directory (merge-pathnames "src/project/template/"
                                        (asdf:system-source-directory :eloquent-mvc))))
