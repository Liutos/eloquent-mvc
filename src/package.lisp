(defpackage #:eloquent.mvc
  (:use #:cl)
  (:shadow #:load)
  (:export #:load
           #:make-project
           #:show
           #:try-request
           #:unload))

(in-package #:eloquent.mvc)

(defun load (&rest args)
  #.(documentation 'eloquent.mvc.loader:load 'function)
  (apply #'eloquent.mvc.loader:load args))

(defun make-project (&rest args)
  #.(documentation 'eloquent.mvc.project:make-project 'function)
  (apply #'eloquent.mvc.project:make-project args))

(defun show (&rest args)
  #.(documentation 'eloquent.mvc.router:show 'function)
  (apply #'eloquent.mvc.router:show args))

(defun try-request (&rest args)
  #.(documentation 'eloquent.mvc.router:try-request 'function)
  (apply #'eloquent.mvc.router:try-request args))

(defun unload (&rest args)
  #.(documentation 'eloquent.mvc.loader:unload 'function)
  (apply #'eloquent.mvc.loader:unload args))
