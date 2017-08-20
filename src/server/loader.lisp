(defpackage #:eloquent.mvc.loader
  (:use #:cl)
  (:shadow #:load)
  (:export #:load
           #:reload
           #:unload))

(in-package #:eloquent.mvc.loader)

(define-condition project-not-found-error ()
  ((directory
    :documentation "The root directory of a project"
    :initarg :directory))
  (:report (lambda (c stream)
             (format stream "Project in \"~A\" isn't started"
                     (slot-value c 'directory)))))

(define-condition not-directory-error (file-error)
  ()
  (:report (lambda (c stream)
             (format stream "\"~A\" is not an existing directory"
                     (namestring (file-error-pathname c))))))

(defvar *apps*
  (make-hash-table :test #'equal))

(defun make-config-paths (directory)
  "Returns the file path of a basic configuration file and a extented one, which is optional."
  (let ((basic (merge-pathnames "config/eloquent-mvc.yaml" directory))
        (env (uiop:getenv "CL_ENV"))
        extented)
    (when env
      (setf extented (merge-pathnames (format nil "config/eloquent-mvc.~A.yaml" env) directory)))
    (if (and extented (uiop:file-exists-p extented))
        (list basic extented)
        (list basic))))

(defun make-middleware-path (directory)
  (merge-pathnames "config/middlewares.lisp" directory))

(defun make-router-path (directory)
  (merge-pathnames "config/router.lisp" directory))

;;; export

(defvar *jobs* nil
  "A list contains all the periodic jobs loaded by START-CRON.")

(defun load (directory
             &key before-hook)
  "Load configuration, router and middlewares under DIRECTORY, and start the server for handling client request.

If BEFORE-HOOK is a function, it will be invoked before the server started."
  (check-type before-hook (or function null))
  (check-type directory pathname)
  (unless (uiop:directory-exists-p directory)
    (error 'not-directory-error :pathname directory))

  (let ((config-paths (make-config-paths directory))
        (middlewares-path (make-middleware-path directory))
        (router-path (make-router-path directory)))
    (let ((config (eloquent.mvc.config:parse config-paths))
          (key (namestring directory))
          (middlewares (eloquent.mvc.middleware:parse middlewares-path)))
      (eloquent.mvc.logger:init
       :directory (eloquent.mvc.config:get-log-directory config))
      (eloquent.mvc.router:init router-path)
      (when before-hook
        (funcall before-hook config))
      (setf (gethash key *apps*)
            (start-server config (eloquent.mvc.dispatcher:make-handler config middlewares)))
      (let ((jobs-file (eloquent.mvc.config:get-cron-jobs config))
            (log-file (eloquent.mvc.config:get-cron-log config)))
        (setf cl-cron:*cron-log-file* log-file)
        (start-cron jobs-file)))))

(defun reload (directory)
  "Shutdown and restart the application at DIRECTORY."
  (check-type directory pathname)
  (unload directory)
  (load directory))

(defun start-cron (jobs-file)
  "Read file at path JOBS-FILE, parse it as a crontab, and start to call functions periodic."
  (check-type jobs-file (or null pathname))
  (labels ((regex-capture-1 (regex target-string)
             (let ((captures (nth-value 1 (cl-ppcre:scan-to-strings regex target-string))))
               (aref captures 0)))
           (parse (jobs-file)
             (check-type jobs-file pathname)
             (let ((lines (eloquent.mvc.prelude:read-lines jobs-file)))
               (mapcar #'parse-line lines)))
           (parse-line (line)
             (check-type line string)
             (destructuring-bind (min hour day-of-month month day-of-week function-symbol)
                 (eloquent.mvc.prelude:split line #\Space)
               (let ((sym (eloquent.mvc.prelude:find-symbol* function-symbol))
                     (args (append (parse-period min :minute :step-min)
                                   (parse-period hour :hour :step-hour)
                                   (parse-period day-of-month :day-of-month :step-dom)
                                   (parse-period month :month :step-month)
                                   (parse-period day-of-week :day-of-week :step-dow))))
                 (cons sym args))))
           (parse-period (period mode-key step-key)
             (check-type period string)
             (check-type mode-key keyword)
             (check-type step-key keyword)
             (cond ((string= period "*")
                    nil)
                   ((cl-ppcre:scan "\\*/\\d+" period)
                    (let ((step (regex-capture-1 "\\*/(\\d+)" period)))
                      (list step-key (parse-integer step))))
                   ((cl-ppcre:scan "\\d+" period)
                    (let ((step (regex-capture-1 "(\\d+)" period)))
                      (list mode-key (parse-integer step))))
                   (t (error "Don't know how to parse period ~A" period)))))
    (when (null jobs-file)
      (return-from start-cron))
    (let ((jobs (parse jobs-file)))
      (when (null jobs)
        (return-from start-cron))

      (dolist (args jobs)
        (let ((job-id (apply #'cl-cron:make-cron-job args)))
          (push args *jobs*)
          (push job-id *jobs*)))
      (let ((cl-cron:*cron-load-file* nil))
        (cl-cron:start-cron)))))

(defun start-server (config app)
  (let ((port (eloquent.mvc.config:get-server-port config))
        (server (eloquent.mvc.config:get-server-server config)))
    (clack:clackup app
                   :port port
                   :server server)))

(defun stop-cron ()
  "Stop all jobs stored in *JOBS*."
  (loop
     :for job-id :in *jobs* :by #'cddr
     :do (cl-cron:delete-cron-job job-id))
  (cl-cron:stop-cron))

(defun stop-server (handler)
  (clack:stop handler))

(defun unload (directory
               &key before-hook)
  "Unbind the listen on port and stop the server thread."
  (check-type before-hook (or function null))
  (check-type directory pathname)
  (let ((key (namestring directory)))
    (multiple-value-bind (handler found)
        (gethash key *apps*)
      (unless found
        (error 'project-not-found-error :directory directory))
      (stop-cron)
      (when before-hook
        (funcall before-hook))
      (stop-server handler)
      (remhash key *apps*))))
