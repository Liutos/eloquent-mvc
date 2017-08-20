(asdf:defsystem #:eloquent-mvc
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on ((:version #:alexandria "0.0.0")
               #:cl-emb
               (:version #:cl-base64 "3.1")
               (:version #:cl-cron "0.1")
               (:version #:cl-json "0.5.0")
               (:version #:cl-ppcre "2.0.10")
               (:version #:cl-yaml "0.1")
               (:version #:clack "2.0.0")
               #:do-urlencode
               (:version #:flexi-streams "1.0.15")
               #:ironclad
               (:version #:local-time "1.0.6")
               (:version #:optima "1.0")
               (:version #:quickproject "1.2.2")
               (:version #:salza2 "2.0.9")
               (:version #:split-sequence "1.2")
               #:str
               (:version #:trivial-backtrace "1.1.0")
               (:version #:trivial-types "0.1")
               #:uiop)
  :components
  ((:module "src"
            :components
            ((:file "config")
             (:file "logger" :depends-on ("base" "config"))
             (:module "base"
                      :components
                      ((:file "package")
                       (:file "alist" :depends-on ("package"))
                       (:file "cookie" :depends-on ("alist"))
                       (:file "io" :depends-on ("split"))
                       (:file "split" :depends-on ("package"))
                       (:file "symbol" :depends-on ("split"))
                       (:file "time" :depends-on ("package"))
                       (:file "uri" :depends-on ("alist"))))
             (:module "cli"
                      :components
                      ((:module "project"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "index"))))
                      :depends-on ("base"))
             (:module "http"
                      :components
                      ((:file "request")
                       (:file "response")
                       (:file "router" :depends-on ("request")))
                      :depends-on ("cli"))
             (:module "contrib"
                      :components
                      ((:module "middleware"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "index")
                                 (:file "access-log")
                                 (:file "apply-matched-rule")
                                 (:file "compress")
                                 (:file "fill-template")
                                 (:file "handle-error")
                                 (:file "not-found")
                                 (:file "parse")
                                 (:file "parse-body")
                                 (:file "set-matched-rule")
                                 (:file "static-file"))))
                      :depends-on ("http"))
             (:module "server"
                      :components
                      ((:module "dispatcher"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "make-handler")))
                       (:module "loader"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "cron")
                                 (:file "server")
                                 (:file "load"))))
                      :depends-on ("contrib"))
             (:module "mvc"
                      :serial t
                      :components
                      ((:file "package")
                       (:file "alist-bind")
                       (:file "form-bind")
                       (:file "json-body-bind")
                       (:file "query-string-bind")
                       (:file "url-bind"))
                      :depends-on ("server"))
             (:file "package" :depends-on ("mvc"))))))
