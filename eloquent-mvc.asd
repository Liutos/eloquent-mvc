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
            :serial t
            :components
            ((:module "base"
                      :serial t
                      :components
                      ((:module "base64"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "export")))
                       (:module "jwt"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "export")))
                       (:file "alist" :depends-on ("package"))
                       (:file "package")
                       (:file "find-symbol")
                       (:file "io" :depends-on ("package" "split"))
                       (:file "make-keyword")
                       (:file "parse-cookie-string" :depends-on ("alist" "package"))
                       (:file "split")
                       (:file "time" :depends-on ("package"))
                       (:file "uri" :depends-on ("alist" "package"))
                       (:file "index")))
             (:module "cli"
                      :components
                      ((:module "project"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "index")))))
             (:module "config"
                      :serial t
                      :components
                      ((:file "package")
                       (:file "config")
                       (:file "parse")))
             (:module "http"
                      :components
                      ((:module "request"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "env-to-request")
                                 (:file "get-cookie")
                                 (:file "get-header")
                                 (:file "http-request")
                                 (:file "request")))
                       (:module "response"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "encoder")
                                 (:file "response")
                                 (:file "response-to-list")
                                 (:file "respond")))
                       (:module "router"
                                :serial t
                                :components
                                ((:file "package")
                                 (:file "rule")
                                 (:file "router")
                                 (:file "index")))))
             (:module "logger"
                      :serial t
                      :components
                      ((:file "package")
                       (:file "log")
                       (:file "init")
                       (:file "format")))
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
                                 (:file "static-file")))))
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
                                 (:file "load")))))
             (:module "mvc"
                      :serial t
                      :components
                      ((:file "package")
                       (:file "alist-bind")
                       (:file "form-bind")
                       (:file "json-body-bind")
                       (:file "query-string-bind")
                       (:file "url-bind")))
             (:file "package")))))
