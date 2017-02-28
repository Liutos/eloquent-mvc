(asdf:defsystem #:eloquent-mvc
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on ((:version #:alexandria "0.0.0")
               (:version #:cl-json "0.5.0")
               (:version #:cl-ppcre "2.0.10")
               (:version #:cl-yaml "0.1")
               (:version #:clack "2.0.0")
               #:do-urlencode
               (:version #:flexi-streams "1.0.15")
               (:version #:local-time "1.0.6")
               (:version #:optima "1.0")
               (:version #:quickproject "1.2.2")
               (:version #:salza2 "2.0.9")
               (:version #:split-sequence "1.2")
               (:version #:trivial-backtrace "1.1.0")
               (:version #:trivial-types "0.1")
               #:uiop)
  :components ((:module "src"
                        :serial t
                        :components ((:module "prelude"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "find-symbol")
                                                           (:file "make-keyword")
                                                           (:file "read-file-string")
                                                           (:file "split")
                                                           (:file "string-to-alist")
                                                           (:file "read-lines")
                                                           (:file "urldecode")
                                                           (:file "parse-query-string")
                                                           (:file "string-assoc")))
                                     (:module "config"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "config")
                                                           (:file "parse")))
                                     (:module "request"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "env-to-request")
                                                           (:file "get-header")
                                                           (:file "http-request")
                                                           (:file "request")))
                                     (:module "response"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "response")
                                                           (:file "response-to-list")
                                                           (:file "respond")))
                                     (:module "logger"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "log")
                                                           (:file "init")
                                                           (:file "format")))
                                     (:module "router"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "rule")
                                                           (:file "router")
                                                           (:file "index")))
                                     (:module "middleware"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "access-log")
                                                           (:file "apply-matched-rule")
                                                           (:file "compress")
                                                           (:file "fill-template")
                                                           (:file "handle-error")
                                                           (:file "not-found")
                                                           (:file "parse")
                                                           (:file "parse-body")
                                                           (:file "set-matched-rule")
                                                           (:file "static-file")))
                                     (:module "dispatcher"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "make-handler")))
                                     (:module "loader"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "server")
                                                           (:file "load")))
                                     (:module "controller"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "alist-bind")
                                                           (:file "json-body-bind")
                                                           (:file "query-string-bind")
                                                           (:file "url-bind")))
                                     (:module "project"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "index")))
                                     (:file "package")))))
