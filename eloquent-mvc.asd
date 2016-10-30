(asdf:defsystem #:eloquent-mvc
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:alexandria
               #:cl-json
               #:cl-ppcre
               #:cl-yaml
               #:clack
               #:do-urlencode
               #:flexi-streams
               #:local-time
               #:optima
               #:quickproject
               #:salza2
               #:split-sequence
               #:trivial-types
               #:uiop)
  :components ((:module "src"
                        :serial t
                        :components ((:module "prelude"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "dolist")
                                                           (:file "equivalent")
                                                           (:file "find-symbol")
                                                           (:file "make-keyword")
                                                           (:file "read-file-string")
                                                           (:file "split")
                                                           (:file "string-to-alist")
                                                           (:file "read-lines")
                                                           (:file "urldecode")
                                                           (:file "parse-query-string")))
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
                                                           (:file "get")
                                                           (:file "parse")))
                                     (:module "dispatcher"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "make-handler")))
                                     (:module "middleware"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "access-log")
                                                           (:file "compress")
                                                           (:file "parse")
                                                           (:file "parse-body")
                                                           (:file "static-file")))
                                     (:module "loader"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "server")
                                                           (:file "load")))
                                     (:module "controller"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "json-body-bind")
                                                           (:file "query-string-bind")
                                                           (:file "url-bind")))
                                     (:module "project"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "index")))
                                     (:file "package")))))
