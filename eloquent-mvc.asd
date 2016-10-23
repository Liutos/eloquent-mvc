(asdf:defsystem #:eloquent-mvc
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:alexandria
               #:clack
               #:local-time
               #:optima
               #:py-configparser
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
                                                           (:file "read-lines")))
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
                                                           (:file "request")))
                                     (:module "response"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "response")
                                                           (:file "response-to-list")
                                                           (:file "respond")))
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
                                                           (:file "parse")))
                                     (:module "loader"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "server")
                                                           (:file "load")
                                                           (:file "unload")))
                                     (:module "controller"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "query-string-bind")))
                                     (:file "package")))))
