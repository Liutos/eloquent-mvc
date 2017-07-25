# Eloquent-MVC

The Eloquent-MVC, whose name is inspired by the Spring-MVC, is a Clack-based Web framework for Common Lisp. The goal is to become the best choice of web application developing for Common Lisp programmer. Now it's still not available on Quicklisp and it's in progress.

Install this framework:

```sh
ros install Liutos/eloquent-mvc
```

## Routes

In Eloquent-MVC, a route is a list contains at least three elements: The HTTP method(a keyword), the URI(a string), and the corresponding handler(a symbol designates a function):

```lisp
((:get "/" a-handler-name)
 (:post "/" a-handler-name)
 (:put "/" a-handler-name)
 (:patch "/" a-handler-name)
 (:delete "/" a-handler-name)
 (:options "/" a-handler-name)
 (:link "/" a-handler-name)
 (:unlink "/" a-handler-name))
```

Routes are matched in the order they are defined. The first route that matches the request is invoked.

Routes with trailing slashes are the same as the ones without

```lisp
((:get "/foo" a-handler-name))          ; Also match "GET /foo/"
```

Route URI may include named parameters, accessible via the keyword arguments:

```lisp
;;; router.lisp
((:get "/hello/:name" greet))

(defun greet (request
              &key name)
  (declare (ignorable request))
  (eloquent.mvc.response:respond
   (format nil "Hello, ~A!" name)))
```

Route matching with Regular Expression:

```lisp
;;; router.lisp
((:get (:regexp "/hello/[\\w]+") greet))
```

Routes may also utilize query parameters:

```lisp
;;; router.lisp
((:get "/posts" genji-task-manager::greet))

(defun greet (request)
  (eloquent.mvc.controller:query-string-bind ((title "title")
                                              (author "author"))
      request
    (eloquent.mvc.response:respond
     (format nil "Hello, ~A, the author of ~A!" author title))))
```

## Conditions

Routes may include an additional matching condition, this is specified by the :PRECONDITION property:

```lisp
((:get "/" a-handler-name
       :precondition a-precondition)
 (:get "/" a-handler-name
       :precondition another-precondition))
```

A request will be processed by A-HANDLER-NAME if its method is GET, path is "/", and satisfies the A-PRECONDITION. If the first rule failed, the request will be submit to the second rule for checking.

## Logging

The package ELOQUENT.MVC.LOGGER provides the abilities for logging something. It reads configuration from section log in file eloquent-mvc.yaml, valid options include:

- directory. The absolute path of a directory contains log files. This path must end with a slash.

Function FORMAT exported from this package is responsible for logging. It's like the CL:FORMAT but the first parameter is a label, which indicates a file for writting logs. For example, in the function ACCESS-LOG exported from ELOQUENT.MVC.MIDDLEWARE, the label is :ACCESS then the messages were written in file access.log.

The logger switch files automatically as time goes by. The old log file will be renamed by appending a suffix of pattern YYYYMMDDhh, where YYYY for the year, MM for the month, DD for date, and hh for hour.
