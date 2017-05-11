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
