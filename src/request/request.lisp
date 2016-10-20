(in-package #:eloquent.mvc.request)

(defclass <request> ()
  ((clack.streaming :documentation "Directly from Clack"
                    :initarg :clack.streaming
                    :type boolean)
   (content-length :documentation "The Content-Length field in request headers"
                   :initarg :content-length
                   :reader request-content-length
                   :type fixnum)
   (content-type :documentation "The Content-Type field in request headers"
                 :initarg :content-type
                 :reader request-content-type
                 :type string)
   (headers :documentation "All HTTP headers from request"
            :initarg :headers
            :reader request-headers
            :type hash-table)
   (path-info :documentation "Directly from Clack"
              :initarg :path-info
              :reader request-path-info
              :type string)
   (query-string :documentation "The query string part in request URL"
                 :initarg :query-string
                 :reader request-query-string
                 :type string)
   (raw-body :documentation "A input stream contains the HTTP body's content"
             :initarg :raw-body
             :reader request-raw-body
             :type stream)
   (remote-addr :documentation "The IP address of the client"
                :initarg :remote-addr
                :reader request-remote-addr
                :type string)
   (remote-port :documentation "The port bind by the client-side socket"
                :initarg :remote-port
                :reader request-remote-port
                :type fixnum)
   (request-method :documentation "The HTTP method used by client"
                   :initarg :request-method
                   :reader request-method
                   :type keyword)
   (request-uri :documentation "Directly from Clack"
                :initarg :request-uri
                :reader request-uri
                :type string)
   (script-name :documentation "Directly from Clack"
                :initarg :script-name
                :reader request-script-name
                :type string)
   (server-name :documentation "Directly from Clack"
                :initarg :server-name
                :reader request-server-name
                :type string)
   (server-port :documentation "The port listen by the server-side socket"
                :initarg :server-port
                :reader request-server-port
                :type fixnum)
   (server-protocol :documentation "Directly from Clack"
                    :initarg :server-protocol
                    :reader request-server-protocol
                    :type keyword)
   (url-scheme :documentation "The scheme in the request URL"
               :initarg :url-scheme
               :reader request-url-scheme
               :type keyword))
  (:documentation "A HTTP request"))
