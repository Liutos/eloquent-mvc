(in-package :fw)

(defclass <cos-client> ()
  ((app-id
    :reader app-id-of
    :initarg :app-id)
   (secret-id
    :reader secret-id-of
    :initarg :secret-id)
   (secret-key
    :reader secret-key-of
    :initarg :secret-key)))

(defun make-cos-client (app-id secret-id secret-key)
  (make-instance '<cos-client>
                 :app-id app-id
                 :secret-id secret-id
                 :secret-key secret-key))

(defun make-authorization (cos-client method path header-alist url-param-alist
                           &key (expire (* 5 60)) expire-at expire-from)
  "生成用于腾讯云COS用的签名字符串

EXPIRE为签名的过期时间，单位为秒
EXPIRE-FROM为签名开始生效的时间，为秒级的UNIX时间戳。默认为从当前时间开始"
  (check-type cos-client <cos-client>)
  (let* ((sorted-headers (ksort header-alist))
         (sorted-url-param (ksort url-param-alist))
         (q-header-list (alist-keys sorted-headers))
         (q-url-param-list (alist-keys sorted-url-param))
         (q-sign-time (make-q-sign-time expire-from expire expire-at))
         (authorization-alist
          `(("q-sign-algorithm" . "sha1")
            ("q-ak" . ,(secret-id-of cos-client))
            ("q-sign-time" . ,q-sign-time)
            ("q-key-time" . ,q-sign-time)
            ("q-header-list" . ,(str:join ";" q-header-list))
            ("q-url-param-list" . ,(str:join ";" q-url-param-list))
            ("q-signature" . ,(make-signature cos-client
                                              method
                                              path
                                              (make-query-string header-alist :keys q-header-list)
                                              q-sign-time
                                              (make-query-string url-param-alist :keys q-url-param-list))))))
    (values (make-query-string authorization-alist)
            authorization-alist)))

(defun make-http-string (method path q-header q-url-param)
  (str:join
   (format nil "~%")
   (list (string-downcase (symbol-name method))
         path
         q-url-param
         q-header
         "")))

(defun make-q-sign-time (expire-from expire expire-at)
  (unless expire-from
    (setf expire-from (truncate (get-current-ts) 1000)))
  (unless expire-at
    (setf expire-at (+ expire-from expire)))
  (format nil "~D;~D" expire-from expire-at))

(defun make-signature (cos-client method path q-header q-key-time q-url-param)
  (check-type cos-client <cos-client>)
  (let* ((sign-key (easy-hmac (secret-key-of cos-client) q-key-time))
         (http-string (make-http-string method path q-header q-url-param))
         (http-string-sha1 (easy-sha1 http-string))
         (string-to-sign (make-string-to-sign http-string-sha1 q-key-time)))
    (easy-hmac sign-key string-to-sign)))

(defun make-string-to-sign (http-string-sha1 q-key-time)
  (str:join
   (format nil "~%")
   (list "sha1"
         q-key-time
         http-string-sha1
         "")))

(defun put-object (cos-client bucket region path payload)
  "将PAYLOAD中的数据以PATH为名存储到腾讯云对象存储的BUCKET位置下"
  (check-type cos-client <cos-client>)
  (check-type bucket string)
  (check-type region string)
  (check-type path string)
  (check-type payload (or string (vector (unsigned-byte 8))))

  (when (stringp payload)
    (setf payload (flexi-streams:string-to-octets payload)))

  (let* ((host (format nil "~A.cos.~A.myqcloud.com" bucket region))
         (uri (format nil "https://~A~A" host path))
         (method :put)
         (authorization
          (make-authorization cos-client
                              method
                              path
                              `(("host" . ,host))
                              '())))
    (fw::http-request
     uri
     :additional-headers `(("Authorization" . ,authorization))
     :content payload
     :method method)
    uri))

(defun test/make-authorization ()
  (let* ((cos-client (make-cos-client
                      "1254000000"
                      "AKIDQjz3ltompVjBni5LitkWHFlFpwkn9U5q"
                      "BQYIM75p8x0iWVFSIgqEKwFprpRSVHlz"))
         (header-alist
          `(("host" . "bucket1-1254000000.cos.ap-beijing.myqcloud.com")
            ("x-cos-content-sha1" . "7b502c3a1f48c8609ae212cdfb639dee39673f5e")
            ("x-cos-storage-class" . "nearline")))
         (url-param-alist
          `()))
    (multiple-value-bind (authorization alist)
        (make-authorization cos-client
                            :put
                            "/testfile2"
                            header-alist
                            url-param-alist
                            :expire-at 1417853898
                            :expire-from 1417773892)
      (declare (ignorable authorization))
      (assert (string= (assoc-string "q-signature" alist)
                       "84f5be2187452d2fe276dbdca932143ef8161145")))))
