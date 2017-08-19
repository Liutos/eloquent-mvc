(in-package #:eloquent.mvc.prelude.jwt)

(defun sign (payload secret-key)
  "Constructs a JWT string according to PAYLOAD and SECRET-KEY."
  (check-type payload (trivial-types:association-list string))
  (check-type secret-key string)
  (flet ((encode-json-base64 (alist)
           (check-type alist (trivial-types:association-list string))
           (eloquent.mvc.prelude.base64:encode
            (cl-json:encode-json-alist-to-string alist)
            :uri t))
         (string-to-octets (s)
           (flexi-streams:string-to-octets s :external-format :utf-8)))
    (let* ((header '(("typ" . "JWT")
                     ("alg" . "HS256")))
           (header (encode-json-base64 header))
           (payload (encode-json-base64 payload))
           (plain (str:concat header "." payload))

           (secret-key (string-to-octets secret-key))
           (hmac (ironclad:make-hmac secret-key :sha256))
           (digest (progn
                     (ironclad:update-hmac hmac (string-to-octets plain))
                     (ironclad:hmac-digest hmac))))
      (values digest
              header
              payload
              plain
              (eloquent.mvc.prelude.base64:encode digest :uri t)))))
