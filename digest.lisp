(in-package :fw)

(defun easy-hmac (key text &key (digest-name :sha1))
  "简化一下IRONCLAD中计算HMAC摘要的步骤"
  (check-type text string)
  (when (stringp key)
    (setf key (flexi-streams:string-to-octets key)))
  (let* ((bytes (flexi-streams:string-to-octets text))
         (hmac (ironclad:make-hmac key digest-name)))
    (ironclad:update-hmac hmac bytes)
    (let ((digest (ironclad:hmac-digest hmac)))
      (string-downcase (format nil "~{~2,'0X~}" (coerce digest 'list))))))
