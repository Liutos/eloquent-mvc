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

(defun easy-sha1 (text)
  "简化IRONCLAD提供的计算SHA1的函数的使用"
  (let ((bytes (flexi-streams:string-to-octets text))
        (digester (ironclad:make-digest :sha1)))
    (ironclad:update-digest digester bytes)
    (let ((digest (ironclad:produce-digest digester)))
      (string-downcase (format nil "~{~2,'0X~}" (coerce digest 'list))))))
