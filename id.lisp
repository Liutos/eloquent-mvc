;;; 提供生成ID时可能需要的一些功能
(in-package :fw)

(defun make-machine-id ()
  "Return an integer as an ID constructured from one of the private IP address."
  (let ((interfaces (ip-interfaces:get-ip-interfaces)))
    (dolist (interface interfaces)
      (let ((ip (ip-interfaces:ip-interface-address interface)))
        (when (private-ip-p ip)
          (return-from make-machine-id
            (logand (ip-vector-to-integer ip)
                    #B1111111111)))))))

(defun make-sequence-no ()
  "Return an integer as the sequence part of snowflake ID."
  ;; 通过将1左移12位再减去1，即可快速得到准确的全为1的12位二进制数字
  (random (1- (ash 1 12))))

(defun make-snowflake-id ()
  "Return an integer as an ID constructured by snowflake algorithm."
  (let ((ts (get-current-ts))
        (machine-id (make-machine-id))
        (sequence-no (make-sequence-no)))
    (let ((snowflake-id (+ (ash ts 22)
                           (ash machine-id 12)
                           sequence-no)))
      (values snowflake-id
              ts
              machine-id
              sequence-no))))
