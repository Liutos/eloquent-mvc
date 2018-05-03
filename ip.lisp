;;; 提供处理IP地址时可能需要的一些功能
(in-package :fw)

(defun ip-in-range-p (ip min max)
  "Return T if IP is inside the closing range whose boundary is MIN and MAX."
  (check-type ip (simple-vector 4))
  (check-type min (simple-vector 4))
  (check-type max (simple-vector 4))
  (let ((target (ip-vector-to-integer ip))
        (boundary-min (ip-vector-to-integer min))
        (boundary-max (ip-vector-to-integer max)))
    (and (>= target boundary-min)
         (<= target boundary-max))))

(defun ip-vector-to-integer (ip)
  "Return an integer equals to the IP vector.

For example, the vector form of IP address 127.0.0.1 is #(127 0 0 1), and passing it to this function will return an integer 2130706433."
  (check-type ip (simple-vector 4))
  (reduce #'(lambda (result element)
              (+ (ash result 8) element))
          ip))

(defun private-ip-p (ip)
  "Return T if IP is a private IP address.

An IP address is private when it:
1. Within 10.0.0.0 to 10.255.255.255, or
2. Within 172.16.0.0 to 172.31.255.255, or
3. Within 192.168.0.0 to 192.168.255.255"
  (check-type ip (simple-vector 4))
  (or (ip-in-range-p ip #(10 0 0 0) #(10 255 255 255))
      (ip-in-range-p ip #(172 16 0 0) #(172 31 255 255))
      (ip-in-range-p ip #(192 168 0 0) #(192 168 255 255))))
