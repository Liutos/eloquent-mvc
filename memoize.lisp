(in-package :fw)

(defvar *memory-cache*
  (make-hash-table :test #'equal))

(defclass <memory-cache-entry> ()
  ((expire
    :reader expire-of
    :initarg :expire)
   (expire-at
    :reader expire-at-of
    :initarg :expire-at)
   (value
    :reader value-of
    :initarg :value))
  (:documentation "使用内存缓存时的存储结构"))

(defun make-cache-entry (value expire)
  (check-type expire integer)
  (let (expire-at)
    (when expire
      (setf expire-at (+ (truncate (get-current-ts) 1000) expire)))
    (make-instance '<memory-cache-entry>
                   :expire expire
                   :expire-at expire-at
                   :value value)))

(defun entry-expire-p (entry)
  "如果ENTRY已经过期了则返回T"
  (with-slots (expire-at) entry
    (and expire-at
         (> (truncate (get-current-ts) 1000)
            expire-at))))

(defmacro with-cache ((key &key expire (in :memory)) &body body)
  "以KEY为索引，将BODY的求值结果存储到IN当中。如果KEY已经存在了，那么直接返回上一次写入的值"
  (declare (ignorable in))
  (let ((old (gensym))
        (result (gensym)))
    `(let* ((,old (get-cache ,key ,in)))
       (cond (,old (values ,old t))
             (t (let ((,result (progn ,@body)))
                  (set-cache ,key ,in ,result ,expire)
                  (values ,result nil)))))))

(defun delete-cache (key place)
  "删除PLACE中以KEY为索引的数据"
  (ecase place
    (:memory (remhash key *memory-cache*))))

(defun clear-cache (place)
  "清空所有在PLACE中的缓存数据"
  (ecase place
    (:memory (clrhash *memory-cache*))))

(defun get-cache (key place)
  "以KEY为索引从PLACE中取出一个早已存入的对象。如果在当前时间经过EXPIRE秒后再次访问这个索引，那么缓存将不复存在。"
  (ecase place
    (:memory (let ((old (gethash key *memory-cache*)))
               (if (or (null old)
                       (entry-expire-p old))
                   nil
                   (value-of old))))))

(defun set-cache (key place value expire)
  "以KEY为索引向PLACE中写入对象VALUE，日后可以根据KEY再次取出"
  (ecase place
    (:memory (setf (gethash key *memory-cache*)
                   (make-cache-entry value expire)))))
