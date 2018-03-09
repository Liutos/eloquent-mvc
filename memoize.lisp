(in-package :fw)

(defvar *memory-cache*
  (make-hash-table :test #'equal))

(defmacro with-cache ((key &key (in :memory)) &body body)
  "以KEY为索引，将BODY的求值结果存储到IN当中。如果KEY已经存在了，那么直接返回上一次写入的值"
  (declare (ignorable in))
  (let ((old (gensym))
        (result (gensym)))
    `(let* ((,old (get-cache ,key ,in)))
       (cond (,old (values ,old t))
             (t (let ((,result (progn ,@body)))
                  (set-cache ,key ,in ,result)
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
  "以KEY为索引从PLACE中取出一个早已存入的对象"
  (ecase place
    (:memory (gethash key *memory-cache*))))

(defun set-cache (key place value)
  "以KEY为索引向PLACE中写入对象VALUE，日后可以根据KEY再次取出"
  (ecase place
    (:memory (setf (gethash key *memory-cache*) value))))
