(in-package :fw)

(defvar *memory-cache*
  (make-hash-table :test #'equal))

(defmacro with-cache ((key &key (in :memory)) &body body)
  "以KEY为索引，将BODY的求值结果存储到IN当中。如果KEY已经存在了，那么直接返回上一次写入的值"
  (declare (ignorable in))
  (let ((old (gensym))
        (result (gensym))
        (store (gensym)))
    `(let* ((,store *memory-cache*)
            (,old (gethash ,key ,store)))
       (cond (,old ,old)
             (t (let ((,result (progn ,@body)))
                  (setf (gethash ,key ,store) ,result)
                  ,result))))))
