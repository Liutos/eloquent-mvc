(in-package #:eloquent.mvc.prelude)

(defun string-to-alist (text group-delimiter value-delimiter)
  "Return an assoiate-list consist of groups separated by group-delimiter in text, each group contains two components separated by value-delimiter in text.

If in a group, the content at the right-hand-side of value-delimiter is empty, then it will be an empty string in the associate-list."
  (declare (type string text))
  (declare (type character group-delimiter value-delimiter))
  (mapcar #'(lambda (group)
              (optima:match (split group value-delimiter :remove-empty-subseqs t)
                ((list key) (cons key ""))
                ((list key value) (cons key value))))
          (split text group-delimiter
                 :remove-empty-subseqs t)))
