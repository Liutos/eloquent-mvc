(in-package #:eloquent.mvc.prelude)

(defun string-to-alist (text group-delimiter value-delimiter
                        &key trim-key-p)
  "Return an assoiate-list consist of groups separated by group-delimiter in text, each group contains two components separated by value-delimiter in text.

If in a group, the content at the right-hand-side of value-delimiter is empty, then it will be an empty string in the associate-list."
  (check-type text string)
  (check-type group-delimiter character)
  (check-type value-delimiter character)
  (flet ((parse-key (key)
           (if trim-key-p
               (string-trim '(#\ ) key)
               key)))
    (mapcar #'(lambda (group)
                (optima:match (split group value-delimiter :remove-empty-subseqs t)
                  ((list key) (cons (parse-key key) ""))
                  ((list key value) (cons (parse-key key) value))))
            (split text group-delimiter
                   :remove-empty-subseqs t))))
