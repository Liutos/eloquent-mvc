(in-package #:eloquent.mvc.prelude)

(defgeneric split (xs delimiter &rest keys)
  (:documentation "Return a list of components in xs separated by delimiter"))

(defmethod split ((text string) (delimiter character) &rest keys)
  (apply #'split-sequence:split-sequence delimiter text keys))
