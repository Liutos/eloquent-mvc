(in-package #:eloquent.mvc.prelude)

(defgeneric equivalent (x y)
  (:documentation "Return true if two arguments are equivalent at special means"))

(defmethod equivalent ((x string) (y string))
  (string= x y))

(defmethod equivalent ((x symbol) (y string))
  (string= (symbol-name x) y))
