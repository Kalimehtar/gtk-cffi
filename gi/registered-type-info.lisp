(in-package #:gi-cffi)

(defclass registered-type-info (base-info)
  ())

(deffuns registered-type-info
  (:get type-name :string)
  (:get type-init :string)
  (get-g-type g-type))
  
(defmethod free-ptr ((type (eql 'registered-type-info)) ptr)
  (g-base-info-unref ptr))
