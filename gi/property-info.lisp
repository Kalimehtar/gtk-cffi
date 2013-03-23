(in-package #:gi-cffi)

(defclass property-info (base-info)
  ())

(defmethod free-ptr ((type (eql 'property-info)) ptr)
  (g-base-info-unref ptr))