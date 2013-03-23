(in-package #:gi-cffi)

(defclass field-info (base-info)
  ())

(defmethod free-ptr ((type (eql 'field-info)) ptr)
  (g-base-info-unref ptr))