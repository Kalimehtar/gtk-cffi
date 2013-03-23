(in-package #:gi-cffi)

(defclass vfunc-info (callable-info)
  ())

(defmethod free-ptr ((type (eql 'vfunc-info)) ptr)
  (g-base-info-unref ptr))