(in-package #:gi-cffi)

(defclass signal-info (callable-info)
  ())

(defmethod free-ptr ((type (eql 'signal-info)) ptr)
  (g-base-info-unref ptr))