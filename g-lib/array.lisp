;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; array.lisp --- CFFI wrapper for arrays
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :g-lib-cffi)

(define-foreign-type g-lib-array (cffi-array)
  ()
  (:actual-type :pointer))

(define-parse-method garray (type &rest rest)
  (apply #'make-instance 'g-lib-array :type type rest))

(defcfun g-free :void (var :pointer))
(defcfun g-malloc :pointer (n-bytes :int))

(defmethod free-ptr ((type (eql 'g-lib-array)) ptr)
  (g-free ptr))

(define-foreign-type g-lib-string (freeable)
  ((free-from-foreign :initform t))
  (:simple-parser g-lib-string)
  (:actual-type :pointer))

(defmethod translate-to-foreign (value (type g-lib-string))
  (with-foreign-string ((str len) value)
    (let ((ptr (g-malloc len)))
      (lisp-string-to-foreign value ptr len)
      ptr)))

(defmethod translate-from-foreign (value (type g-lib-string))
  (foreign-string-to-lisp value))

(defmethod free-ptr ((type (eql 'g-lib-string)) ptr)
  (g-free ptr))