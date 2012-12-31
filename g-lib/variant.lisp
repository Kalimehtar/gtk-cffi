;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; variant.lisp -- interface to GVariantType
;;;
;;; Copyright (C) 2012, Roman Klochkov <monk@slavsoft.surgut.ru>
;;;

(in-package #:g-lib-cffi)

(define-foreign-type variant-type (freeable)
  ()
  (:actual-type :pointer)
  (:simple-parser variant-type))

(defcfun g-variant-type-peek-string :pointer (ptr :pointer))
(defcfun g-variant-type-new :pointer (format :string))
(defcfun g-variant-type-free :void (ptr :pointer))
(defcfun g-variant-type-get-string-length gsize (ptr :pointer))

(defmethod free-ptr ((type (eql 'variant-type)) ptr)
  (g-variant-type-free ptr))

(defmethod translate-from-foreign (ptr (type variant-type))
  (declare (type foreign-pointer ptr))
  (when (not (null-pointer-p ptr))
    (foreign-string-to-lisp 
     (g-variant-type-peek-string ptr)
     :count (g-variant-type-get-string-length ptr))))

(defmethod translate-to-foreign ((str string) (type variant-type))
  (g-variant-type-new str))

(define-foreign-type variant (freeable)
  ((free :initform t))
  (:actual-type :pointer)
  (:simple-parser variant))

(defcfun g-variant-parse :pointer
  (type variant-type) (text :pointer) (limit :pointer) (end :pointer)
  (g-error object))

(defcfun g-variant-print (:string :free-from-foreign t)
  (variant :pointer) (annotate :boolean))

(defcfun g-variant-unref :void (variant :pointer))

(defmethod free-ptr ((type (eql 'variant)) ptr)
  (g-variant-unref ptr))

(defmethod translate-from-foreign (ptr (type variant-type))
  (g-variant-print ptr t))

(defmethod translate-to-foreign ((str string) (type variant-type))
  (destructuring-bind (fstr len) (foreign-string-alloc str)
    (let (ptr)
      (with-g-error g-error
        (setf ptr
              (g-variant-parse (null-pointer) fstr (inc-pointer fstr len)
                               (null-pointer) g-error))
        (when (null-pointer-p ptr) (error "Error in GVariant: ~a" g-error)))
      (foreign-string-free str)
      ptr)))
