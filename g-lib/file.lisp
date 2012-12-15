;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; file.lisp -- interface to GFile
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-lib-cffi)

(defclass g-file (object)
  ())

(define-foreign-type gfile (cffi-object)
  ()
  (:actual-type :pointer)
  (:simple-parser g-file))

(defmethod translate-from-foreign (ptr (gfile gfile))
  (declare (type foreign-pointer ptr))
  (make-instance 'g-file :pointer ptr))