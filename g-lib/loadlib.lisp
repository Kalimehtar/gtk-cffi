;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; loadlib.lisp --- loading C library
;;;
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-lib-cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library :g-lib
    (:unix "libglib-2.0.so")
    (:windows "libglib-2.0-0.dll"))

  (load-foreign-library :g-lib))

(defctype gsize :int)