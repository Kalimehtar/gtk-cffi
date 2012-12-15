;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gdk-cffi.lisp --- loading C library 
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gdk-cffi)

;(eval-when (:compile-toplevel :load-toplevel :execute)
(define-foreign-library :gdk
  (:unix "libgdk-3.so.0")
  (:windows "libgdk-win32-3xs-0.dll"))

(use-foreign-library :gdk)

