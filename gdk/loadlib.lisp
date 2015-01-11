;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gdk-cffi.lisp --- loading C library 
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gdk-cffi)

;(eval-when (:compile-toplevel :load-toplevel :execute)
(define-foreign-library :gdk
  (:unix (:or "libgdk-3.so.0" "libgdk-3.so"))
  (:windows "libgdk-win32-3-0.dll"))

(use-foreign-library :gdk)

