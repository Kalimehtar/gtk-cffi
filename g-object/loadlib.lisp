;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; loadlib.lisp --- loading C library
;;;
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-object-cffi)

(define-foreign-library :g-object
  (:unix (:or "libgobject-2.0.so.0" "libgobject-2.0.so"))
  (:windows "libgobject-2.0-0.dll"))

(use-foreign-library :g-object)