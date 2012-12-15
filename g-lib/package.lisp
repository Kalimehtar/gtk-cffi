;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for glib-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cl-user)

(defpackage #:g-lib-cffi
  (:nicknames #:g-lib #:glib)
  (:use #:common-lisp #:cffi-objects #:iterate #:alexandria)
  (:export
   ;; gerror macro
   #:with-g-error
   #:throw-g-error
   
   ;; types
   #:g-list
   #:g-slist
   #:g-quark
   #:string-list
   #:variant-type
   #:variant
   #:gsize

   #:g-error
   #:get-error

   #:garray
   #:*array-length*

   #:timeout-add
   #:timeout-remove
   #:yield
   #:yield1

   #:g-intern-static-string
   #:g-free

   #:g-lib-array
   #:g-lib-string
   
   #:g-file
   ))
