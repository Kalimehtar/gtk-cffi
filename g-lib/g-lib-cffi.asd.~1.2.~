;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; g-lib-cffi-system.asd --- ASDF system definition for glib-cffi-system
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:g-lib-cffi-system
  (:use #:cl #:asdf))
(in-package #:g-lib-cffi-system)

(defsystem g-lib-cffi
  :description "CFFI interface to some GLib primitives"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "BSD"
  :depends-on (cffi-object)
  :components
  ((:file package)
   (:file loadlib :depends-on (package))
   (:file list :depends-on (loadlib))
   (:file quark :depends-on (loadlib))
   (:file array :depends-on (loadlib))
   (:file error :depends-on (quark))
   (:file file :depends-on (loadlib))
   (:file mainloop :depends-on (loadlib))))