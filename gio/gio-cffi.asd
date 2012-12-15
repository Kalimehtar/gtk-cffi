;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gio-cffi.asd --- ASDF system definition for gio-cffi
;;;
;;; Copyright (C) 2012, Roman Klochkov <monk@slavsoft.surgut.ru>
;;;

(defpackage #:gio-cffi-system
  (:use #:cl #:asdf))
(in-package #:gio-cffi-system)

(defsystem gio-cffi
  :description "Interface to GIO via CFFI"
  :author "Roman Klochkov <monk@slavsoft.surgut.ru>"
  :version "0.5"
  :license "BSD"
  :depends-on (g-object-cffi g-lib-cffi gtk-cffi-utils)
  :components
  ((:file package)
   (:file loadlib :depends-on (package))
   (:file action-group :depends-on (loadlib))
   (:file application :depends-on (action-group))))
