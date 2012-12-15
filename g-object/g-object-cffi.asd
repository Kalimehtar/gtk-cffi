;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; glib-cffi-system.asd --- ASDF system definition for glib-cffi-system
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:gobject-cffi-system
  (:use #:cl #:asdf))
(in-package #:gobject-cffi-system)

(defsystem g-object-cffi
  :description "GObject,GType and GValue staff for gtk-cffi"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.3"
  :license "BSD"
  :depends-on (g-lib-cffi gtk-cffi-utils)
  :components
  ((:file package)
   (:file loadlib :depends-on (package))
   (:file generics :depends-on (package))
   (:file g-type :depends-on (loadlib generics))
   (:file pobject :depends-on (g-type))
   (:file defslots :depends-on (pobject))
   (:file g-value :depends-on (pobject))
   (:file g-object :depends-on (g-value))
   (:file g-object-class :depends-on (g-object))
   (:file subclass :depends-on (g-object))))

