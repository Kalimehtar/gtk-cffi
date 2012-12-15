;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gdk-cffi.asd --- ASDF system definition for gdk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:gdk-cffi-system
  (:use #:cl #:asdf))
(in-package #:gdk-cffi-system)

(defsystem gdk-cffi
  :description "Interface to GTK/GLib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "BSD"
  :depends-on (g-object-cffi g-lib-cffi cl-cairo2)
  :components
  ((:file package)
   (:file loadlib :depends-on (package))
   (:file generics :depends-on (package))
   (:file rectangle :depends-on (loadlib generics))
   (:file screen :depends-on (loadlib generics))
   (:file window :depends-on (loadlib generics))
   (:file pango :depends-on (loadlib generics))
   (:file keys :depends-on (package window pango))
   (:file threads :depends-on (package))
   (:file event :depends-on (loadlib generics window))
   (:file color :depends-on (loadlib generics))
   (:file gc :depends-on (loadlib generics))
   (:file visual :depends-on (loadlib generics))
   (:file image :depends-on (visual))
   (:file atom :depends-on (loadlib))   
   (:file pixbuf :depends-on (image gc))
   (:file cairo :depends-on (pixbuf))
   (:file drag-drop :depends-on (package))))
