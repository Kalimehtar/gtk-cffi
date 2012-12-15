
(defpackage #:gtk-cffi-ext-system
  (:use #:cl #:asdf))
(in-package #:gtk-cffi-ext-system)


(defsystem gtk-cffi-ext
  :description "Extensions for GTK-CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi)
  :components
  ((:file package)
   (:file lisp-model :depends-on (package))
   (:file addons :depends-on (package))))

