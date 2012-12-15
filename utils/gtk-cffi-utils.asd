(defpackage #:gtk-cffi-utils-system
  (:use #:cl #:asdf))
(in-package #:gtk-cffi-utils-system)


(defsystem gtk-cffi-utils
  :description "Different utils for gtk-cffi"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "1.0"
  :license "LGPL"
  :depends-on (alexandria iterate cffi)
  :components
  ((:file package)
   (:file utils :depends-on (package))))