(defpackage #:gi-cffi-system
  (:use #:cl #:asdf))
(in-package #:gi-cffi-system)

(defsystem gi-cffi
  :description "Interface to GObjectIntrospection via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "LLGPL"
  :depends-on (gtk-cffi)
  :serial t
  :components
  ((:file package)
   (:file loadlib)
   (:file repository)
   (:file base-info)
   (:file constant-info)
   (:file registered-type-info)
   (:file struct-info)
   (:file union-info)
   (:file enum-info)
   (:file interface-info)
   (:file object-info)
   (:file type-info)
   (:file arg-info)
   (:file callable-info)
   (:file function-info)
   (:file field-info)
   (:file property-info)
   (:file vfunc-info)
   (:file signal-info)))