;;;
;;; buildable.lisp -- GtkBuildable
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass buildable (object)
  ())

(defslot buildable name :string)