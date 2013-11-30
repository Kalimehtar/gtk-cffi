;;;
;;; orientable.lisp -- GtkOrientable
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass orientable (object)
  ())

(defcenum orientation :horizontal :vertical)

(defslot orientable orientation orientation)
