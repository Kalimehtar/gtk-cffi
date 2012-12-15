;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; spinner.lisp --- Wrapper for GtkSpinner
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass spinner (widget)
  ())

(defcfun gtk-spinner-new :pointer)

(defmethod gconstructor ((spinner spinner) &key)
  (gtk-spinner-new))

(deffuns spinner
  (start :void)
  (stop :void))