;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; color-button.lisp --- Wrapper for GtkColorButton
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass color-button (button color-chooser)
  ())

(defcfun gtk-color-button-new :pointer)
(defcfun gtk-color-button-new-with-color :pointer (color pcolor))
(defcfun gtk-color-button-new-with-rgba :pointer (rgbd prgba))

(defmethod gconstructor ((color-button color-button) &key color rgba)
  (initialize color-button '(color rgba))
  (cond
    (color (gtk-color-button-new-with-color color))
    (rgba (gtk-color-button-new-with-rgba rgba))
    (t (gtk-color-button-new))))

(defslots color-button
  rgba prgba
  alpha :uint16
  use-alpha :boolean
  title :string)

(deffuns color-button
  (:get color pcolor &key)
  (:set color pcolor &key))

(init-slots color-button)