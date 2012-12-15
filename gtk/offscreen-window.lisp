;;;
;;; offscreen-window.lisp -- GtkOffscreenWindow
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass offscreen-window (window) ())

(defcfun gtk-offscreen-window-new :pointer)

(defmethod gconstructor ((offscreen-window offscreen-window) &key)
  (gtk-offscreen-window-new))

(deffuns offscreen-window
  (:get pixbuf pobject))

(defcfun gtk-offscreen-window-get-surface :pointer (off-win pobject))

(defgeneric surface (offscreen-window)
  (:method ((offscreen-window offscreen-window))
    (cairo:create-surface-from-foreign
     (gtk-offscreen-window-get-surface offscreen-window))))