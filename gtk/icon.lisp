;;;
;;; icon-size.lisp -- GtkIconSize
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defcenum icon-size
  :invalid
  :menu
  :small-toolbar
  :large-toolbar
  :button
  :dnd
  :dialog)

(defcenum state 
  :normal :active :prelight :selected :insensitive :inconsistent :focused)

(defclass icon-source (object) ())

(defcfun gtk-icon-source-new :pointer)

(defmethod gconstructor ((icon-source icon-source) &key)
  (gtk-icon-source-new))

(defgtkslots icon-source
    direction text-direction
    direction-wildcarded :boolean
    filename :string
    pixbuf pobject
    icon-name :string
    size icon-size
    size-wildcarded :boolean
    state state
    state-wildcarded :boolean)