;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; menu-item.lisp --- GtkMenuItem
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass menu-item (bin)
  ())

(defcfun gtk-menu-item-new :pointer)
(defcfun gtk-menu-item-new-with-label :pointer (label gtk-string))
(defcfun gtk-menu-item-new-with-mnemonic :pointer (label gtk-string))

(defmethod gconstructor ((menu-item menu-item) 
                         &key label mnemonic &allow-other-keys)
  (if label
      (if mnemonic
          (gtk-menu-item-new-with-mnemonic label)
          (gtk-menu-item-new-with-label label))
      (gtk-menu-bar-new)))

(defgtkslots menu-item
    right-justified :boolean
    label gtk-string
    use-underline :boolean
    submenu pobject
    accel-path gtk-string
    reserve-indicator :boolean)

(defgtkfun select :void menu-item)
(defgtkfun deselect :void menu-item)
(defgtkfun activate :void menu-item)
(defgtkfun toggle-size-request :void menu-item (requisition :pointer))
(defgtkfun toggle-size-allocate :void menu-item (allocation :int))

(init-slots menu-item nil)