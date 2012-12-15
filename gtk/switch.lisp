;;;
;;; switch.lisp -- GtkSwitch
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;


(in-package :gtk-cffi)

(defclass switch (widget actionable activatable) ())

(defcfun gtk-switch-new :pointer)

(defmethod gconstructor ((switch switch) &key &allow-other-keys)
  (gtk-switch-new))

(defslot switch active :boolean)

(init-slots switch)
