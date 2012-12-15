;;;
;;; window-group.lisp --- GtkWindowGroup
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass window-group (g-object)
  ())

(defcfun gtk-window-group-new :pointer)

(defmethod gconstructor ((window-group window-group) &key &allow-other-keys)
  (gtk-window-group-new))

(deffuns window-group
  (add-window :void (window pobject))
  (remove-window :void (window pobject))
  (list-windows g-list-object)
  (:get current-grab pobject)
  (:get current-device-grab pobject (device pobject)))

(init-slots window-group)
