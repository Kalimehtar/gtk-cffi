;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; accel-group -- GtkAccelGroup
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass accel-group (object)
  ())

(defbitfield accel-flags 
  :visible :locked)

(defcfun "gtk_accel_group_new" :pointer)

(defmethod gconstructor ((accel-group accel-group) &key &allow-other-keys)
  (gtk-accel-group-new))

(defcfun gtk-accel-group-connect :void 
  (accel-group pobject) (accel-key :uint) (accel-mods modifier-type)
  (accel-flags accel-flags) (closure :pointer))

(defcfun gtk-accel-group-connect-by-path :void
  (accel-group pobject) (accel-path :string) (closure :pointer))

(defmethod connect ((accel-group accel-group) func 
                    &key path key accel-mods accel-flags)
  "FUNC should have args: (accel-group acceleratable keyval modifier)
CONNECT returns foreign pointer to create GLib closure"
  (let ((closure (make-closure func)))
    (if path 
        (gtk-accel-group-connect-by-path accel-group path closure) 
        (gtk-accel-group-connect accel-group 
                                 key accel-mods accel-flags closure))
    closure))

(defgtkfun disconnect :boolean accel-group (closure object))

(defcfun ("gtk_accel_group_from_accel_closure" accel-group-from-accel-closure)
    pobject (closure :pointer))