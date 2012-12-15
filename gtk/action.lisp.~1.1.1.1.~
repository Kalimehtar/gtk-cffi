(in-package :gtk-cffi)

(defclass action (gobject)
  ())

(defcfun gtk-action-new :pointer (name gtk-string) (label gtk-string)
         (tooltip gtk-string) (stockid gtk-string))

(defmethod gconstructor ((action action) 
                         &key name label tooltip stock-id &allow-other-keys)
  (gtk-action-new name label tooltip stockid))

(defcfun gtk-action-get-name gtk-string (action pobject))

(defmethod name ((action action))
  (gtk-action-get-name action))

(defgtkslots action 
    sensitive :boolean
    visible :boolean
    accel-path gtk-string
    icon-name gtk-string
    label gtk-string
    short-label gtk-string
    stock-id gtk-string
    visible-horizontal :boolean
    visible-vertical :boolean
    is-important :boolean)

