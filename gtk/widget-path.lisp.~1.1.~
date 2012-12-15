(in-package :gtk-cffi)

(defclass widget-path (object)
  ())

(defgtkfun free :void widget-path)

(defcfun gtk-widget-path-new :pointer)

(defmethod gconstructor ((widget-path widget-path) &key &allow-other-keys)
  (gtk-widget-path-new))

