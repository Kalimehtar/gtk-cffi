(in-package :gtk-cffi)

(defclass invisible (widget)
  ())

(defcfun gtk-invisible-new :pointer)
(defcfun gtk-invisible-new-for-screen :pointer (screen pobject))

(defmethod gconstructor ((invisible invisible) &key screen &allow-other-keys) 
  (if screen (gtk-invisible-new-for-screen screen) (gtk-invisible-new)))

(defslot invisible screen pobject)