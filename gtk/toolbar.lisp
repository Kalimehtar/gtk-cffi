(in-package #:gtk-cffi)

(defclass toolbar (container tool-shell) ())

(defcfun "gtk_toolbar_new" :pointer)

(defmethod gconstructor ((toolbar toolbar) &rest rest)
  (declare (ignore toolbar rest))
  (gtk-toolbar-new))