(in-package :gtk-cffi)

(defclass notebook (container)
  ())

(defcfun "gtk_notebook_new" :pointer)

(defmethod gconstructor ((notebook notebook) &rest rest)
  (declare (ignore rest))
  (gtk-notebook-new))