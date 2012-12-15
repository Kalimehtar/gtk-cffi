(in-package :gtk-cffi)

(defclass menu (menu-shell)
  ())

(defcfun "gtk_menu_new" :pointer)

(defmethod gconstructor ((menu menu) &rest rest)
  (declare (ignore rest menu))
  (gtk-menu-new))