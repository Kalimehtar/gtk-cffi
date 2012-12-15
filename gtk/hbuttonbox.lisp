(in-package :gtk-cffi)

(defclass h-button-box (button-box)
  ())

(defcfun "gtk_hbutton_box_new" :pointer)

(defmethod gconstructor ((h-button-box h-button-box) &key &allow-other-keys)
  (gtk-hbutton-box-new))