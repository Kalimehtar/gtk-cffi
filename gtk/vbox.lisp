(in-package :gtk-cffi)

(defclass v-box (box)
  ())

(defcfun "gtk_vbox_new" :pointer (homogeneous :boolean) (spacing :int))

(defmethod gconstructor ((v-box v-box)
                         &key homogeneous (spacing 0) &allow-other-keys)
  (gtk-vbox-new homogeneous spacing))




