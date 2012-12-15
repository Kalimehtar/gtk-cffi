(in-package :gtk-cffi)

(defclass h-box (box)
  ())

(defcfun "gtk_hbox_new" :pointer (homogeneous :boolean) (spacing :int))

(defmethod gconstructor ((h-box h-box)
                         &key (homogeneous nil) (spacing 0) &allow-other-keys)
  (gtk-hbox-new homogeneous spacing))


