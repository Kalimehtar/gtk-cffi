(in-package :gtk-cffi)

(defclass frame (bin)
  ())

(defcfun "gtk_frame_new" :pointer (label :string))

(defmethod gconstructor ((frame frame)
                         &key label &allow-other-keys)
  (gtk-frame-new label))

(defcfun "gtk_frame_set_shadow_type" :void
  (frame pobject) (shadow shadow-type))

(defmethod (setf shadow-type) (shadow-type (frame frame))
  (gtk-frame-set-shadow-type frame shadow-type))

(defcfun "gtk_frame_get_shadow_type" shadow-type (frame pobject))

(defmethod shadow-type ((frame frame))
  (gtk-frame-get-shadow-type frame))

