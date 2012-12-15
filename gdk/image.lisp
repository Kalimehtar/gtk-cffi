;(in-package :gdk-cffi)
;
;(defclass image (gobject)
;  ())

;(defcenum image-type
;  :normal :shared :fastest)

;(defcfun "gdk_image_new" :pointer
;  (image-type image-type) (visual pobject) (width :int) (height :int))

;(defmethod gconstructor ((image image)
;                         &key (type :fastest) visual width height)
;  (gdk-image-new type (or visual (make-instance 'visual)) width height))