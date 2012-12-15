(in-package :gdk-cffi)

(defclass screen (g-object)
  ())

(defcfun "gdk_screen_get_default" :pointer)

(defmethod gconstructor ((screen screen)
                         &key &allow-other-keys)
  (gdk-screen-get-default))

(defcfun "gdk_screen_get_width" :int (screen pobject))

(defmethod width ((screen screen))
  (gdk-screen-get-width screen))

(defcfun "gdk_screen_get_height" :int (screen pobject))

(defmethod height ((screen screen))
  (gdk-screen-get-height screen))


