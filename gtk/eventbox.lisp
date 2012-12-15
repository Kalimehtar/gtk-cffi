(in-package :gtk-cffi)

(defclass event-box (bin)
  ())

(defcfun "gtk_event_box_new" :pointer)

(defmethod gconstructor ((event-box event-box)
                         &key &allow-other-keys)
  (gtk-event-box-new))
