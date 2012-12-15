(in-package :gtk-cffi)

(defclass pixbuf (gobject)
  ())

(defmethod initialize-instance
  :after ((pixbuf pixbuf)
          &key pointer file &allow-other-keys)
  (setf (pointer screen)
        (if pointer pointer
          (when file (gdk-pixbuf-new ))))