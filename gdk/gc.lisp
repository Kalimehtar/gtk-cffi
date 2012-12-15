(in-package :gdk-cffi)

(defclass gc (g-object)
  ())

(register-type 'gc "GdkGC")
(register-type 'gc "GdkGCX11")