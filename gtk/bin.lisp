(in-package :gtk-cffi)

(defclass bin (container)
  ())

(defcfun "gtk_bin_get_child" pobject (bin pobject))

(defmethod child ((bin bin))
  (gtk-bin-get-child bin))

(defmethod (setf kids) (kids (bin bin))
  (if (second kids)
      (error "bin should have only one child")
    (add bin (car kids))))
