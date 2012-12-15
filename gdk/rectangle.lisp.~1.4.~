(in-package :gdk-cffi)

(defclass rectangle (object)
  ())

(defmethod new-struct ((class (eql 'rectangle)))
  (foreign-alloc 'cairo_rectangle_t))

(defcstruct-accessors (rectangle . cairo_rectangle_t))

(defcfun gdk-rectangle-intersect :boolean
  (src1 (struct rectangle)) (src2 (struct rectangle)) 
  (dest (struct rectangle :out t)))

(defgeneric intersect (rect1 rect2))
(defmethod intersect ((rect1 rectangle) (rect2 rectangle))
  "Returns new GdkRectangle: intersection of rect1 and rect2"
  (let ((dest (make-instance 'rectangle)))
    (when (gdk-rectangle-intersect rect1 rect2 dest)
      dest)))

(defcfun gdk-rectangle-union :void
  (src1 (struct rectangle)) (src2 (struct rectangle)) 
  (dest (struct rectangle :out t)))

(defgeneric rectangle-union (rect1 rect2))
(defmethod rectangle-union ((rect1 rectangle) (rect2 rectangle))
  (let ((dest (make-instance 'rectangle)))
    (gdk-rectangle-union rect1 rect2 dest)))

(defcfun gdk-rectangle-get-type g-type)
