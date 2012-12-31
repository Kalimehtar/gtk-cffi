(in-package :gdk-cffi)

(defcstruct color-struct
  "GdkColor"
  (pixel :int32)
  (red :int16)
  (green :int16)
  (blue :int16))

(defcfun gdk-color-parse :boolean (str :string) 
         (color :pointer))
(defcfun gdk-color-to-string :string (color :pointer))
(defcfun gdk-color-free :void (color :pointer))

(define-foreign-type color-cffi (freeable)
  ()
  (:actual-type :pointer)
  (:simple-parser pcolor))

(defmethod free-ptr ((class (eql 'color-cffi)) ptr)
  (gdk-color-free ptr))

(defmethod translate-to-foreign (value (type color-cffi))
  (if (pointerp value) value
    (let ((color-st (foreign-alloc (cffi-objects::struct-type 'color-struct))))
      (gdk-color-parse (string value) color-st)
      color-st)))

(defmethod translate-from-foreign (ptr (type color-cffi))
  (gdk-color-to-string ptr))

(defcfun (color-equal "gdk_color_equal") :boolean 
  (color pcolor) (color2 pcolor))

(defcstruct rgba-struct
  "GdkRGBA"
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(define-foreign-type rgba-cffi (freeable)
  ()
  (:actual-type :pointer)
  (:simple-parser prgba))

(defcfun gdk-rgba-parse :boolean (color :pointer)
         (str :string))
(defcfun gdk-rgba-to-string :string (color :pointer))
(defcfun gdk-rgba-free :void (color :pointer))

(defmethod free-ptr ((class (eql 'rgba-cffi)) ptr)
  (gdk-rgba-free ptr))

(defmethod translate-to-foreign (value (type rgba-cffi))
  (if (pointerp value) value
    (let ((color-st (foreign-alloc :pointer)))
      (assert (gdk-rgba-parse color-st (string value)) (value) 
              "Bad RGBA color") 
      color-st)))

(defmethod translate-from-foreign (ptr (type rgba-cffi))
  (gdk-rgba-to-string ptr))
