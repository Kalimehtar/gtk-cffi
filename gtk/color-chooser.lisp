;;;
;;; color-chooser.lisp -- GtkColorChooser
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass color-chooser (object)
  ())

(defslots color-chooser
  rgba prgba
  use-alpha :boolean)

(defcfun gtk-color-chooser-add-palette 
    :void (color-chooser pobject) (orientation orientation) 
    (colors-per-line :int) (n-colors :int) (colors :pointer))

(defgeneric add-palette (color-chooser colors colors-per-line 
                                       &key orientation)
  (:method ((color-chooser color-chooser) colors colors-per-line 
            &key orientation)
    (let ((type 'gdk-cffi::rgba-struct)
          (n-colors (length colors)))
      (with-foreign-object (pcolors type n-colors)
        (dotimes (i n-colors)
          (destructuring-bind (red green blue alpha) (elt colors i)
            (template (field var) 
                (('gdk-cffi::red red) ('gdk-cffi::green green)
                 ('gdk-cffi::blue blue) ('gdk-cffi::alpha alpha))
              `(setf (foreign-slot-value (mem-ref pcolors type i)
                                         type ,field) ,var))))
        (gtk-color-chooser-add-palette color-chooser orientation
                                       colors-per-line n-colors colors)))))