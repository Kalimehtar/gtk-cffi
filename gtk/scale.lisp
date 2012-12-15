;;;
;;; scale.lisp -- GtkScale
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;
(in-package :gtk-cffi)

(defclass scale (range)
  ())

(defcfun gtk-scale-new :pointer (orientation orientation) (adjustment pobject))
(defcfun gtk-scale-new-with-range :pointer (orientation orientation)
         (min :double) (max :double) (step :double))

(defmethod gconstructor ((scale scale) &key orientation adjustment 
                         (min 0.0d0) (max 0.0d0) (step 0.0d0) &allow-other-keys)
  (if adjustment
      (gtk-scale-new orientation adjustment)
      (gtk-scale-new-with-range orientation min max step)))
      

(defslots scale
  digits :int
  draw-value :boolean
  has-origin :boolean
  value-pos position-type)

(deffuns scale
  (:get layout pobject)
  (add-mark :void &key (value :double) 
            (position position-type) (markup :string))
  (clear-marks :void))

(defcfun gtk-scale-get-layout-offsets :void (entry pobject) 
         (x :pointer) (y :pointer))

(defmethod layout-offsets ((scale scale))
  (with-foreign-outs-list ((x :int) (y :int)) :ignore
    (gtk-scale-get-layout-offsets scale x y)))

(init-slots scale)
