(in-package :gtk-cffi)

(defclass range (widget)
  ())

(defcenum sensitivity-type
  :auto :on :off)

(defslots range
  fill-level :double
  restrict-to-fill-level :boolean
  show-fill-level :boolean
  adjustment pobject
  inverted :boolean
  value :double
  round-digits :int  
  lower-stepper-sensitivity sensitivity-type
  upper-stepper-sensitivity sensitivity-type
  flippable :boolean
  min-slider-size :int
  slider-size-fixed :boolean)

(defcfun gtk-range-set-increments :void 
  (range pobject) (step :double) (page :double))
(defgeneric (setf increments) (increments range)
  (:method (increments (range range))
    (destructuring-bind (step page) increments
      (gtk-range-set-increments range step page))
    increments))

(defcfun gtk-range-set-range :void 
  (range pobject) (min :double) (max :double))
(defgeneric (setf range) (min-max range)
  (:method (min-max (range range))
    (destructuring-bind (min max) min-max
      (gtk-range-set-increments range min max))
    min-max))

(defcfun gtk-range-get-slider-range :void 
  (range pobject) (start :pointer) (end :pointer))
(defgeneric slider-range (range)
  (:method ((range range))
    (with-foreign-outs-list ((start :int) (end :int)) :ignore
      (gtk-range-get-slider-range range start end))))

(defcfun gtk-range-get-range-rect :void 
  (range pobject) (rect (struct rectangle :out t)))

(defgeneric range-rect (rect)
  (:method ((range range))
    (let ((dest (make-instance 'rectangle)))
      (gtk-range-get-range-rect range dest)
      dest)))
