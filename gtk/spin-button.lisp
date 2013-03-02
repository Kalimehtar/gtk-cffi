;;;
;;; spin-button.lisp -- GtkSpinButton
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;
(in-package :gtk-cffi)

(defclass spin-button (entry)
  ())

(defcfun gtk-spin-button-new :pointer (adjustment pobject) (climb-rate :double)
         (digits :uint))
(defcfun gtk-spin-button-new-with-range :pointer (min :double) (max :double)
         (step :double))

(defmethod gconstructor ((spin-button spin-button) 
                         &key adjustment climb-rate digits
                         (min 0.0d0) (max 0.0d0) (step 0.0d0) &allow-other-keys)
  (initialize spin-button '(adjustment digits))
  (if adjustment
      (gtk-spin-button-new adjustment climb-rate digits)
      (gtk-spin-button-new-with-range min max step)))

(defcfun gtk-spin-button-configure :void (spin-button pobject) 
         (adjustment pobject) (climb-rate :double) (digits :uint))

(defmethod reinitialize-instance ((spin-button spin-button) 
                                  &key adjustment climb-rate digits)
  (gtk-spin-button-configure spin-button adjustment climb-rate digits))

(defcenum spin-button-update-policy :always :if-valid)

(defcenum spin-type 
  :step-forward :step-backward :page-forward :page-backward
  :home :end :user-defined)

(defslots spin-button
  adjustment pobject
  digits :int
  value :double
  update-policy spin-button-update-policy
  numeric :boolean
  wrap :boolean
  snap-to-ticks :boolean)

(deffuns spin-button
  (spin :void (direction spin-type) (increment :double))
  (update :void)
  (:get value-as-int :int))

(defcfun gtk-spin-button-set-increments :void (spin-button pobject)
         (step :double) (page :double))
(defcfun gtk-spin-button-get-increments :void (spin-button pobject)
         (step :pointer) (page :pointer))

(defgeneric increments (spin-button)
  (:method ((spin-button spin-button))
    (with-foreign-outs-list ((step :double) (page :double)) :ignore
      (gtk-spin-button-get-increments spin-button step page))))

(defmethod (setf increments) (value (spin-button spin-button))
  (destructuring-bind (step page) value
    (gtk-spin-button-set-increments spin-button step page)))
(save-setter spin-button increments)

(defcfun gtk-spin-button-set-range :void (spin-button pobject)
         (min :double) (max :double))
(defcfun gtk-spin-button-get-range :void (spin-button pobject)
         (min :pointer) (max :pointer))

(defgeneric range (spin-button)
  (:method ((spin-button spin-button))
    (with-foreign-outs-list ((min :double) (max :double)) :ignore
      (gtk-spin-button-get-range spin-button min max))))

(defmethod (setf range) (value (spin-button spin-button))
  (destructuring-bind (min max) value
    (gtk-spin-button-set-range spin-button min max)))
(save-setter spin-button range)


(init-slots spin-button)
  
