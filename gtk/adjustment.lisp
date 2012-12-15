;;;
;;; adjustment.lisp -- GtkAdjustment
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass adjustment (g-object)
  ())

(defcfun gtk-adjustment-new :pointer
  (value :double) (lower :double) (upper :double) 
  (step-increment :double) (page-increment :double) (page-size :double))

(defmethod gconstructor ((adjustment adjustment) &key value lower upper 
                         step-increment page-increment page-size)
  (initialize adjustment '(value lower upper 
                           step-increment page-increment page-size))
  (gtk-adjustment-new value lower upper 
                      step-increment page-increment page-size))

(defslots adjustment
  value :double
  lower :double
  page-increment :double
  page-size :double
  step-increment :double
  upper :double)

(deffuns adjustment
  (clamp-page :void (lower :double) (upper :double))
  (changed :void)
  (value-changed :void)
  (:get minimum-increment :double))

(defcfun gtk-adjustment-configure :pointer
  (adjustment pobject) (value :double) (lower :double) (upper :double) 
  (step-increment :double) (page-increment :double) (page-size :double))

(defmethod reinitialize-instance ((adjustment adjustment) 
                                  &key value lower upper 
                                  step-increment page-increment page-size)
  (gtk-adjustment-configure adjustment value lower upper 
                            step-increment page-increment page-size))

  
