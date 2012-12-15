;;;
;;; actionable.lisp -- GtkActionable
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass actionable (object)
  ())

#+gtk3.4 
(defslots actionable
  action-name :string
  action-target-value variant)

#+gtk3.4
(deffuns actionable
    (:set detailed-action-name :string))

#+gtk3.4
(init-slots actionable)
  
  
  

