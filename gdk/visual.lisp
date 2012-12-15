(in-package :gdk-cffi)

(defclass visual (gobject)
  ())

(defcenum visual-type
  :static-gray
  :grayscale
  :static-color
  :pseudo-color
  :true-color
  :direct-color)

(defcfun "gdk_visual_get_best" :pointer)
(defcfun "gdk_visual_get_system" :pointer)
(defcfun "gdk_visual_get_best_with_both" :pointer
  (depth :int) (visual-type visual-type))
(defcfun "gdk_visual_get_best_with_type" :pointer (visual-type visual-type))
(defcfun "gdk_visual_get_best_with_depth" :pointer (depth :int))


(defmethod gconstructor ((visual visual)
                         &key (type :best) depth visual-type)
  (cond
   ((eq type :system) (gdk-visual-get-system))
   ((and depth visual-type) (gdk-visual-get-best-with-both depth visual-type))
   (depth (gdk-visual-get-best-with-depth depth))
   (visual-type (gdk-visual-get-best-with-type visual-type))
   (t (gdk-visual-get-best))))
    
