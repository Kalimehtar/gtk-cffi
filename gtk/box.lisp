(in-package :gtk-cffi)

(defclass box (container orientable)
  ((expand :initform t :initarg :expand)
   (fill :initform t :initarg :fill)
   (padding :initform 0 :initarg :padding)
   (cur-param :initform nil :allocation :class)))

(defcfun gtk-box-pack-start :void (box pobject) (widget pobject)
  (expand :boolean) (fill :boolean) (padding :int))

(defcfun gtk-box-pack-end :void (box pobject) (widget pobject)
  (expand :boolean) (fill :boolean) (padding :int))

(defslots box
  homogeneous :boolean
  spacing :int)

(deffuns box
  (reorder-child :void (child pobject) (position :int)))

(defcfun gtk-box-query-child-packing :void 
  (box pobject) (child pobject) (expand :pointer) (fill :pointer) 
  (padding :pointer) (pack-type :pointer))
  
(defcfun gtk-box-set-child-packing :void
  (box pobject) (child pobject) (expand :boolean) (fill :boolean) 
  (padding :uint) (pack-type pack-type))

(defgeneric child-packing (box child)
  (:method ((box box) (child widget))
    (with-foreign-outs-list ((expand :boolean) (fill :boolean) 
                             (padding :uint) (pack-type 'pack-type)) :ignore
      (gtk-box-query-child-packing box child expand fill padding pack-type))))

(defgeneric (setf child-packing) (value box child)
  (:method (value (box box) (child widget))
    (destructuring-bind (expand fill padding pack-type) value
      (gtk-box-set-child-packing box child expand fill padding pack-type))))
  

(defmethod pack ((box box) (widget widget)
                 &key end
                 (expand :default) (fill :default) (padding :default))
  (macrolet ((default (field)
               `(if (eq ,field :default) (slot-value box ',field) ,field)))
;    (debug-out "~a~%" (list box widget
;                            expand (default expand)
;                            fill (default fill)
;                            padding (default padding)))
    (funcall (if end #'gtk-box-pack-end
               #'gtk-box-pack-start)
             box widget
             (default expand) (default fill) (default padding))))

(defmethod pack ((box box) token &rest rest)
  "Handle to let user set (pack* box widget1 :expand t widget2 widget3)
Here, widget2  and widget3 will be packed with expand"
  (declare (ignore rest))
  (pack-with-param box token cur-param '(:fill :padding :expand)))




