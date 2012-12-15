(in-package :gtk-cffi)

(defclass box (container)
  ((expand :initform t :initarg :expand)
   (fill :initform t :initarg :fill)
   (padding :initform 0 :initarg :padding)
   (cur-param :initform nil :allocation :class)))

(defcfun "gtk_box_pack_start" :void (box pobject) (widget pobject)
  (expand :boolean) (fill :boolean) (padding :int))

(defcfun "gtk_box_pack_end" :void (box pobject) (widget pobject)
  (expand :boolean) (fill :boolean) (padding :int))

(defmethod pack ((box box) (widget widget)
                 &key end
                 (expand :default) (fill :default) (padding :default))
  (macrolet ((default (field)
               `(if (eq ,field :default) (slot-value box ',field) ,field)))
    (debug-out "~a~%" (list box widget
                            expand (default expand)
                            fill (default fill)
                            padding (default padding)))
    (funcall (if end #'gtk-box-pack-end
               #'gtk-box-pack-start)
             box widget
             (default expand) (default fill) (default padding))))

(defmethod pack ((box box) token &rest rest)
  "Handle to let user set (pack* box widget1 :expand t widget2 widget3)
Here, widget2  and widget3 will be packed with expand"
  (declare (ignore rest))
  (pack-with-param box token cur-param '(:fill :padding :expand)))




