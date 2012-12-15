(in-package :gtk-cffi)

(defclass paned (container)
  ((resize :initform nil :initarg :resize)
   (shrink :initform t :initarg :shrink)
   (pane-type :initform 1)
   (cur-param  :initform nil :allocation :class)))

(defgtkslot paned (paned-position . position) :int)

(defcfun "gtk_paned_add1" :void (paned pobject) (widget pobject))

(defcfun "gtk_paned_add2" :void (paned pobject) (widget pobject))

(defcfun "gtk_paned_pack1" :void (paned pobject) (widget pobject)
  (resize :boolean) (shrink :boolean))

(defcfun "gtk_paned_pack2" :void (paned pobject) (widget pobject)
  (resize :boolean) (shrink :boolean))

(defmethod pack ((paned paned) (widget widget)
                 &key (pane-type :default) (resize :default) (shrink :default))
  (macrolet ((default (field)
               `(if (eq ,field :default) (slot-value paned ',field) ,field)))
    (case (default pane-type)
      (1 (if (and (not (default resize)) (default shrink))
             (gtk-paned-add1 paned widget)
           (gtk-paned-pack1 paned widget (default resize) (default shrink))))
      (2 (if (and (default resize) (default shrink))
             (gtk-paned-add2 paned widget)
           (gtk-paned-pack2 paned widget (default resize) (default shrink))))))
  (setf (slot-value paned 'pane-type) 2
        (slot-value paned 'resize) t))

(defmethod pack ((paned paned) token &rest rest)
  "Handle to let user set ('paned :resize nil widget1 :resize t widget2)
Here, widget2 will be packed with expand"
  (declare (ignore rest))
  (pack-with-param paned token cur-param '(:resize :shrink)))


(defclass h-paned (paned)
  ())

(defcfun "gtk_hpaned_new" :pointer)

(defmethod gconstructor ((h-paned h-paned)
                         &key &allow-other-keys)
  (gtk-hpaned-new))

(defclass v-paned (paned)
  ())

(defcfun "gtk_vpaned_new" :pointer)

(defmethod gconstructor ((v-paned v-paned)
                         &key &allow-other-keys)
  (gtk-vpaned-new))

