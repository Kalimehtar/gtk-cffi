(in-package :gtk-cffi)

(defclass container (widget)
  ((%child-properties :accessor %child-properties 
                      :initform nil :allocation :class)))

(defcenum resize-mode
  :parent :queue :immediate)

(defcfun "gtk_container_add" :void (container pobject) (widget pobject))

(defgtkslots container
    border-width :uint
    resize-mode resize-mode
    focus-child pobject
    focus-vadjustment pobject
    focus-hadjustment pobject)

(defmethod add ((container container) (widget widget))
  (gtk-container-add container widget))

(defgeneric pack (container widget &rest rest)
  (:method  ((container container) (widget widget) &rest rest)
    (declare (ignore rest))
    (add container widget)))

(defmacro pack* (box &rest widgets)
  `(progn
     ,@(mapcar
        (lambda (widget) 
          `(pack ,box ,@(ensure-cons widget)))
        widgets)))

(defgeneric (setf kids) (kids container)
  (:documentation "Pack kids to container")
  (:method (kids (container container))
    (mapc (lambda (x) (setf (kid container) x)) kids)))

(defgeneric (setf kid) (kid container)
  (:method (kid (container container))
    (pack container kid)))

(defmethod initialize-instance
  :after ((container container)
          &key kid kids &allow-other-keys)
  (setf-init container kid kids))

(defmacro pack-with-param (container token cur-param keyword-list)
  "Handle to let user set (pack* box widget1 :expand t widget2 widget3)
Here, widget2 and widget3 will be packed with expand."
  `(if (member ,token ,keyword-list) ;'(:pack-fill :padding :expand))
       (setf (slot-value ,container ',cur-param)
             (intern (string ,token) #.*package*))
       (let ((param (slot-value ,container ',cur-param)))
         (when param
           (setf (slot-value ,container param) ,token)))))

(defcfun gtk-container-child-get-property :void 
  (container pobject) (widget pobject) (name cffi-keyword) (value pobject))

(defcfun gtk-container-child-set-property :void 
  (container pobject) (widget pobject) (name cffi-keyword) (value pobject))

(defgeneric child-property-type (container key))

(defmethod child-property-type ((container container) (key symbol))
  (child-property-type container (string-downcase key)))

(defmethod child-property-type ((container container) key)
  "Should return GType of property KEY."
  (let ((skey (string-downcase key)))
    (or (cdr (assoc skey (%child-properties container) :test #'string=))
        (let* ((gclass (make-instance 'g-object-cffi:g-object-class 
                                      :object container))
               (prop (find-child-property gclass skey)))
          (when prop
            (let ((g-type (g-type prop)))
              (setf (%child-properties container)
                    (acons skey g-type (%child-properties container)))
              g-type)))
        (error "Incorrect child property name ~a" key))))

(defgeneric child-property (widget parent &key keys)
  (:method ((widget widget) (parent container) &rest keys)
    (funcall (lambda (x) (if (cdr x) x (car x)))
             (mapcar (lambda (key)
                       (with-g-value
                           (:g-type (child-property-type parent key))
                         (gtk-container-child-get-property 
                          parent widget key *g-value*)))
                     keys)))

  (:method ((widget widget) (parent null) &rest keys)
    (apply #'child-property `(,widget ,(parent widget) ,@keys))))

(defgeneric (setf child-property) (values widget parent &key keys)
  (:documentation "
Usage: (setf (child-property object parent :property) value)
       (setf (child-property object parent :prop1 :prop2) 
             (list value1 value2))")
  (:method (values (widget widget) (parent container) &rest keys)
    (mapc (lambda (key value)
            (declare (type (or symbol string) key))
            (with-g-value (:value value 
                                  :g-type (child-property-type parent key))
              (gtk-container-child-set-property parent widget 
                                                key *g-value*)))
          keys (if (listp values) values (list values))))

  (:method (values (widget widget) (parent null) &rest keys)
    (apply #'(setf child-property) `(,values ,widget ,(parent widget) ,@keys))))

(defcfun "gtk_container_class_find_child_property" :pointer
  (obj-class pobject) (key :string))

(defgeneric find-child-property (container key)
  (:method ((container container) key)
    (let ((ptr (gtk-container-class-find-child-property container key)))
      (unless (null-pointer-p ptr)
        (make-instance 'g-object-cffi:g-param-spec :pointer ptr)))))

(defcfun gtk-container-remove :void (container pobject) (widget pobject))

(defmethod container-remove ((container container) (widget widget))
  (gtk-container-remove container widget))

(defcfun gtk-container-propagate-draw 
    :void (container pobject) (child pobject) (context :pointer))

(defmethod propagate-draw ((container container) (widget widget) 
                           &optional (context cl-cairo2:*context*))
  (gtk-container-propagate-draw container widget 
                                (cl-cairo2::get-pointer context)))
