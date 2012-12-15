(in-package :gtk-cffi)

(defclass combo-box (bin cell-layout)
  ())

(defcfun gtk-combo-box-new :pointer)
(defcfun gtk-combo-box-new-with-entry :pointer)
(defcfun gtk-combo-box-new-with-model :pointer (model pobject)) 
(defcfun gtk-combo-box-new-with-model-and-entry :pointer (model pobject))
(defcfun gtk-combo-box-new-with-area :pointer (area pobject)) 
(defcfun gtk-combo-box-new-with-area-and-entry :pointer (area pobject))

(defmethod gconstructor ((combo-box combo-box)
                         &key model area entry &allow-other-keys)
  (initialize combo-box '(model area entry))
  (cond 
    (model
     (if entry
         (gtk-combo-box-new-with-model-and-entry model)
         (gtk-combo-box-new-with-model model)))
    (area
     (if entry
         (gtk-combo-box-new-with-area-and-entry area)
         (gtk-combo-box-new-with-area area)))
    (t
     (if entry
         (gtk-combo-box-new-with-entry)
         (gtk-combo-box-new)))))

(defslots combo-box
  wrap-width :int
  row-span-column :int
  column-span-column :int
  active :int
  id-column :int
  add-tearoffs :boolean
  title :string
  focus-on-click :boolean
  button-sensitivity sensitivity-type
  entry-text-column :int
  model pobject
  popup-fixed-width :boolean)

(deffuns combo-box
  (:get active-id :string)
  (popup-for-device :void (device pobject))
  (popup :void)
  (popdown :void)
  (:get row-separator-func :pointer)
  (:get has-entry :boolean))


(defcallback cb-row-separator-func 
    :boolean ((model pobject) (iter pobject) (data pdata))
  (funcall data model iter))

(defcfun gtk-combo-box-set-row-separator-func :void
  (combo-box pobject) (func pfunction) (data pdata) (notify pfunction))


(defgeneric (setf row-separator-func) (func combo-box &key data destroy-notify)
  (:method (func (combo-box combo-box) &key data destroy-notify)
    (set-callback combo-box gtk-combo-box-set-row-separator-func
                  cb-row-separator-func func data destroy-notify)))



(defcfun gtk-combo-box-set-active-id :boolean 
  (combo-box pobject) (active-id :string))
(defgeneric (setf active-id) (active-id combo-box)
  (:method (active-id (combo-box combo-box))
    (values active-id
            (gtk-combo-box-set-active-id combo-box active-id))))
(save-setter combo-box active-id)

(defcfun gtk-combo-box-set-active-iter 
    :void (combo-box pobject) (iter (struct tree-iter :free-to-foreign nil)))
(defcfun gtk-combo-box-get-active-iter 
    :boolean (combo-box pobject) (iter (struct tree-iter :out t)))

(defgeneric (setf active-iter) (active-iter combo-box)
  (:method (active-iter (combo-box combo-box))
    (gtk-combo-box-set-active-iter combo-box active-iter)
    active-iter))
(save-setter combo-box active-iter)

(defgeneric active-iter (combo-box)
  (:method ((combo-box combo-box))
    (let ((res (make-instance 'tree-iter)))
      (values res (gtk-combo-box-get-active-iter combo-box res)))))

(init-slots combo-box)






  
