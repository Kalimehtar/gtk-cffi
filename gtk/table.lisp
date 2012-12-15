(in-package :gtk-cffi)

(defclass table (container)
  ())

(defcfun gtk-table-new :pointer 
  (rows :uint) (columns :uint) (homogeneous :boolean))


(defmethod gconstructor ((table table)
                         &key homogeneous (rows 1) (columns 1) 
                         &allow-other-keys)
  (gtk-table-new rows columns homogeneous))

(defbitfield attach-options
  :expand :shrink :fill)

(defcfun gtk-table-attach-defaults :void
  (table pobject) (widget pobject) 
  (left-attach :uint) (right-attach :uint)
  (top-attach :uint) (bottom-attach :uint))

(defcfun gtk-table-attach :void
  (table pobject) (widget pobject) 
  (left-attach :uint) (right-attach :uint)
  (top-attach :uint) (bottom-attach :uint)
  (xoptions attach-options) (yoptions attach-options)
  (xpadding :uint) (ypadding :uint))

(defgeneric attach (table widget &key)
  (:method ((table table) (widget widget)
            &key (left 0) (right 1) (top 0) (bottom 1)
            (xoptions '(:expand :fill) xoptions-p) 
            (yoptions '(:expand :fill) yoptions-p)
            (xpadding 0) (ypadding 0))
    (if (and (null xoptions-p)
             (null yoptions-p)
             (eq xpadding 0)
             (eq ypadding 0))
        (gtk-table-attach-defaults table widget left right top bottom)
        (gtk-table-attach table widget left right top bottom
                          xoptions yoptions xpadding ypadding))))

(defcfun gtk-table-get-size :void
  (table pobject) (rows (:pointer :int)) (columns (:pointer :int)))

(defgeneric table-size (table)
  (:method ((table table))
    (with-foreign-outs-list ((rows :int) (columns :int)) :ignore
      (gtk-table-get-size table rows columns))))

(defcfun gtk-table-resize :void
  (table pobject) (rows :uint) (columns :uint))

(defgeneric (setf table-size) (new-size table)
  (:method ((new-size list) (table table))
    (destructuring-bind (rows columns) new-size
      (gtk-table-resize table rows columns))))

(defgeneric resize (table &key)
  (:method ((table table) &key rows columns)
    (unless (and rows columns)
      (destructuring-bind (cur-rows cur-columns) (table-size table)
        (unless rows (setf rows cur-rows))
        (unless columns (setf columns cur-columns))))
    (gtk-table-resize table rows columns)))

(defmethod pack ((table table) (list list) &rest rest)
  "Table should have list of widgets to add"
  (declare (ignore rest))
  (let ((rows (+ (first (table-size table)) 1))
        (width 1))
    (loop 
       :for i :from 0
       :for widget :in list
       :do (cond 
             ((numberp widget) (setf width widget) (incf i -1))
             ((not (null widget))  
              (attach table widget 
                      :left i :right (+ i width)
                      :top (- rows 1) :bottom rows))))))




  
  

