(in-package :gtk-cffi)

(defclass table (container)
  ())

(defcfun "gtk_table_new" :pointer
  (rows :uint)
  (columns :uint)
  (homogeneous :boolean))


(defmethod gconstructor ((table table)
                         &key homogeneous (rows 1) (columns 1) 
                         &allow-other-keys)
  (gtk-table-new rows columns homogeneous))

(defbitfield attach-options
  :expand :shrink :fill)

(defcfun "gtk_table_attach_defaults" :void
  (table pobject) (widget pobject) 
  (left-attach :uint) (right-attach :uint)
  (top-attach :uint) (bottom-attach :uint))

(defcfun "gtk_table_attach" :void
  (table pobject) (widget pobject) 
  (left-attach :uint) (right-attach :uint)
  (top-attach :uint) (bottom-attach :uint)
  (xoptions attach-options) (yoptions attach-options)
  (xpadding :uint) (ypadding :uint))

(defmethod attach ((table table) (widget widget)
                   &key (left 0) (right 1) (top 0) (bottom 1)
                   (xoptions :default) (yoptions :default)
                   (xpadding 0) (ypadding 0))
  (flet ((def (m) (if (eq m :default) '(:expand :fill) m)))
    (if (and (eq xoptions :default)
             (eq yoptions :default)
             (eq xpadding 0)
             (eq ypadding 0))
        (gtk-table-attach-defaults table widget left right top bottom)
        (gtk-table-attach table widget left right top bottom
                          (def xoptions) (def yoptions) xpadding ypadding))))

(defcfun "gtk_table_resize" :void
  (table pobject) (rows :uint) (columns :uint))

(defmethod resize ((table table) &key (rows :default) (columns :default))
  (gtk-table-resize table
                    (if (eq rows :default)
                        (property table :n-rows) rows)
                    (if (eq columns :default)
                        (property table :n-columns) columns)))

(defmethod pack ((table table) (list list) &rest rest)
  "Table should have list of widgets to add"
  (declare (ignore rest))
  (let (;(cols (max (property table :n-columns) (length list)))
        (rows (+ (property table :n-rows) 1)))
    ;(resize table :rows rows :columns cols)
    (let ((width 1))
      (loop 
         :for i :from 0
         :for widget :in list
         :do (cond 
               ((numberp widget) (setf width widget) (incf i -1))
               ((not (null widget))  
                (attach table widget 
                        :left i :right (+ i width)
                        :top (- rows 1) :bottom rows)))))))

