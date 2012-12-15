(in-package :gtk-cffi)

(defclass cell-layout (g-object)
  ())

(defcfun gtk-cell-layout-pack-start :void
  (cell-layout pobject) (renderer pobject) (expand :boolean))

(defcfun gtk-cell-layout-pack-end :void
  (cell-layout pobject) (renderer pobject) (expand :boolean))

(defmethod pack ((cell-layout cell-layout)
                 (cell-renderer cell-renderer)
                 &key end expand)
  (funcall (if end
               #'gtk-cell-layout-pack-end
               #'gtk-cell-layout-pack-start)
           cell-layout cell-renderer expand)
  (iter
    (for (attr column) in (attributes cell-renderer))
    (add-attribute cell-layout cell-renderer attr column)))

(deffuns cell-layout
  (add-attribute :void (cell pobject) (attr cffi-keyword) (column :int))
  (:get cells g-list-object)
  (:get area pobject)
  (reorder :void (cell pobject) (poisition :int))
  (clear-attributes :void (cell-renderer pobject))
  (clear :void))

(defcallback cb-cell-data-func :void
  ((cell-layout pobject) (cell-renderer pobject)
   (model pobject) (tree-iter (struct tree-iter)) (data pdata))
  (funcall data cell-layout cell-renderer model tree-iter))

(defcfun gtk-cell-layout-set-cell-data-func :void
  (cell-layout pobject) (renderer pobject) (func pfunction)
  (data pdata) (notify :pointer))

(defmethod (setf cell-data-func) (func
                                  (cell-layout cell-layout)
                                  (cell-renderer cell-renderer)
                                  &key data destroy-notify)
  (set-callback cell-layout gtk-cell-layout-set-cell-data-func
                cb-cell-data-func func data destroy-notify cell-renderer))

