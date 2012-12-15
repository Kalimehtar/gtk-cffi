(in-package :gtk-cffi)

(defclass tree-view-column (cell-layout)
  ())

(defcfun gtk-tree-view-column-new :pointer)
(defcfun gtk-tree-view-column-new-with-area :pointer (area pobject))

(defmethod gconstructor ((tree-view-column tree-view-column)
                         &key area
                         &allow-other-keys)
  (initialize tree-view-column '(area))
  (if area 
      (gtk-tree-view-column-new-with-area area)
      (gtk-tree-view-column-new)))

(defcenum tree-view-column-sizing :grow-only :autosize :fixed)
  

(defslots tree-view-column
  title :string
  spacing :int
  visible :boolean
  resizable :boolean
  sizing tree-view-column-sizing
  fixed-width :int
  min-width :int
  max-width :int
  expand :boolean
  clickable :boolean
  widget pobject
  alignment :float
  reorderable :boolean
  sort-column-id :int
  sort-indicator :boolean
  sort-order sort-type)

(defcfun gtk-tree-view-column-pack-start :void
  (tree-view-column pobject) (renderer pobject) (expand :boolean))

(defcfun gtk-tree-view-column-pack-end :void
  (tree-view-column pobject) (renderer pobject) (expand :boolean))

(defmethod pack ((tree-view-column tree-view-column)
                 (cell-renderer cell-renderer)
                 &key end expand)
  (funcall (if end
               #'gtk-tree-view-column-pack-end
               #'gtk-tree-view-column-pack-start)
           tree-view-column cell-renderer expand)
  (iter
    (for (attr column) in (attributes cell-renderer))
    (add-attribute tree-view-column cell-renderer attr column)))

(deffuns tree-view-column
  (add-attribute :void (cell pobject) (attr cffi-keyword) (column :int))
  (clear-attributes :void (cell-renderer pobject))
  (clear :void)
  (clicked :void)
  (cell-is-visible :boolean)
  (queue-resize :void &key)
  (:get tree-view pobject)
  (:get x-offset :int)
  (focus-cell :void (cell-renderer pobject))
  (cell-set-cell-data :void (model pobject) (iter (struct tree-iter))
                      (is-expander :boolean) (is-expanded :boolean)))

(defcfun gtk-tree-view-column-set-cell-data-func :void
  (tree-view-column pobject) (renderer pobject) (func pfunction)
  (data pdata) (notify :pointer))

(defmethod (setf cell-data-func) (func
                                  (tree-view-column tree-view-column)
                                  (cell-renderer cell-renderer)
                                  &key data destroy-notify)
  (set-callback tree-view-column gtk-tree-view-column-set-cell-data-func
                cb-cell-data-func func data destroy-notify cell-renderer))



(defcfun gtk-tree-view-column-cell-get-size :void
  (column pobject) (cell-renderer pobject) (area (struct rectangle))
  (x-offset :pointer) (y-offset :pointer) (width :pointer) (height :pointer))

(defmethod cell-get-size ((tree-view-column tree-view-column)
                          (cell-renderer cell-renderer) area)
  (with-foreign-outs-list
      ((x-offset :int) (y-offset :int) (width :int) (height :int)) :ignore
    (gtk-tree-view-column-cell-get-size tree-view-column cell-renderer area
                                        x-offset y-offset width height)))

(defcfun gtk-tree-view-column-cell-get-position :boolean
  (column pobject) (cell-renderer pobject)
  (start-pos :pointer) (width :pointer))

(defmethod cell-get-position ((tree-view-column tree-view-column)
                              (cell-renderer cell-renderer))
  (with-foreign-outs-list
      ((start-pos :int) (width :int)) :if-success
    (gtk-tree-view-column-cell-get-position tree-view-column
                                            cell-renderer start-pos width)))


(defmethod get-cell-at ((tree-view-column tree-view-column) x)
  (loop :for cell in (cells tree-view-column)
        :when (destructuring-bind (start-pos width)
                  (cell-get-position tree-view-column cell)
                (and (>= x start-pos) (>= (+ start-pos width) x)))
        :return cell))

(init-slots tree-view-column (cell attributes)
    (when cell
      (pack tree-view-column cell)
      (iter
        (for (key value) on attributes by #'cddr)
        (add-attribute tree-view-column cell key value))))