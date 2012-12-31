;;; GtkTreeView
;;;
;;; (foreach tree-view ...) = gtk-tree-view-map-expanded-rows
;;; (path-at-pos ... :is-blank t) = gtk-tree-view-is-blank-at-pos
;;; (convert-bin-window-to-widget tree-view x y) -> (list wx wy) = 
;;;              gtk-tree-view-convert-bin-window-to-widget-coords
;;; (convert-{smth} ...) = gtk-tree-view-convert-{smth}-coords

(in-package :gtk-cffi)

(defclass tree-view (container)
  ())

(defcenum tree-view-grid-lines
  :none :horizontal :vertical :both) 

(defcfun gtk-tree-view-new :pointer)
(defcfun gtk-tree-view-new-with-model :pointer (model pobject))

(defmethod gconstructor ((tree-view tree-view)
                         &key model &allow-other-keys)
  (if model
      (gtk-tree-view-new-with-model model)
    (gtk-tree-view-new)))

(defslots tree-view
  level-indentation :int
  show-expanders :boolean
  model pobject
  hadjustment pobject
  vadjustment pobject
  headers-visible :boolean
  headers-clickable :boolean
  rules-hint :boolean
  hover-selection :boolean
  hover-expand :boolean
  rubber-banding :boolean
  search-column :int
  expander-column pobject
  reorderable :boolean)
  

(deffuns tree-view 
  (remove-column :int (column pobject))
  (append-column :int (column pobject))
  (insert-column :int (column pobject) (position :int) &key)
  (:get selection pobject)
  (:get columns g-list-object)
  (:get column pobject (n :int))
  (:get n-columns :int)
  (move-column-after :void (column pobject) (base-column pobject))
  (scroll-to-point :void (x :int) (y :int))
  (row-activated :void (path tree-path) (comumn pobject))
  (expand-all :void)
  (collapse-all :void)
  (expand-to-path :void (path tree-path))
  (expand-row :void (path tree-path) (open-all :boolean))
  (collapse-row :void (path tree-path))
  (row-expanded :boolean (path tree-path))
  (:get bin-window pobject))
  
  
  

(defcfun gtk-tree-view-scroll-to-cell :void 
  (tree-view pobject) (path ptree-path) (column pobject) (use-align :boolean) 
  (row-align :float) (col-align :float))

(defgeneric scroll-to-cell (tree-view path column &key row-align col-align)
  (:method ((tree-view tree-view) path column 
            &key (row-align 0.0 row-align-p) (col-align 0.0 col-align-p))
    (gtk-tree-view-scroll-to-cell tree-view path column 
                                  (or row-align-p col-align-p) 
                                  row-align col-align)))


(defmethod (setf columns) (columns (tree-view tree-view))
  (dolist (column (columns tree-view))
    (remove-column tree-view column))
  (labels
      ((mk-column (column num)
         (typecase column
           (string (make-instance 'tree-view-column 
                                  :title column
                                  :cell (make-instance 'cell-renderer-text)
                                  :attributes `(:text ,num)))
           (cons (apply #'make-instance
                        'tree-view-column column))
           (t column))))
    (reduce (lambda (num column)
              (append-column tree-view (mk-column column num)))
            columns :initial-value 0)))
(save-setter tree-view columns)
       

(defcfun gtk-tree-view-get-cursor :void (view pobject)
  (path :pointer) (column :pointer))

(defgeneric cursor (tree-view)
  (:method ((tree-view tree-view))
    (with-foreign-outs-list ((path 'tree-path) (column 'pobject)) :ignore
      (gtk-tree-view-get-cursor tree-view path column))))

(defcfun gtk-tree-view-set-cursor :void
  (tree-view pobject) (path tree-path) (focus-column pobject)
  (start-editing :boolean))

(defcfun gtk-tree-view-set-cursor-on-cell :void
  (tree-view pobject) (path tree-path) (focus-column pobject)
  (focus-cell pobject) (start-editing :boolean))

(defgeneric (setf cursor) (path+column tree-view &key start-editing cell)
  (:method (path+column (tree-view tree-view) &key start-editing cell)
    (destructuring-bind (path column) path+column
      (if cell
          (gtk-tree-view-set-cursor-on-cell tree-view path column 
                                            cell start-editing)
          (gtk-tree-view-set-cursor tree-view path column start-editing)))
    path+column))
          

(defcfun gtk-tree-view-insert-column-with-data-func :int
  (tree-view pobject) (position :int) (title :string) (cell pobject)
  (data-func pfunction) (data pdata) (destroy pfunction))

(defmethod insert-column ((tree-view tree-view) (cell cell-renderer) position 
                          &key title func data destroy-notify)
  (set-callback tree-view gtk-tree-view-insert-column-with-data-func
                cb-cell-data-func func data destroy-notify 
                position title cell))

(defcfun gtk-tree-view-set-column-drag-function :void
  (tree-view pobject) (func pfunction) (user-data pdata) (destroy pfunction))

(defcallback cb-column-drop-function :boolean
    ((tree-view pobject) (column pobject) (prev-column pobject) 
     (next-column pobject) (data pdata))
  (funcall data tree-view column prev-column next-column))

(defgeneric (setf column-drag-function) (func tree-view 
                                              &key data destroy-notify)
  (:documentation "gtk_tree_view_set_column_drag_function")
  (:method (func (tree-view tree-view) &key data destroy-notify)
    (set-callback tree-view gtk-tree-view-set-column-drag-function
                  cb-column-drop-function func data destroy-notify)))

(make-foreach (tree-view gtk-tree-view-map-expanded-rows)
              (path ptree-path) (data pdata))

(defcfun gtk-tree-view-is-blank-at-pos :boolean
  (tree-view pobject) (x :int) (y :int)
  (path :pointer) (column :pointer) (cell-x :pointer) (cell-y :pointer))

(defcfun gtk-tree-view-get-path-at-pos :boolean
  (tree-view pobject) (x :int) (y :int)
  (path :pointer) (column :pointer) (cell-x :pointer) (cell-y :pointer))

(defgeneric path-at-pos (tree-view x y &key is-blank)
  (:documentation "if is-blank gtk-tree-view-is-blank-at-pos called, else
gtk-tree-view-path-at-pos")
  (:method ((tree-view tree-view) x y &key is-blank)
    (with-foreign-outs ((path 'tree-path) (column 'pobject) 
                        (cell-x :int) (cell-y :int)) :return
      (funcall (if is-blank #'gtk-tree-view-is-blank-at-pos 
                   #'gtk-tree-view-get-path-at-pos)
               tree-view x y path column cell-x cell-y))))

(macrolet ((get-area (area-type)
             (let ((cname (symbolicate 'gtk-tree-view-get- area-type '-area))
                   (lname (symbolicate area-type '-area)))
             `(progn
                (defcfun ,cname :void
                  (tree-view pobject) (path tree-path) (column pobject)
                  (rect (struct rectangle :out t)))
                (defgeneric ,lname
                    (tree-view path column)
                  (:method ((tree-view tree-view) path column)
                    (let ((res (make-instance 'rectangle)))
                      (,cname tree-view path column res)
                      res)))))))
  (get-area background)
  (get-area cell))

(defcfun gtk-tree-view-get-visible-rect :void
  (tree-view pobject) (visible-rect (struct rectangle :out t)))

(defgeneric visible-rect (tree-view)
  (:method ((tree-view tree-view))
    (let ((res (make-instance 'rectangle)))
      (gtk-tree-view-get-visible-rect tree-view res)
      res)))

(defcfun gtk-tree-view-get-visible-range :void
  (tree-view pobject) (start-path :pointer) (end-path :pointer))

(defgeneric visible-range (tree-view)
  (:method ((tree-view tree-view))
    (with-foreign-outs-list ((start-path 'tree-path) (end-path 'tree-path))
        :ignore
      (gtk-tree-view-get-visible-range tree-view start-path end-path))))

(macrolet ((def-coords (from to)
             (flet ((name-coord (sym1 sym2)
                      (symbolicate (aref (symbol-name sym1) 0) sym2)))
               (let ((cfun (symbolicate 'gtk-tree-view-convert- from 
                                        '-to- to '-coords))
                     (lfun (symbolicate 'convert- from '-to- to))
                     (from-x (name-coord from 'x))
                     (from-y (name-coord from 'y))
                     (to-x (name-coord to 'x))
                     (to-y (name-coord to 'y)))
                 `(progn 
                    (defcfun ,cfun :void 
                      (tree-view pobject) 
                      (,from-x :int) (,from-y :int) 
                      (,to-x :pointer) (,to-y :pointer))
                    (defgeneric ,lfun (tree-view x y)
                      (:method ((tree-view tree-view) x y)
                        (with-foreign-outs-list ((,to-x :int) (,to-y :int)) 
                            :ignore
                            (,cfun tree-view x y ,to-x ,to-y)))))))))
  (def-coords bin-window tree)
  (def-coords bin-window widget)
  (def-coords tree bin-window)
  (def-coords tree widget)
  (def-coords widget bin-window)
  (def-coords widget tree))

(defcfun gtk-tree-view-enable-model-drag-dest :void
  (tree-view pobject) (targets (carray (struct target-entry)))
  (n-targets :int) (action drag-action))

(init-slots tree-view (on-select)
  (when on-select
    (setf (gsignal (selection tree-view) :changed)
          (lambda (selection)
            (destructuring-bind (rows model) (selected-rows selection)
              (when rows
                (apply on-select model rows)))))))