(in-package :gtk-cffi)

(defclass tree-model-filter (g-object tree-model)
  ((model :accessor model :initarg :model)))

(defcfun "gtk_tree_model_filter_new" :pointer (model pobject) (path pobject))

(defmethod gconstructor ((tree-model-filter tree-model-filter)
                         &key model path &allow-other-keys)
  (gtk-tree-model-filter-new model path))

(defmethod columns ((tree-model-filter tree-model-filter))
  (columns (model tree-model-filter)))

(defcfun "gtk_tree_model_filter_set_visible_column" :void
  (model pobject) (column :int))

(defmethod (setf visible-column) (column (tree-model-filter tree-model-filter))
  (gtk-tree-model-filter-set-visible-column tree-model-filter
                                            (round column)))

(defcfun "gtk_tree_model_filter_convert_child_path_to_path" :pointer
  (model pobject) (path pobject))

(defmethod path-from-child ((tree-model-filter tree-model-filter)
                            (tree-path tree-path))
  (let ((ptr (gtk-tree-model-filter-convert-child-path-to-path
              tree-model-filter tree-path)))
    (unless (null-pointer-p ptr)
      (make-instance 'tree-path :pointer ptr))))

(defmacro with-parent-path (path parent child-path &body body)
  `(let ((,path (path-from-child ,parent ,child-path)))
     (unwind-protect
         (progn ,@body)
       (when ,path (free ,path)))))

(defcfun "gtk_tree_model_filter_convert_iter_to_child_iter" :void
  (model pobject) (child-iter pobject) (iter pobject))

(defmethod iter-to-child ((tree-model-filter tree-model-filter)
                          (tree-iter tree-iter))
  (let ((child-iter (make-instance 'tree-iter)))
    (gtk-tree-model-filter-convert-iter-to-child-iter
     tree-model-filter child-iter tree-iter)
    child-iter))

(defmacro with-child-iter (child-iter parent iter &body body)
  `(let ((,child-iter (iter-to-child ,parent ,iter)))
     (unwind-protect
         (progn ,@body)
       (when ,child-iter (free ,child-iter)))))

(defcfun "gtk_tree_model_filter_get_model" pobject (model pobject))

(defmethod (setf model-values)
  (values (tree-model-filter tree-model-filter)
          &key (iter (iter tree-model-filter)) col
          (columns (when col (list col))))
  (with-child-iter child-iter tree-model-filter iter
    (setf (model-values (model tree-model-filter) 
                        :iter child-iter :columns columns) values)))

  

