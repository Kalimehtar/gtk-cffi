(in-package :gtk-cffi)

(defclass tree-selection (g-object)
  ())

(defcenum selection-mode
  :none :single :browse :multiple)

(defslot tree-selection mode selection-mode)

(deffuns tree-selection
  (:get select-function :pointer)
  (:get user-data pdata)
  (:get tree-view pobject)
  (count-selected-rows :int)
  (select-path :void (path tree-path))
  (unselect-path :void (path tree-path))
  (path-is-selected :boolean (path tree-path))
  (select-iter :void (tree-iter (struct tree-iter)))
  (unselect-iter :void (tree-iter (struct tree-iter)))
  (iter-is-selected :boolean (tree-iter (struct tree-iter)))
  (select-all :void)
  (unselect-all :void)
  (select-range :void (start-path tree-path) (end-path tree-path))
  (unselect-range :void (start-path tree-path) (end-path tree-path)))
  

(defcallback cb-tree-selection-func :boolean
    ((selection pobject) (model pobject) (path ptree-path)
     (path-currently-selected :boolean) (data pdata))
  (funcall data selection model path path-currently-selected))

(defcfun gtk-tree-selection-set-select-function :void
  (selection pobject) (func pfunction) (data pdata) (destroy pfunction))

(defgeneric (setf select-function) (tree-selection func 
                                                   &key data destroy-notify)
  (:method ((tree-selection tree-selection) func &key data destroy-notify)
    (set-callback tree-selection gtk-tree-selection-set-select-function
                  cb-tree-selection-func func data destroy-notify)))

(defcfun gtk-tree-selection-get-selected :boolean
  (selection pobject) (model :pointer) (tree-iter (struct tree-iter :out t)))

(defgeneric selected (tree-selection)
  (:method ((tree-selection tree-selection))
    (let ((tree-iter (make-instance 'tree-iter)))
      (with-foreign-object (p :pointer)
        (when (gtk-tree-selection-get-selected tree-selection p tree-iter)
          (values tree-iter (convert-from-foreign (mem-ref p :pointer) 
                                                  'pobject)))))))

(defcfun gtk-tree-selection-get-selected-rows (g-list :elt tree-path)
  (selection pobject) (model :pointer))
                                             
(defgeneric selected-rows (tree-selection)
  (:method ((tree-selection tree-selection))
    (with-foreign-object (p :pointer)
      (values (gtk-tree-selection-get-selected-rows tree-selection p)
              (mem-ref p 'pobject)))))
    

(make-foreach (tree-selection gtk-tree-selection-selected-foreach)
              (model pobject) (path tree-path :free-from-foreign nil) 
              (tree-iter (struct tree-iter)) (data pdata))

;; (defcfun gtk-tree-selection-selected-foreach :void
;;   (selection pobject) (func pfunction) (data pdata))

;; (defvar *tree-selection-foreach* nil)

;; (defcallback cb-tree-selection-foreach :boolean
;;   ((model pobject) (path tree-path :free-from-foreign nil) 
;;    (tree-iter (struct tree-iter)) (data pdata))
;;   (when *tree-selection-foreach*
;;     (funcall *tree-selection-foreach* model path tree-iter data)))

;; (defmethod foreach ((tree-selection tree-selection)
;;                                func &optional (data (null-pointer)))
;;   (when func    
;;     (let ((*tree-selection-foreach* func))
;;       (gtk-tree-selection-selected-foreach 
;;        tree-selection (if (functionp func)
;;                           (callback cb-tree-selection-foreach) func)
;;        data))))





