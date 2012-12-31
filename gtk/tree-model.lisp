;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tree-model.lisp --- GtkTreeModel, GtkTreePath, GtkTreeIter, 
;;;                     GtkTreeRowReference
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;
(in-package #:gtk-cffi)

;; I think, that tree-path as a pointer is not useful on Lisp side
;; so it will be represented as a lisp array

(defcfun gtk-tree-path-new :pointer)
(defcfun gtk-tree-path-free :void (path :pointer))
(defcfun gtk-tree-path-new-from-string :pointer (str :string))
(defcfun gtk-tree-path-append-index :void (path :pointer) (index :int))

(defcfun gtk-tree-path-get-indices-with-depth :pointer 
  (path :pointer) (depth :pointer))

(define-foreign-type tree-path (freeable)
  ((free-from-foreign :initform t)) ; NB: except callbacks
  (:simple-parser tree-path)
  (:actual-type :pointer))

(defmethod translate-from-foreign (ptr (tree-path tree-path))
  (unless (null-pointer-p ptr)
    (with-foreign-object (pdepth :int)
      (let* ((indices (gtk-tree-path-get-indices-with-depth ptr pdepth))
             (depth (mem-ref pdepth :int))
             (res (make-array depth :element-type 'fixnum)))
        (dotimes (i depth res)
          (setf (aref res i) (mem-aref indices :int i)))))))

(defmethod translate-to-foreign ((value array) (tree-path tree-path))
  (let ((res (gtk-tree-path-new)))
    (dotimes (i (length value) res)
      (gtk-tree-path-append-index res (aref value i)))))

(defmethod translate-to-foreign ((value list) (tree-path tree-path))
  (let ((res (gtk-tree-path-new)))
    (dolist (i value res)
      (gtk-tree-path-append-index res i))))

(defmethod translate-to-foreign ((value string) (tree-path tree-path))
  (gtk-tree-path-new-from-string value))

(defmethod translate-to-foreign ((value null) (tree-path tree-path))
  (null-pointer))


(defmethod free-ptr ((tree-path (eql 'tree-path)) ptr)
  (gtk-tree-path-free ptr))

(define-foreign-type ptree-path (tree-path)
  ((free-from-foreign :initform nil))
  (:documentation "Tree path for callbacks")
  (:simple-parser ptree-path)
  (:actual-type :pointer))

(defmethod free-ptr ((tree-path (eql 'ptree-path)) ptr)
  (gtk-tree-path-free ptr))

(defclass tree-row-reference (object)
  ())

(defcfun gtk-tree-row-reference-new :pointer (model pobject) (path tree-path))

(defcfun gtk-tree-row-reference-free :void (row pobject))

(defmethod gconstructor ((tree-row-reference tree-row-reference)
                         &key model path &allow-other-keys)
  (gtk-tree-row-reference-new model path))

(defmethod free-ptr ((class (eql 'tree-row-reference)) ptr)
  (gtk-tree-row-reference-free ptr))

(deffuns tree-row-reference
  (copy (object tree-row-reference))
  (:get model pobject)
  (:get path tree-path)
  (valid :boolean))
  

(defcstruct* tree-iter
    "GtkTreeIter"
  (stamp :int)
  (u1 :pointer)
  (u2 :pointer)
  (u3 :pointer))


(defclass tree-model (object)
  ((columns :accessor columns :initarg :columns)
   (iter :accessor tree-iter :documentation "Current tree-iter")))

(defcstruct tree-model-iface
    "GtkTreeModelIface"
  (g-iface g-type-interface) ; :struct
  (row-changed :pointer)
  (row-inserted :pointer)
  (has-child-toggled :pointer)
  (row-deleted :pointer)
  (row-reordered :pointer)

  ; virtual methods
  (get-flags :pointer)
  (get-n-columns :pointer)
  (get-column-type :pointer)
  (get-iter :pointer)
  (get-path :pointer)
  (get-value :pointer)
  (iter-next :pointer)
  (iter-previous :pointer)
  (iter-children :pointer)
  (iter-has-child :pointer)
  (iter-n-children :pointer)
  (iter-nth-child :pointer)
  (iter-parent :pointer)
  (ref-node :pointer)
  (unref-node :pointer))

(defmethod initialize-instance
  :after ((tree-model tree-model)
          &key &allow-other-keys)
  (setf (tree-iter tree-model) (make-instance 'tree-iter :new-struct t 
                                              :free-after nil)))

(defmethod free :before ((tree-model tree-model))
  (free (tree-iter tree-model)))

(make-foreach tree-model
              (model pobject)
              (path ptree-path) 
              (tree-iter (object tree-iter))
              (data pdata))

(defcfun gtk-tree-model-get-path tree-path 
  (model pobject) (tree-iter (struct tree-iter)))

(defgeneric iter->path (tree-model tree-iter)
  (:method ((tree-model tree-model) (tree-iter tree-iter))
    (gtk-tree-model-get-path tree-model tree-iter)))

(defcfun gtk-tree-model-get-string-from-iter :string
  (model pobject) (tree-iter (struct tree-iter)))

(defgeneric iter->string (tree-model tree-iter)
  (:method ((tree-model tree-model) (tree-iter tree-iter))
    (gtk-tree-model-get-string-from-iter tree-model tree-iter)))

(defcfun gtk-tree-model-get-value :void (model pobject) (iter pobject)
  (column :int) (g-value pobject))

(defgeneric model-values (tree-model &key tree-iter column columns)
  (:method ((tree-model tree-model) 
            &key (tree-iter (tree-iter tree-model)) 
                 column 
                 (columns (ensure-list column)))
    "columns = num0 &optional num1 num2 ..."
                                        ;(format t "model-values: ~a ~a ~a~%" tree-model tree-iter cols)
    (mapcar
     (lambda (col) 
       (with-g-value ()  
         (gtk-tree-model-get-value tree-model
                                   tree-iter col *g-value*)))
     columns)))

(defcfun gtk-tree-model-get-iter :boolean
  (model pobject) (iter (struct tree-iter :out t)) (path tree-path))

(defcfun gtk-tree-model-get-iter-from-string :boolean
    (model pobject) (tree-iter (struct tree-iter :out t)) (path :string))

(defgeneric path->iter (tree-model tree-path-string &optional tree-iter)
  (:method ((tree-model tree-model) tree-path
            &optional (tree-iter (tree-iter tree-model)))
    (when (gtk-tree-model-get-iter tree-model tree-iter tree-path)
      tree-iter))
  (:method ((tree-model tree-model) (tree-path-string string)
            &optional (tree-iter (tree-iter tree-model)))
    (when (gtk-tree-model-get-iter-from-string tree-model 
                                               tree-iter tree-path-string)
      tree-iter)))

(defmacro with-tree-iter (var &body body)
  `(with-object (,var) (make-instance 'tree-iter)
                ,@body))

(defbitfield tree-model-flags :iters-persist :list-only)

(deffuns tree-model
  (:get n-columns :int)
  (:get column-type g-type (col :int))
  (:get flags tree-model-flags)
  (iter-has-child :boolean (tree-iter (struct tree-iter)))
  (iter-n-children :int (tree-iter (struct tree-iter)))
  (ref-node :void (tree-iter (struct tree-iter)))
  (unref-node :void (tree-iter (struct tree-iter)))
  (row-changed :void (path tree-path) (tree-iter (struct tree-iter)))
  (row-inserted :void (path tree-path) (tree-iter (struct tree-iter)))
  (row-has-child-toggled :void (path tree-path) (tree-iter (struct tree-iter)))
  (row-deleted :void (path tree-path))
  (rows-reordered :void 
                  (path tree-path) (tree-iter (struct tree-iter))
                  (new-order (carray :int))))

(template 
    (name lisp-name) 
    ((get-iter-first iter-first)
     (iter-next iter-next)
     (iter-previous iter-previous))
  (let ((c-name (symbolicate 'gtk-tree-model- name)))
    `(progn
       (defcfun ,c-name :boolean
         (model pobject) (tree-iter (struct tree-iter :out t)))
       (defgeneric ,lisp-name (tree-model &optional tree-iter)
         (:method ((tree-model tree-model) 
                   &optional (tree-iter (tree-iter tree-model)))
           (when (,c-name tree-model tree-iter)
             tree-iter))))))

(defcfun gtk-tree-model-iter-nth-child :boolean
  (model pobject) (tree-iter (struct tree-iter :out t))
  (parent (struct tree-iter)) (n :int))

(defgeneric iter-nth-child (tree-model parent n &optional tree-iter)
  (:method ((tree-model tree-model) parent n
            &optional (tree-iter (tree-iter tree-model)))
    (when (gtk-tree-model-iter-nth-child tree-model tree-iter parent n)
      tree-iter)))

(defcfun gtk-tree-model-iter-parent :boolean
  (model pobject) (tree-iter (struct tree-iter :out t))
  (child (struct tree-iter)))

(defgeneric iter-parent (tree-model child &optional tree-iter)
  (:method ((tree-model tree-model) child
            &optional (tree-iter (tree-iter tree-model)))
    (when (gtk-tree-model-iter-parent tree-model tree-iter child)
      tree-iter)))
