(in-package :gtk-cffi-ext)

(defmethod show ((model-impl lisp-model-impl) &key (columns '("List"))
                 &allow-other-keys)
  (show
   (gtk-model 
     'window
     ('scrolled-window
      ('tree-view :model (make-instance 'lisp-model
                                        :implementation model-impl)
                  :columns columns)))))

(defmethod show ((seq sequence) &key &allow-other-keys)
  (show
   (if (some #'consp seq)
       (make-instance 'lisp-model-tree-array
                      :tree (labels ((process (x)
                                       (if (consp x)
                                           (cons (list (car x)) 
                                                 (mapcar #'process (cdr x)))
                                           (list (list x)))))
                              (mapcar #'process (coerce seq 'list)))
                      :columns '(:string))
       (make-instance 'lisp-model-array 
                      :array (map 'vector 
                                  (compose #'list #'princ-to-string)
                                  seq)
                      :columns '(:string)))))

;; (defun status-tree ()
;;   (let ((tree-model (make-instance 'tree-strore)))
;;     (show
;;      (gtk-model
;;        'window
;;        ('scrolled-window
;;         ('tree-view :model tree-model))))))
                 