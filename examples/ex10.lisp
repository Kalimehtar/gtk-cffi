(asdf:oos 'asdf:load-op :gtk-cffi-ext)
;(declaim (optimize speed))
(defpackage #:test-10
  (:use #:common-lisp #:iter #:gtk-cffi #:gtk-cffi-ext #:g-object-cffi))
(in-package #:test-10)

(gtk-init)
(defparameter *model*
  (make-instance 'lisp-model
                 :implementation
                 (make-instance 'lisp-model-tree-array
                                :tree '((("ok" 1) 
                                         (("Ok2" 2))) 
                                        (("ok3" 3)))
                                :columns '(:string :int))))
                                ;:array #(("ok" 1))
                                ;:columns '(:string :int))))

;; (defparameter *model0*
;;   (make-instance 'list-store :columns '(:int)))

;; (append-values *model0* '(1))
;; (append-values *model0* '(2))
;; (append-values *model0* '(3))

;; (let ((arr (make-array 0 :adjustable t :fill-pointer 0)))
;;   (iter (for i from 1 to 100000)
;;         (vector-push-extend (list (format nil "str ~a" i) i) arr))
;;   (setf (larray (implementation *model*)) arr))

(defparameter *window*
  (gtk-model
    'window :width 400
            :height 400
            :signals '(:destroy :gtk-main-quit)
    ('scrolled-window
     ('tree-view :model *model* :columns '("Test str" "Test int")))))


;(show *window*)
;(show #(1 2 3 4 5))

(show (make-instance 'lisp-model-tree-array
                     :tree '((("ok" 1) 
                              (("Ok2" 2))) 
                             (("ok3" 3)))
                     :columns '(:string :int))
      :columns '("str" "int"))

(show '((1 2 3) 4))

(gtk-main)
