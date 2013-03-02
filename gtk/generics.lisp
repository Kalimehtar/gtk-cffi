(in-package :gtk-cffi)

(defgeneric selection-bounds (widget &key)) ;; text-buffer, label
(defgeneric text (widget &key)) ;; entry, label, text-buffer
(defgeneric (setf text) (value widget &key))
(defgeneric layout-offsets (object)) ;; entry, label, scale
(defgeneric (setf model) (model object)) ;; combo-box, list-store, 
                                         ;; tree-model-filter
(defgeneric (setf increments) (value object)) ;; spin-button, range
(defgeneric (setf range) (value object)) ;; spin-button, range



