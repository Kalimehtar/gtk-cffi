(in-package :gtk-cffi)

(defclass cell-renderer (g-object)
  ((attributes :initarg :attributes :reader attributes :initform nil)))

(defcenum cell-renderer-mode
  :inert :activatable :editable)

