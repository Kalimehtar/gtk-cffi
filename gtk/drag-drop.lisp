(in-package :gtk-cffi)

(defbitfield dest-defaults :motion :highlight :drop (:all #x7))

(defcfun gtk-drag-dest-set :void (widget pobject)
 (targets (carray (struct target-entry))) (n-targets :int) (action drag-action))

(defun drag-dest-set (widget targets action)
  (gtk-drag-dest-set widget targets (length targets) action))