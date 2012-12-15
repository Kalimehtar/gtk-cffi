(in-package :gtk-cffi)

(defclass cell-renderer-toggle (cell-renderer)
  ())

(defcfun "gtk_cell_renderer_toggle_new" :pointer)

(defmethod gconstructor ((cell-renderer-toggle cell-renderer-toggle)
                         &key &allow-other-keys)
  (gtk-cell-renderer-toggle-new))