(in-package :gtk-cffi)

(defclass cell-renderer-text (cell-renderer)
  ())

(defcfun "gtk_cell_renderer_text_new" :pointer)

(defmethod gconstructor ((cell-renderer-text cell-renderer-text)
                         &key &allow-other-keys)
  (gtk-cell-renderer-text-new))


