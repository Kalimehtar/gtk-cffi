(in-package :gtk-cffi)

(defclass cell-renderer-pixbuf (cell-renderer)
  ())

(defcfun "gtk_cell_renderer_pixbuf_new" :pointer)

(defmethod gconstructor ((cell-renderer-pixbuf cell-renderer-pixbuf)
                         &key &allow-other-keys)
  (gtk-cell-renderer-pixbuf-new))
