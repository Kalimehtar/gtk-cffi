(in-package :gdk-cffi)

(defcfun gdk-cairo-create :pointer (window pobject))

(defun cairo-create (window)
  (let* ((p (gdk-cairo-create window))
         (context (make-instance 'cl-cairo2:context :pointer p)))
    (tg:finalize context #'(lambda () (cl-cairo2::cairo_destroy p)))
    context))

(defcfun gdk-cairo-set-source-pixbuf :void
  (context :pointer) (pixbuf pobject) (x :double) (y :double))

(defun cairo-set-source-pixbuf (pixbuf x y 
                                &optional (context cl-cairo2:*context*))
  (gdk-cairo-set-source-pixbuf (cl-cairo2::get-pointer context) pixbuf x y))
