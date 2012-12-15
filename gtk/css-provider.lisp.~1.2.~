(in-package :gtk-cffi)

(defclass css-provider (g-object style-provider)
  ())

(defcfun gtk-css-provider-get-default :pointer)
(defcfun gtk-css-provider-get-named :pointer (name :string) (variant :string))
(defcfun gtk-css-provider-new :pointer)

(defmethod gconstructor ((css-provide css-provider) &key name variant default)
  (cond
    (default (gtk-css-provider-get-default))
    (name (gtk-css-provider-get-named name variant))
    (t (gtk-css-provider-new))))

(defcfun gtk-css-provider-load-from-data :boolean 
  (css-provider pobject) (data :string) (length :int) (g-error object)) 

(defcfun gtk-css-provider-load-from-file :boolean
  (css-provider pobject) (file g-file) (g-error object))

(defcfun gtk-css-provider-load-from-path :boolean
  (css-provider pobject) (path :string) (g-error object))

(defgeneric css-provider-load (css-provider &key data filename gfile)
  (:method  ((css-provider css-provider) &key data filename gfile)
    (with-g-error g-error
      (unless 
        (cond 
          (data (gtk-css-provider-load-from-data css-provider data -1 g-error))
          (filename (gtk-css-provider-load-from-path css-provider 
                                                     filename g-error))
          (gfile (gtk-css-provider-load-from-file css-provider gfile g-error)))
        (cerror "Continue" "CSS Provider load error: ~a" g-error)))))
