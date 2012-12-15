(in-package :gtk-cffi)

(defclass file-filter (g-object)
  ())

(defcfun gtk-file-filter-new :pointer)

(defmethod gconstructor ((file-filter file-filter) &key)
  (gtk-file-filter-new))

(defslot file-filter name :string)

(defbitfield filter-flags
  :filename :uri :display-name :mime-type)

(deffuns file-filter
  (add-mime-type :void (mime-type :string))
  (add-pattern :void (pattern :string))
  (add-pixbuf-formats :void)
  (:get needed filter-flags))
  
  
    