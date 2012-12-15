(in-package :gtk-cffi)

(defclass statusbar (box)
  ())

(defcfun gtk-statusbar-new :pointer)

(defmethod gconstructor ((statusbar statusbar) &key &allow-other-keys)
  (gtk-statusbar-new))

(deffuns statusbar
  ((statusbar-push . push) :uint (context-id :uint) (text :string))
  ((statusbar-pop . pop) :void (context-id :uint))
  (:get context-id :uint (context :string))
  (:get message-area pobject))

(defcfun gtk-statusbar-remove :void
  (statusbar pobject) (context-id :uint) (message-id :uint))
(defcfun gtk-statusbar-remove-all :void
  (statusbar pobject) (context-id :uint))

(defgeneric statusbar-remove (statusbar context-id &optional message-id) 
  (:method ((statusbar statusbar) context-id &optional message-id)
    (if message-id
        (gtk-statusbar-remove statusbar context-id message-id)
        (gtk-statusbar-remove-all statusbar context-id))))
