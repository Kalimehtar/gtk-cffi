(in-package :gtk-cffi)

(defclass text-mark (g-object)
  ())

(defcfun gtk-text-mark-new :pointer (name gtk-string) (left-gravity :boolean))

(defmethod gconstructor ((text-mark text-mark) &key name left-gravity
                         &allow-other-keys)
  (gtk-text-mark-new name left-gravity))

(defgtkslot text-mark visible :boolean)

(defgtkfuns text-mark
  (:get deleted :boolean)
  (:get name gtk-string)
  (:get buffer pobject)
  (:get left-gravity :boolean))

(init-slots text-mark)