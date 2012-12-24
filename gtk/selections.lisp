(in-package :gtk-cffi)

(defclass target-list (object)
  ())

(defcstruct* target-entry
  (target :string)
  (flags :uint)
  (info :uint))
