(in-package :gtk-cffi)

(defclass target-list (object)
  ())

(defcstruct* target-entry
  (target :string)
  (flags :uint)
  (info :uint))

(defcfun gtk-target-entry-new :pointer
  (target :string) (flags :uint) (info :uint))

(defmethod gconstructor ((target-entry target-entry)
                         &key new-struct target flags info
                         &allow-other-keys)
  (if new-struct
      (gtk-target-entry-new target flags info)
      (call-next-method)))

(defcfun gtk-target-entry-free :void (ptr :pointer))

(defmethod free-struct ((class (eql 'target-entry)) ptr)
  (gtk-target-entry-free ptr))
  
