(in-package #:gi-cffi)

(defclass object-info (registered-type-info)
  ())

(deffuns object-info
  (:get type-name :string)
  (:get type-init :string)
  (:get abstract :boolean)
  (:get fundamental :boolean)
  (:get parent (object object-info))
  (:get n-interfaces :int)
  (get-interface (object interface-info) (n :int))
  (:get n-fields :int)
  (:get field (object field-info) (n :int))
  (:get n-methods :int)
  (get-method (object function-info) (n :int))
  ((object-find-method . find-method) (object function-info) (name :string)))
        

(list-builder interfaces n-interfaces get-interface)


(defmethod free-ptr ((type (eql 'object-info)) ptr)
  (g-base-info-unref ptr))