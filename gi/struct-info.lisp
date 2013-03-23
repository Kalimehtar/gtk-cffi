(in-package #:gi-cffi)

(defclass struct-info (registered-type-info)
  ())

(defctype gsize g-type)

(deffuns struct-info
  (:get n-fields :int)
  (:get field (object field-info) (n :int))
  (:get n-methods :int)
  (get-method (object function-info) (n :int))
  ((struct-find-method . find-method) (object function-info) (name :string))
  (:get size gsize)
  (:get alignment gsize)
  (is-gtype-struct :boolean)
  (is-foreign :boolean))

(defmethod free-ptr ((type (eql 'struct-info)) ptr)
  (g-base-info-unref ptr))

(list-builder fields n-fields field)
(list-builder methods n-methods get-method)

