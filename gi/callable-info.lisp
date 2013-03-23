(in-package #:gi-cffi)

(defclass callable-info (base-info)
  ())

(deffuns callable-info
  (:get return-type (object type-info))
  (:get caller-owns transfer)
  (may-return-null :boolean)
  (:get return-attribute :string)
  (:get n-args :int)
  (:get arg (object arg-info) (n :int)))

(defmethod free-ptr ((type (eql 'callable-info)) ptr)
  (g-base-info-unref ptr))