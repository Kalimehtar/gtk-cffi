(in-package #:gi-cffi)

(defclass base-info (object)
  ())

(defcenum info-type
  :invalid :function :callback :struct :boxed 
  :enum :flags :object :interface :constant 
  :invalid_0 :union :value :signal :vfunc
  :property :field :arg :type :unresolved)

(deffuns base-info
  (ref :pointer)
  (unref :void)
  (get-type info-type)
  (:get name :string)
  (:get namespace :string)
  (is-deprecated :boolean)
  (:get attribute :string (name :string))
  (:get container (object base-info))
;  (:get typelib (object typelib))  ;; useless?
  ((info-equal . equal) :boolean (info2 pobject)))

(defmethod free-ptr ((type (eql 'base-info)) ptr)
  (g-base-info-unref ptr))

(defmethod print-object ((base-info base-info) stream)
  (print-unreadable-object (base-info stream :type t)
    (princ (name base-info) stream)))
 