(in-package #:gi-cffi)

(defclass field-info (base-info)
  ())

(defbitfield field-info-flags :readable :writable)

(deffuns field-info
  (:get flags field-info-flags)
  (:get size :int)
  (:get offset :int)
  (get-type (object type-info)))

(defmethod print-object ((field-info field-info) stream)
  (print-unreadable-object (field-info stream)
    (format stream "~a : ~a" 
            (name field-info)
            (get-type field-info))))

(defmethod free-ptr ((type (eql 'field-info)) ptr)
  (g-base-info-unref ptr))