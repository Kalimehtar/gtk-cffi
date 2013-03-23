(in-package #:gi-cffi)

(defclass constant-info (base-info)
  ())

(deffuns constant-info
  (get-type (object type-info)))

(defmethod free-ptr ((type (eql 'constant-info)) ptr)
  (g-base-info-unref ptr))

(defcfun g-constant-info-get-value :int (info pobject) (arg (argument :out t)))

(defmethod value ((constant-info constant-info))
  (let ((arg (make-arg (get-type constant-info))))
    (g-constant-info-get-value constant-info arg)
    (arg-value arg)))

(defmethod print-object ((constant-info constant-info) stream)
  (print-unreadable-object (constant-info stream)
    (format stream "~a = ~s : ~a" 
            (name constant-info) (value constant-info) 
            (get-type constant-info))))
