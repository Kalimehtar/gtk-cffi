(in-package #:gi-cffi)

(defclass arg-info (base-info)
  ())

(defcenum transfer :nothing :container :everything)

(defcenum direction :in :out :inout)

(defcenum scope-type :invalid :call :async :notified)

(deffuns arg-info
  (:get direction direction)
  (is-caller-allocates :boolean)
  (is-return-value :boolean)
  (is-optional :boolean)
  (may-be-null :boolean)
  (:get ownership-transfer transfer)
  (:get scope scope-type)
  (:get closure :int)
  (:get destroy :int)
  (get-type (object type-info)))

(defmethod free-ptr ((type (eql 'arg-info)) ptr)
  (g-base-info-unref ptr))

(defmethod print-object ((arg-info arg-info) stream)
  (print-unreadable-object (arg-info stream)
    (format stream "~a ~a transfer ~a, type ~a" 
            (name arg-info) (direction arg-info) 
            (ownership-transfer arg-info) (get-type arg-info))))

(defun arg->argument (arg &optional value)
  (make-arg (get-type arg) value))
