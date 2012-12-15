 ;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; pobject.lisp --- CFFI type for class OBJECT with GType guessing
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-object-cffi)

(define-foreign-type cffi-pobject (cffi-object)
  ()
  (:actual-type :pointer))

(define-parse-method pobject (&optional class)
  (make-instance 'cffi-pobject :class class))

(defmethod translate-from-foreign (ptr (cffi-pobject cffi-pobject))
  "The first int at GObject instance is its type pointer, take it and
make up lisp object"
  (declare (type foreign-pointer ptr))
  (unless (null-pointer-p ptr)
;    (format t "pobject: ~a~%" ptr)
    (let ((class (or (object-class cffi-pobject) 
                     (g-type->lisp (g-type-from-instance ptr)))))
 ;     (format t "gtype: ~a :: ~a~%" (g-type-from-instance ptr) class)
      
      (find-object ptr class))))

;; register as object type for g-list
(defmethod g-lib-cffi::object-type ((type-name (eql 'pobject))) t)

;;; Class STORAGE

(defclass storage (object)
  ((data :accessor data :initarg :data)
   (volatile :initform nil :accessor volatile))
  (:documentation "A storage for any data for callbacks.
  On make-instance it allocates one byte on heap and associates itself
  with the address of that byte."))

;; register as object type for g-list
(defmethod g-lib-cffi::object-type ((type-name (eql 'pdata))) t)

(defmethod gconstructor ((storage storage) &key &allow-other-keys)
  (foreign-alloc :char))

(defcallback free-storage :void ((data :pointer) (closure :pointer))
  (declare (ignore closure))
  (unless (null-pointer-p data)
    (setf (pointer (find-object data)) (null-pointer))
    (remhash (pointer-address data) *objects*)
    (foreign-free data)))


(define-foreign-type cffi-pdata (cffi-pobject freeable-base)
  ((free-to-foreign :initform nil))
  (:actual-type :pointer)
  (:simple-parser pdata)
  (:documentation "PDATA lets send any data via a c-pointer. C-pointer used as
an id for the data. NB! Don't forget to free pointers after use."))

(defmethod free-ptr ((type (eql 'cffi-pdata)) object)
                                        ; it's not typo: 
                                        ;we free object, not pointer
  (free object))

(defmethod translate-from-foreign (ptr (type cffi-pdata))
  "Returns saved data."
  (let ((obj (find-object ptr)))
    (if obj 
        (typecase obj
          (storage (prog1 (data obj) (free-returned-if-needed type obj)))
          (t obj))
        ptr)))

(defmethod translate-to-foreign ((any-data object) (type cffi-pdata))
  (pointer any-data))

(defmethod translate-to-foreign ((any-data null) (type cffi-pdata))
  (null-pointer))

(defmethod translate-to-foreign (any-data (type cffi-pdata))
  (if (pointerp any-data)
      any-data
      (let ((obj (make-instance 'storage :data any-data)))
        (values (pointer obj) obj))))

(defmethod free-translated-object (ptr (type cffi-pdata) param)
  (when param
    (free-sent-if-needed type param param)))

(defctype g-list-object (g-list :elt pobject))


(defcfun g-type-interface-peek-parent pobject (iface pobject))

(defcfun g-type-class-peek-parent pobject (class pobject))
