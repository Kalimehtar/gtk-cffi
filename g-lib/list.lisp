;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; list.lisp -- interface to GList
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-lib-cffi)


;; I don't see where one can use GList as is. So there is no such class.
;; Only convertors to and from lisp lists

(defcfun g-list-free :void (g-list :pointer))
(defcfun g-list-foreach :void 
  (g-list :pointer) (func :pointer) (data :pointer))
(defcfun g-list-prepend :pointer (g-list :pointer) (data object))
(defcfun g-list-reverse :pointer (g-list :pointer))

(defvar *list*)
(defvar *list-type*)

(defgeneric object-type (type-name)
  (:documentation "Tests is TYPE-NAME is member of object types")
  (:method ((type-name (eql 'object))) t)
  (:method (type-name) nil))
                  

(defcallback list-collect :void ((data :pointer) (user-data :pointer))
  (declare (ignore user-data))
  (push (cond
          ((null *list-type*) data)
          ((object-type (ensure-car *list-type*))
           (convert-from-foreign data *list-type*))
          (t (mem-ref data *list-type*))) *list*))

(define-foreign-type g-list (freeable)
  ((list-type :initarg :elt :accessor list-type :initform nil
              :documentation "If null, then list is of pointers or GObjects")
   (free-from-foreign :initform t))
  (:simple-parser g-list)
  (:actual-type :pointer))

(defmethod free-ptr ((type (eql 'g-list)) ptr)
  (g-list-free ptr))

(defmethod translate-from-foreign (ptr (g-list g-list))
  (declare (type foreign-pointer ptr))
  (let ((*list* nil)
        (*list-type* (list-type g-list)))
    (g-list-foreach ptr (callback list-collect) (null-pointer))
    (nreverse *list*)))

(defmethod translate-to-foreign (lisp-list (g-list g-list))
  (declare (type list lisp-list))
  (let ((converter
         (let ((list-type (list-type g-list)))
           (if (and list-type (not (object-type (ensure-car list-type))))
             (lambda (x) (foreign-alloc list-type :initial-element x))
             #'identity))))
    (let ((p (null-pointer)))
      (mapc (lambda (x)
              (setf p (g-list-prepend p (apply converter x))))
            lisp-list)
      (g-list-reverse p))))


;; Copy-paste fom g-list. Bad, but what to do?
(define-foreign-type g-slist (freeable)
  ((list-type :initarg :elt :accessor list-type 
              :documentation "If null, then list is of pointers or GObjects")
   (free-from-foreign :initform t))
  (:simple-parser g-slist)
  (:actual-type :pointer))

(defcfun g-slist-free :void (g-slist :pointer))
(defcfun g-slist-foreach :void 
  (g-list :pointer) (func :pointer) (data :pointer))
(defcfun g-slist-prepend :pointer (g-slist :pointer) (data object))
(defcfun g-slist-reverse :pointer (g-slist :pointer))


(defmethod free-ptr ((type (eql 'g-slist)) ptr)
  (g-slist-free ptr))

(defmethod translate-from-foreign (ptr (g-slist g-slist))
  (declare (type foreign-pointer ptr))
  (let ((*list* nil)
        (*list-type* (list-type g-slist)))
    (g-slist-foreach ptr (callback list-collect) (null-pointer))
    (nreverse *list*)))

(defmethod translate-to-foreign (lisp-list (g-slist g-slist))
  (declare (type list lisp-list))
  (let ((converter
         (let ((list-type (list-type g-slist)))
           (if (and list-type (not (object-type (ensure-car list-type))))
             (lambda (x) (foreign-alloc list-type :initial-element x))
             #'identity))))
    (let ((p (null-pointer)))
      (mapc (lambda (x)
              (setf p (g-slist-prepend p (apply converter x))))
            lisp-list)
      (g-slist-reverse p))))

(define-foreign-type string-list (freeable)
  ((free-from-foreign :initform t))
  (:actual-type :pointer)
  (:simple-parser string-list))

(defcfun g-strfreev :void (ptr :pointer))

(defmethod free-ptr ((type (eql 'string-list)) ptr)
  (g-strfreev ptr))

(defmethod translate-from-foreign (ptr (type string-list))
  (declare (type foreign-pointer ptr))
  (iter      
    (for i from 0)
    (for pstr = (mem-aref ptr :pointer i))
    (while (not (null-pointer-p pstr)))
    (collect (convert-from-foreign pstr :string))))
