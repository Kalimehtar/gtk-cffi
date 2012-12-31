;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; g-object.lisp --- G-Object wrappers for Common Lisp
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-object-cffi)

(defclass g-object (object)
  ((signals :accessor gsignals :initform nil)
   ;; redefining VOLATILE for saving in hash
   (volatile :initform nil)
   (free-after :initform nil)
   (%properties :accessor %properties :initform nil :allocation :class))
  (:documentation "Lisp wrapper for GObject"))

(defcstruct g-object
  (g-type-instance :pointer) ;; (:struct g-type-instance)))
  (ref-count :uint)
  (g-data :pointer))

(defcallback destroy-object :void ((data :pointer) (g-object pobject))
  "removes pointer from *objects*, when GObject destroyed"
  (declare (ignore data))
  (debug-out "destroying object ~a~%" g-object)
  (free g-object)) 

(defcfun "g_object_weak_ref" :void
  (object :pointer) (notify pfunction) (data :pointer))

(defmethod (setf pointer) :after (value (g-object g-object))
  (declare (type foreign-pointer value))
  (unless (null-pointer-p value)
    (debug-out "Creating ~a ~a~%" g-object value)
    (g-object-weak-ref value (callback destroy-object) (null-pointer))))

;; (defcfun "g_object_set_property" :void 
;;   (object pobject) (name :string) (value pobject))

;; (defcfun "g_object_get_property" :void 
;;   (object pobject) (name :string) (value pobject))


(defmacro generate-property-accessors (name object set get type
                                       class find prop-slot)
  `(progn
     (defgeneric ,type (,object key))
     (defmethod ,type ((,object ,object) key)
       "Should return GType of property KEY."
       (let ((skey (string-downcase key)))
         (or (cdr (assoc skey (,prop-slot ,object) :test #'string=))
             (let* ((gclass (make-instance ',class :object ,object))
                    (prop (,find gclass skey)))
               (when prop
                 (let ((g-type (g-type prop)))
                   (setf (,prop-slot ,object)
                         (acons key g-type (,prop-slot ,object)))
                   g-type)))
             (error "Incorrect property name ~a" key))))

     ,@(when set
        `((defcfun ,set :void 
            (object pobject) (name cffi-keyword) (value pobject))
          (defgeneric (setf ,name) (values ,object &rest keys)
            (:method (values (,object ,object) &rest keys)
              "Usage: 
          (setf (property object :property) value)
          (setf (property object :prop1 :prop2) (list value1 value2))"
              (mapc (lambda (key value)
                      (declare (type (or symbol string) key))
                      (let ((skey (string-downcase key)))
                        (with-g-value (:value value 
                                       :g-type (,type ,object skey))
                          (,set ,object skey *g-value*))))
                    keys (if (listp values) values (list values)))))))

     (defcfun ,get :void 
       (object pobject) (name cffi-keyword) (value pobject))
     (defgeneric ,name (,object &rest keys))
     (defmethod ,name ((,object ,object) &rest keys)
       "Usage 
         (property object :prop1) -> value1
         (property object :prop1 :prop2 ...) -> (value1 value2 ...)"
       (funcall (lambda (x) (if (cdr x) x (car x)))
                (mapcar (lambda (key)
                          (with-g-value
                              (:g-type (,type ,object key))
                            (,get ,object key *g-value*)))
                        keys)))))

(generate-property-accessors property g-object 
                             g-object-set-property g-object-get-property
                             property-type 
                             g-object-class find-property %properties)


(defbitfield connect-flags
  (:none 0)
  :after
  :swapped)


(defcfun "g_signal_connect_data" :ulong
  (instance pobject)
  (detailed-signal :string)
  (c-handler pfunction)
  (data pdata)
  (destroy-data pfunction)
  (connect-flags connect-flags))



;; Closure staff: marshaller and callbacks

(defcallback free-closure :void ((data :pointer) (closure :pointer))
  (declare (ignore data))
  (when (not (null-pointer-p closure))
    (remhash (pointer-address closure) *objects*)))

(defcfun "g_closure_add_finalize_notifier" :void
  (closure :pointer) (data :pointer) (func pfunction))

(defcfun "g_closure_new_simple" :pointer (sizeof :int) (data :pointer))

(defcfun "g_closure_set_marshal" :void (closure :pointer) (marshal :pointer))

(defcfun "g_signal_connect_closure" :ulong
  (instance pobject)
  (detailed-signal :string)
  (closure :pointer)
  (after :boolean))

(defcallback marshal :void ((closure :pointer)
                            (return :pointer)
                            (n-values :int)
                            (params :pointer)
                            (hint :pointer)
                            (data :pointer))
  (declare (ignore hint data))
  (let ((lisp-func (find-object closure))
        (lisp-params 
         (iter
           (for i from 0 below n-values)
           (collect (value
                     (make-instance
                      'g-value
                      :pointer (mem-aref
                                params 'g-value-struct i)))))) ; will be :struct
        (lisp-return (make-instance 'g-value :pointer return)))
    (let ((res (apply lisp-func lisp-params)))
      (when (/= (g-type lisp-return) 0)
        (setf (value lisp-return) res)))))

(defun make-closure (func)
  (let ((closure-ptr (g-closure-new-simple
                      16 (null-pointer)))) ;; sizeof(GClosure) = 16
    (setf (gethash (pointer-address closure-ptr) *objects*) func)
    (g-closure-set-marshal closure-ptr (callback marshal))
    (g-closure-add-finalize-notifier closure-ptr
                                     (null-pointer) (callback free-closure))
    closure-ptr))


(defcfun g-signal-handler-disconnect :void 
  (instance pobject) (id :ulong))

(defmethod connect ((g-object g-object) c-handler 
                    &key signal data after swapped)
  (let* ((str-signal (string-downcase signal))
         (c-handler (if (and (symbolp c-handler) (fboundp c-handler))
                        (symbol-function c-handler) c-handler))
         (handler-id
          (typecase c-handler
            (function (g-signal-connect-closure 
                       g-object str-signal
                       (make-closure
                        (if data
                            (lambda (&rest params)
                              (apply c-handler 
                                     (if swapped 
                                         (cons data params)
                                         (nconc params (list data)))))
                            c-handler))
                       after))
            (t (g-signal-connect-data
                g-object str-signal c-handler data
                (if (or (null data) (pointerp data) (typep data 'g-object))
                    (null-pointer) (callback free-storage))
                ;; connect-flags
                (bitmask after swapped))))))
    (push (cons str-signal handler-id) (gsignals g-object))))

(defgeneric (setf gsignal) (c-handler g-object detailed-signal &rest flags))

(defmethod (setf gsignal) (c-handler
                           (g-object g-object)
                           detailed-signal &key
                           data after swapped)
  "(SETF GSIGNAL) sets signal handler
c-handler may be lisp function (closure)
                    OR c-pointer on function
                    OR string or keyword = name of external c-function
detailed-signal may be string or keyword
data may be c-pointer or lisp object
after & swapped as in GTK: after = handler called after other handlers
            swapped = data and widget will be swapped in c-handler
                      (in a lisp func data will be first arg if swapped, 
                       and last otherwise)

If c-handler is null (or null pointer), this method removes signal.
In this case detailed-string may be also id of the signal handler 
being removed

Returns assoc: (id-of-handler . detailed-signal)"
  (if (or (null c-handler)
          (and (pointerp c-handler) (null-pointer-p c-handler)))
      ;; remove handler
      (setf (gsignals g-object)
            (mapcan
             (lambda (x)
               (if (if (numberp detailed-signal) 
                       (= detailed-signal (cdr x))
                       (string= (string-downcase detailed-signal) (car x)))
                   (g-signal-handler-disconnect g-object (cdr x)) 
                   (list x)))
             (gsignals g-object)))
      (connect g-object c-handler
               :signal detailed-signal
               :swapped swapped :after after :data data)))

    

(defgeneric gsignal (g-object signal))

(defmethod gsignal ((g-object g-object) detailed-signal)
  "method GSIGNAL of class G-OBJECT
   returns list of IDs of setted signals"
  (mapcan (lambda (x) (when (string= (string-downcase detailed-signal) (car x))
                        (list (cdr x)))) 
          (gsignals g-object)))

(defmethod (setf signals) (signals (g-object g-object))
  "SIGNALS is a list (signal-id signal-value signal-id signal-value ...)
signal-value is a function or c-function name. Signal-id is a string or keyword
identifying GTK signal."
  (when signals
    (destructuring-bind (signal-id signal-value &rest rest) signals
      (setf (gsignal g-object signal-id) signal-value
            (signals g-object) rest))
    signals))

(defmethod (setf properties) (properties (g-object g-object))
  "PROPERTIES is a list (prop-id prop-value prop-id prop-value ...)"
  (when properties
    (destructuring-bind (prop-id prop-value &rest rest) properties
      (setf (property g-object prop-id) prop-value
            (properties g-object) rest))
    properties))


(defmethod initialize-instance :after ((g-object g-object)
                                       &key signals properties
                                       &allow-other-keys)
  (setf-init g-object signals properties))
           

(defcfun g-object-ref :pointer (obj pobject))
(defcfun g-object-unref :void (obj pobject))

(defgeneric ref (obj)
  (:method ((obj g-object))
    (g-object-ref obj)))

(defgeneric unref (obj)
  (:method ((obj g-object))
    (g-object-unref obj)))

(defcfun g-object-new :pointer (class-type g-type) (null :pointer))

(defun new (id)
  (g-object-new id (null-pointer)))

(defcfun g-object-newv :pointer (class-type g-type)
                                (n-params :uint) (params :pointer))


         