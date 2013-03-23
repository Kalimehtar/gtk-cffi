(in-package #:gi-cffi)

(defcenum load-flags (:lazy 1))

(defcfun g-irepository-require :pointer
  (repo :pointer) (namespace :string) (version :string)
  (load-flags load-flags) (g-error pobject))

(defun require (namespace &optional (version (null-pointer)) (load-flags :lazy))
  (g-lib:with-g-error g-error
    (let ((res (g-irepository-require (null-pointer) namespace version
                                      load-flags g-error)))
      (if (null-pointer-p res) 
          (throw-g-error g-error)
          res))))

(defcfun g-irepository-find-by-name :pointer
  (repo :pointer) (namespace :string) (name :string))

(defcfun g-irepository-find-by-gtype :pointer
  (repo :pointer) (gtype g-type))

(defcfun g-irepository-get-n-infos :int
  (repo :pointer) (namespace :string))

(defcfun g-irepository-get-info :pointer
  (repo :pointer) (namespace :string) (index :int))

(defun get-n-infos (namespace)
  (g-irepository-get-n-infos (null-pointer) namespace))

(defcfun g-irepository-get-version :string
  (repo :pointer) (namespace :string))

(defun get-version (namespace)
  (g-irepository-get-version (null-pointer) namespace))

(defun get-info (&key namespace name gtype index)
  (let* ((p
          (cond
            (name (g-irepository-find-by-name (null-pointer) namespace name))
            (gtype (g-irepository-find-by-gtype (null-pointer) gtype))
            (index (g-irepository-get-info (null-pointer) namespace index))
            (t (error 
                "You should fill one of name+namespace, gtype or index"))))
         (base (make-instance 'base-info :pointer p)))
    (case (get-type base)
      ((:function :callback) (make-instance 'function-info :pointer p))
      ((:struct :boxed) (make-instance 'struct-info :pointer p))
      ((:enum :flags) (make-instance 'enum-info :pointer p))
      (:object (make-instance 'object-info :pointer p))
      (:interface (make-instance 'interface-info :pointer p))
      (:constant (make-instance 'constant-info :pointer p))
      (:union (make-instance 'union-info :pointer p))
      (:value (make-instance 'value-info :pointer p))
      (:signal (make-instance 'signal-info :pointer p))
      (:vfunc (make-instance 'vfunc-info :pointer p))
      (:property (make-instance 'property-info :pointer p))
      (:field (make-instance 'field-info :pointer p))
      (:arg (make-instance 'arg-info :pointer p))
      (:type (make-instance 'type-info :pointer p))
      (t base))))
