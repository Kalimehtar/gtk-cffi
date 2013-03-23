(in-package #:gi-cffi)

(defclass function-info (callable-info)
  ())

(defmethod free-ptr ((type (eql 'function-info)) ptr)
  (g-base-info-unref ptr))

(defbitfield function-info-flags 
  :method :constructor :getter :setter :wraps-vfunc :throws)

(deffuns function-info
  (get-symbol :string)
  (:get flags function-info-flags)
  (:get property (object property-info))
  (:get vfunc (object vfunc-info)))

(defmethod print-object ((function-info function-info) stream)
  (print-unreadable-object (function-info stream)
    (format stream "~a(~{~a~^,~})" (name function-info) 
            (mapcar #'name (args function-info)))))
      

(defcfun g-function-info-invoke :boolean 
  (func-info pobject) 
  (in-args arguments) (n-in-args :int)
  (out-args (arguments :out t)) (n-out-args :int)
  (return-value (argument :out t)) (g-error pobject))

(defgeneric invoke (func-info &rest args)
  (:method ((func-info function-info) &rest args)
    (let (in-args out-args (return-value (make-arg (return-type func-info))))
      (dotimes (n-arg (n-args func-info))
        (let ((arg (arg func-info n-arg)))
          (when (member (direction arg) '(:in :inout))
            (push (arg->argument arg (nth n-arg args)) in-args))
          (when (member (direction arg) '(:out :inout))
            (push (arg->argument arg) out-args))))
      (setf in-args (nreverse in-args))
      (setf out-args (nreverse out-args))
      (with-g-error g-error 
        (let ((res (g-function-info-invoke func-info 
                                           in-args (length in-args)
                                           out-args (length out-args)
                                           return-value g-error)))
          (unless res
            (throw-g-error g-error))
          (values-list (cons (arg-value return-value)
                             (mapcar #'arg-value out-args))))))))
                                           
      
      
  
  