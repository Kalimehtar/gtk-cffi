;;;
;;; builder.lisp -- GtkBuilder
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass builder (g-object) ())

(defcfun gtk-builder-new :pointer)

(defmethod gconstructor ((builder builder) &key &allow-other-keys)
  (gtk-builder-new))

(defcfun gtk-builder-add-from-file :uint 
  (builder pobject) (filename :string) (g-error (:pointer (:struct g-error))))

(defcfun gtk-builder-add-from-string :uint 
  (builder pobject) (string :string) (length gsize) 
  (g-error (:pointer (:struct g-error))))

(defcfun gtk-builder-add-objects-from-file :uint 
  (builder pobject) (filename :string) (object-ids string-list) 
  (g-error (:pointer (:struct g-error))))

(defcfun gtk-builder-add-objects-from-string :uint 
  (builder pobject) (string :string) (length gsize) (object-ids string-list) 
  (g-error (:pointer (:struct g-error))))

(defgeneric add-from (builder &key filename string objects)
  (:method 
   ((builder builder) &key filename string objects)
   (with-g-error g-error
     (when 
         (= 0 
            (if filename
                (if objects
                    (gtk-builder-add-objects-from-file builder filename 
                                                       objects g-error)
                    (gtk-builder-add-from-file builder filename g-error))
                (if objects
                    (gtk-builder-add-objects-from-string 
                     builder string (length string) objects g-error)
                    (gtk-builder-add-from-string 
                     builder string (length string) g-error))))
       (throw-g-error g-error)))))

(defcfun gtk-builder-connect-signals-full :void
  (builder pobject) (func pfunction) (user-data :pointer))

(defcallback cb-find-defun :void ((builder :pointer) (object pobject)
                                  (signal-name :string) (handler :string)
                                  (connect-object pobject) (flags connect-flags)
                                  (user-data :pointer))
  (declare (ignore builder user-data connect-object))
  (connect object (eval (read-from-string handler))
           :signal signal-name 
           :after (not (null (find :after flags)))
           :swapped (not (null (find :swapped flags)))))

(defgeneric connect-signals (builder &key func) 
  (:method ((builder builder) &key func)
    (gtk-builder-connect-signals-full builder 
                                      (or func (callback cb-find-defun))
                                      (null-pointer))))


(deffuns builder
  (:get object pobject (name :string))
  (:get objects (g-slist :elt pobject))
  (:get type-from-name g-type (type-name :string)))

(defslots builder
  translation-domain :string)

(defcfun gtk-builder-value-from-string :boolean 
  (builder pobject) (pspec pobject) (string :string) (value pobject) 
  (g-error (:pointer (:struct g-error))))

(defcfun gtk-builder-value-from-string-type :boolean 
  (builder pobject) (g-type g-type) (string :string) (value pobject) 
  (g-error (:pointer (:struct g-error))))

(defgeneric value-from-string (builder &key g-type param-spec string)
  (:method ((builder builder) &key g-type param-spec string)
    (let ((value (make-instance 'g-value)))
      (with-g-error g-error
        (unless (if param-spec
                    (gtk-builder-value-from-string builder param-spec string
                                                   value g-error)
                    (gtk-builder-value-from-string-type builder g-type string
                                                        value g-error))
          (throw-g-error g-error)))
      value)))
                   
        



  
        