(in-package #:gi-cffi)

(defcenum type-tag
  :void :boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64
  :float :double :gtype :utf8 :filename :array :interface :glist :gslist
  :ghash :error :unichar)

(defcenum array-type
  :c :array :ptr-array :byte-array)

(defclass type-info (base-info)
  ())

(deffuns type-info
  (is-pointer :boolean)
  (:get tag type-tag)
  (:get param-type (object type-info) (n :int))
  (:get interface (object base-info))
  (get-array-length  :int)
  (:get array-fixed-size :int)
  (is-zero-terminated :boolean)
  (:get array-type array-type))

(defmethod free-ptr ((type (eql 'type-info)) ptr)
  (g-base-info-unref ptr))

(defmethod print-object ((type-info type-info) stream)
  (print-unreadable-object (type-info stream)
    (when (is-pointer type-info) (princ "pointer to " stream))
    (let ((tag (tag type-info)))
      (princ tag stream)
      (when (eq tag :interface)
        (format stream " to ~a" (interface type-info)))
      (when (eq tag :array)
        (format stream " of ~a" (param-type type-info 0))
        (format stream ", length: ~a" (get-array-length type-info))
        (format stream ", fixed length: ~a" (array-fixed-size type-info))
        (when (is-zero-terminated type-info)
          (princ ", zero terminated" stream)))
      (when (eq tag :ghash)
        (format stream " of {~a, ~a}" 
                (param-type type-info 0) 
                (param-type type-info 1))))))
      

(defcunion giargument
  (boolean :int)
  (int8 :int8)
  (uint8 :uint8)
  (int16 :int16)
  (uint16 :uint16)
  (int32 :int32)
  (uint32 :uint32)
  (int64 :int64)
  (uint64 :uint64)
  (float :float)
  (double :double)
;  (short :short)
;  (ushort :ushort)
;  (int :int)
;  (uint :uint)
;  (long :long)
;  (ulong :ulong)
;  (ssize :long)
  (size :ulong)
  (string :string)
  (pointer :pointer))




;;; arg in lisp is (type . value)

(defun arg-type (place) (car place))
(defun arg-value (place) (cdr place))
(defun (setf arg-value) (value place) (setf (cdr place) value))
(defun make-arg (type &optional value) 
  (cons type (or value
                 (case (tag type)
                   ((:utf8 :filename) "")
                   (:boolean nil)
                   (t 0)))))
            
              

(define-foreign-type cffi-giargument (freeable-out)
  ()
  (:documentation "GIArgument union <-> (cons type-info-expr value)")
  (:simple-parser argument)
  (:actual-type :pointer))

(defmethod translate-to-foreign (place (arg cffi-giargument))
  (let ((ptr (foreign-alloc 'giargument)))
    (to-foreign (tag (arg-type place)) place ptr)
    (values ptr place)))

(defmethod translate-from-foreign (ptr (arg cffi-giargument))
  (error "GIArgument cannot be returned"))

(defmethod copy-from-foreign ((arg cffi-giargument) ptr place)
  (from-foreign (tag (arg-type place)) place ptr))


(defun giargument-type (slot-name)
  (cffi::slot-type (cffi::get-slot-info 'giargument slot-name)))

(defun gen-to-foreign 
    (tag place ptr &key (conv #'identity)
                        (field (intern (symbol-name tag) #.*package*)))
  (if (and (is-pointer (arg-type place))
           (not (member tag '(:utf8 :interface :filename))))
      (let ((ptr2 (foreign-alloc (giargument-type field) 
                                 :initial-contents 
                                 (funcall conv (arg-value place)))))
        (setf (foreign-slot-value ptr 'giargument 'pointer) ptr2))
      (setf (foreign-slot-value ptr 'giargument field)
            (funcall conv (arg-value place)))))

(defun gen-from-foreign
    (tag place ptr &key (conv #'identity)
                        (field (intern (symbol-name tag) #.*package*)))
  (if (and (is-pointer (arg-type place))
           (not (member tag '(:utf8 :interface :filename))))
      (let ((ptr2 (foreign-slot-value ptr 'giargument 'pointer)))
        (setf (arg-value place) 
              (funcall conv (mem-ref ptr2 (giargument-type field))))
        (foreign-free ptr2))
      (setf (arg-value place)
            (funcall conv (foreign-slot-value ptr 'giargument field)))))

                       

(defun to-foreign (tag place ptr)
  (case tag
    (:boolean (gen-to-foreign tag place ptr 
                              :conv (lambda (x) (if x 1 0))))
    ((:int8 :uint8 :int16 :uint16 
                          :int32 :uint32 :int64 :uint64 :float :double)
     (gen-to-foreign tag place ptr))
    (:gtype (gen-to-foreign tag place ptr :field 'size))
    ((:utf8 :filename) (gen-to-foreign tag place ptr :field 'string))
    (:interface (gen-to-foreign tag place ptr 
                                :field 'pointer
                                :conv (lambda (x) 
                                        (convert-to-foreign x 'pobject))))
;    (:array (gen-to-foreign tag place ptr 
;                            :conv (build-array place)
;                            :field 'pointer))
    (t (error "Not implemented"))))
                              
(defun from-foreign (tag place ptr)
  (case tag
    (:boolean (gen-from-foreign tag place ptr 
                                :conv (lambda (x) (if (= x 0) nil t))))
    ((:int8 :uint8 :int16 :uint16 
                          :int32 :uint32 :int64 :uint64 :float :double)
     (gen-from-foreign tag place ptr))
    (:gtype (gen-from-foreign tag place ptr :field 'size))
    ((:utf8 :filename) (gen-from-foreign tag place ptr :field 'string))
    (:interface (gen-from-foreign tag place ptr 
                                  :field 'pointer
                                  :conv (lambda (x)
                                          (convert-from-foreign x 'pobject))))
;    (:array (gen-to-foreign tag place ptr 
;                            :conv (build-array place)
;                            :field 'pointer))
    (t (error "Not implemented"))))

(define-foreign-type cffi-giarguments (freeable-out)
  ()
  (:documentation "GIArguments array <-> (cons type-info-expr value)")
  (:simple-parser arguments)
  (:actual-type :pointer))

(defmethod translate-to-foreign (value (arguments cffi-giarguments))
  (let* ((res (foreign-alloc 'giargument :count (length value)))
         (p res))          
    (map nil
         (lambda (arg)
           (to-foreign (tag (arg-type arg)) arg p)
           (incf-pointer p (foreign-type-size 'giargument)))
         value)
    res))

(defmethod copy-from-foreign ((arguments cffi-giarguments) ptr place)
  (let ((p ptr))
    (map nil
         (lambda (arg)
           (from-foreign (tag (arg-type arg)) arg p)
           (incf-pointer p (foreign-type-size 'giargument)))
         place)))