;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gtype.lisp --- GType functions
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-object-cffi)

(defparameter +fundamental-g-types+
  '(:invalid :void :interface :char :uchar :boolean
    :int :uint :long :ulong :int64 :uint64
    :enum :flags :float :double :string
    :pointer :boxed :param :object))

(defun keyword->g-type (type)
  "Keyword from +fundamental-gtypes+ -> integer"
  (* (or (position type +fundamental-g-types+)
         (position :object +fundamental-g-types+)) 4))

(defctype g-type :ulong "GType")

(defcstruct* g-type-interface
  "GTypeInterface"
  (g-type-type g-type)
  (g-instance-type g-type))

(defcstruct* g-type-class
  "GTypeClass"
  (g-type-type g-type))

(defcstruct* g-type-instance
  "GTypeInstance"
  (g-class (struct g-type-interface)))


(defun g-type-from-instance (ptr)
  (g-type-type (g-class (make-instance 'g-type-instance :pointer ptr 
                                       :free-after nil))))

(defcfun g-type-fundamental g-type (id g-type))
(defcfun g-type-from-name g-type (name :string))
(defcfun g-type-name :string (id :ulong))

(defcstruct* g-type-query
  "GTypeQuery"
  (g-type-type g-type)
  (name :string)
  (class-size :uint)
  (instance-size :uint))

(defcfun g-type-query :void (type g-type) 
         (query (struct g-type-query)))

(defun g-type->keyword (num)
  "Integer (GType) -> keyword from +fundamental-gtypes+"
  (or (nth (/ (g-type-fundamental num) 4) +fundamental-g-types+) :object))

(defvar *types* (make-hash-table)
  "Hash table: GType num -> lisp object")

(defvar *typenames* nil
  "Assoc: GTK type name (string) -> lisp object")

(defun register-type (lisp-class gtk-typename)
  (push (cons gtk-typename lisp-class) *typenames*))

(defvar *gtk-packages* nil
  ;; (mapcar
  ;;  (lambda (x) (cons (car x) (find-package (cdr x))))
  ;;  (("Gtk" . :gtk-cffi)
  ;;   ("Gdk" . :gdk-cffi)
  ;;   ("Pango" . :pango-cffi)))
  "Assoc: gtk-prefix -> lisp package")

(defun register-package (name package)
  (check-type name string)
  (check-type package package)
  (push (cons name package) *gtk-packages*))


(defun g-type->lisp (g-type)
  "Returns lisp class for the gtype and caches result
Ex.: GType of GtkWindow -> 'gtk-cffi:window"
  (declare (type integer g-type))
  (labels ((case-to-lisp (str)
             "Ex.: CaseLikeThis -> CASE-LIKE-THIS"
             (declare (type string str))
             (with-output-to-string (s)
               (let ((*standard-output* s))
                 (loop 
                    :for c :across str
                    :for i :from 0
                    :do (if (and (upper-case-p c)
                                 (> i 0))
                            (mapc #'princ (list "-" c))
                            (princ (char-upcase c))))))))
    (with-hash *types* g-type
      (let ((typename (g-type-name g-type)))
        (when typename
          (or (cdr (assoc typename *typenames* :test 'string=))
              (let* ((pr-pos 
                      (loop 
                         :for c :across (subseq typename 1)
                         :for i :from 1
                         :when (upper-case-p c) :return i))
                     (prefix (subseq typename 0 pr-pos))
                     (package
                      (cdr (assoc prefix *gtk-packages*
                                  :test 'string=))))
                (when package
                    (intern (case-to-lisp
                             (subseq typename pr-pos))
                            package)))))))))

          
(defcfun g-type-children (garray g-type) (type g-type) (n-children :pointer))

(defun children (type)
  (g-type-children type *array-length*))
