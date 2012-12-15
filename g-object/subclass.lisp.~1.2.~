;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gtype.lisp --- GType functions
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-object-cffi)

(defcstruct g-type-info 
    "GTypeInfo"
  (class-size :uint16)
  (base-init :pointer) ; GBaseInitFunc
  (base-finalize :pointer) ; GBaseFinalizeFunc
  (class-init :pointer) ; GClassInitFunc
  (class-finalize :pointer) ; GClassFinalizeFunc
  (class-data :pointer)  ; == null
  (instance-size :uint16)
  (n-preallocs :uint16)
  (instance-init :pointer) ; GInstanceInitFunc
  (value-table :pointer)) ; GTypeValueTable* == null

(defcenum g-type-flags 
    "GTypeFlags"
  (:abstract 16)
  :value-abstract)
  

(defcfun g-type-register-static g-type 
  (parent-type g-type) (type-name :string) (info g-type-info) 
  (flags g-type-flags))

(defcfun g-type-register-static-simple g-type
  (parent-type g-type) (type-name :string) (class-size :uint)
  (class-init :pointer) (instance-size :uint) (instance-init :pointer)
  (flags g-type-flags))

(defcstruct g-interface-info
    "GInterfaceInfo"
  (interface-init :pointer) ; GInterfaceInitFunc
  (interface-finalize :pointer) ; GInterfaceFinalizeFunc
  (interface-data :pointer))

(defcfun g-type-add-interface-static :void
  (instance-type g-type) (interface-type g-type) (info g-interface-info))


  