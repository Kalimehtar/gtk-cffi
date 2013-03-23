(in-package #:cl-user)

(defpackage gi-cffi
  (:use #:common-lisp #:alexandria #:iterate
        #:cffi-objects #:g-lib-cffi #:g-object-cffi
        #:gtk-cffi-utils)
  (:shadow #:require #:property)
  (:export
   #:require
   #:get-n-infos
   #:get-info
   
   #:ref
   #:unref
   #:get-type
   #:container
   #:is-deprecated
   #:namespace
   #:typelib
   #:name
   #:info-equal
   #:attribute
   #:ownership-transfer
   #:destroy
   #:is-optional
   #:closure
   #:get-symbol
   #:scope
   #:may-be-null
   #:flags
   #:is-return-value
   #:is-caller-allocates
   #:direction
   #:property))

(in-package #:gi-cffi)
(g-object-cffi:register-prefix *package* 'g)