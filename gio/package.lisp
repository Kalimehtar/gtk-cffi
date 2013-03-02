;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for glib-cffi
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cl-user)

(defpackage #:gio-cffi
  (:nicknames #:gio)
  (:use #:common-lisp #:cffi-objects #:g-object-cffi #:g-lib-cffi #:iterate)
  (:export
   #:list-actions 
   #:action-parameter-type 
   #:action-state
   #:has-action 
   #:application-id
   #:is-registered 
   #:change-action-state
   #:activate-action 
   #:release
   #:action-group 
   #:hold 
   #:is-remote
   #:action-state-hint 
   #:action-enabled
   #:activate 
   #:action-state-type
   #:inactivity-timeout 
   #:flags))

(in-package #:gio-cffi)
(register-package "G" *package*)
(register-prefix *package* 'g)