;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; action-group.lisp --- GActionGroup
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gio-cffi)

(defclass action-group (object) ())

(deffuns action-group
  (has-action :boolean (action-name :string))
  (list-actions string-list)
  (:get action-enabled :boolean (action-name :string))
  (:get action-parameter-type variant-type (action-name :string))
  (:get action-state-type variant-type (action-name :string))
  (:get action-state-hint variant (action-name :string))
  (:get action-state variant (action-name :string))
  (change-action-state :void (action-name :string) (value variant))
  (activate-action :void (action-name :string) (parameter variant)))
  

  
  