;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; application.lisp --- GApplication
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gio-cffi)

(defclass application (g-object action-group) ())

(defbitfield application-flags
  :none :is-service :is-launcher :handles-open :handles-command-line
  :send-environment :non-unique)

(defcfun g-application-new :pointer
  (application-id :string) (flags application-flags))

(defslots application
  application-id :string
  inactivity-timeout :uint
  flags application-flags)

(deffuns application
  (:set action-group pobject)
  (:get is-registered :boolean)
  (:get is-remote :boolean)
  (hold :void)
  (release :void)
  (activate :void))

(defcfun g-application-run :void (application pobject) 
         (argc :int) (argv :pointer))

(defgeneric run (application &key params &allow-other-keys)
  (:method ((application application) &key args)
    (if args
        (with-foreign-object (ptr :string (length args))
          (iter
            (for i from 0)
            (for arg in args)
            (setf (mem-aref ptr :string i) arg))
          (g-application-run application (length args) ptr))
        (g-application-run application 0 (null-pointer)))))

;  (register :boolean 
