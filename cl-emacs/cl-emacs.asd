;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gtk-cffi.asd --- ASDF system definition for gtk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:emacs-system
  (:use #:cl #:asdf))
(in-package #:emacs-system)

(defsystem cl-emacs
  :description "Emacs-like editr on GTK3"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi alexandria iterate split-sequence flexi-streams)
  :components
  ((:file package)
   (:file keymap :depends-on (package))
   (:file main :depends-on (keymap))))