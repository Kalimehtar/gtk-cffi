;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; simple-action-group.lisp --- GSimpleActionGroup
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gio-cffi)

(defclass simple-action-group (g-object action-group) ())

