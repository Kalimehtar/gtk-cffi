;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; loadlib.lisp --- loading C library
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(eval-when (:compile-toplevel :load-toplevel)
  (unless (find :gtk *features*)
    (push :gtk *features*)
    (define-foreign-library :gtk
      (:unix (:or "libgtk-3.so.0" "libgtk-3.so"))
      (:windows "libgtk-win32-3-0.dll"))
    
    (use-foreign-library :gtk)))

(eval-when (:compile-toplevel)
  (defcfun gtk-get-major-version :uint)
  (defcfun gtk-get-minor-version :uint)
  (when (and (>= (gtk-get-major-version) 3) (>= (gtk-get-minor-version) 2))
    (push :gtk3.2 *features*))
  (when (and (>= (gtk-get-major-version) 3) (>= (gtk-get-minor-version) 4))
    (push :gtk3.4 *features*)))

