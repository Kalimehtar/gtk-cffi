;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
(defpackage :emacs
  (:use :cl :gtk-cffi :gdk-cffi :alexandria :iterate)
  (:shadowing-import-from :gtk-cffi #:window #:image)
  (:export #:run-emacs))
