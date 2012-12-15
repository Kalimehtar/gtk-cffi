(in-package #:gio-cffi)

(define-foreign-library :gio
  (:unix "libgio-2.0.so")
  (:windows "libgio-2.0-0.dll"))

(use-foreign-library :gio)