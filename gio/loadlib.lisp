(in-package #:gio-cffi)

(define-foreign-library :gio
  (:unix (:or "libgio-2.0.so.0" "libgio-2.0.so"))
  (:windows "libgio-2.0-0.dll"))

(use-foreign-library :gio)