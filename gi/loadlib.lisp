(in-package #:gi-cffi)

(define-foreign-library gi
  (:unix (:or "libgirepository-1.0.so.1" "libgirepository-1.0.so"))
  (:windows "libgirepository-win32-1-0.dll"))
(use-foreign-library gi)