(in-package #:gio-cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library :gio
    (:unix "libgio-2.0.so")
    (:windows "libgio-2.0-0.dll"))

  (load-foreign-library :gio))