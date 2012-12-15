(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage #:test
  (:use #:common-lisp #:gtk-cffi #:g-object-cffi))
(in-package #:test)

(gtk-init)

(let ((window (make-instance 'window)))

  (setf (gsignal window :destroy) :gtk-main-quit
        (size-request window) '(400 150))
  

  (setf (bg-pixmap window) "/usr/share/pixmaps/gnome-color-browser.png")

  (show window))

(gtk-main)