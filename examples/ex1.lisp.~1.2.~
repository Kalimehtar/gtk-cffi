(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage #:test-ex1
  (:use #:common-lisp #:gtk-cffi #:g-object-cffi #:cffi))
(in-package #:test-ex1)

(cffi:defcallback hello :void ((widget pobject) (data pdata))
  (format t "Превед ~a from ~a~%" data widget))

(gtk-init)

(defvar window)
(defvar button)

(setf window (make-instance 'window :name "Example 1"))

;(setf (bg-pixmap window) "/usr/share/pixmaps/gqview.png")

;(setf (property window :resize-mode) :immediate)

(setf (gsignal window "delete-event")
      (let ((i 0))
        (lambda (widget event)
          (format t "delete event ~a~%" (incf i))
          (format t "~& widget = ~a ~a~%" widget (name widget))
          (format t "~& event = ~a~%" event)
          (format t "~& child = ~a~%" (child widget))
          ;(gobject-cffi::show-properties widget)
          t)))

(setf (gsignal window :destroy) :gtk-main-quit)


(setf (border-width window) 25)

(setf (default-size window) '(400 100))

;(setf button (make-instance 'button :label "gtk-ok" :type :stock))

(setf button (make-instance 'button :pointer (gtk-cffi::gtk-button-new-from-stock  "gtk-ok")))

;(setf (color button :type :bg) "red")

(setf (color button) "#0000ff")
(setf (font button) "Times New Roman Italic 24")

(setf (gsignal button :clicked :data "Медвед") (cffi:callback hello)
      (gsignal button "clicked" :data window :swapped t) "gtk-widget-destroy")


(add window button)

(show button)

(show window)

(gtk-main)

