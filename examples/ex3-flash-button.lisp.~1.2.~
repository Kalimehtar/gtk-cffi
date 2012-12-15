(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage #:test-ex3
  (:use #:common-lisp #:gtk-cffi #:g-lib-cffi))
(in-package #:test-ex3)

(gtk-init)

(defvar window)
(defvar title)
(defvar button)
(defvar v-box)
(defvar h-box)

(setf window (make-instance 'window))

(setf (gsignal window :destroy) :gtk-main-quit
      (size-request window) '(400 150))

(add window (setf v-box (make-instance 'v-box)))

(setf title (make-instance 'label :text "Create a Flashing Button - Part 1"))


(setf (font title) "Times New Roman Italic 10"
      (color title) "#0000ff"
      (size-request title) '(-1 60))

(pack v-box title)
(pack v-box (setf h-box (make-instance 'h-box)))

(setf button (make-instance 'button :label "Click Me!"))
(setf (size-request button) '(80 32)
      (color button :background t) "#FFCC66")

(defvar *TIMEOUT*)

(pack h-box button :expand t)
(setf (gsignal button :clicked)
      (lambda (widget)
        (declare (ignore widget))
        (format t "You have clicked Click Me!~%")
        (format t "~a~%" *TIMEOUT*)
        (timeout-remove *TIMEOUT*)
        (format t "We here ~%")))

(realize window)

(defparameter *ORG-BG* (color window :background t))

(let (i)
  (defun flash (button bgcolor)
    (setf (color button :background t) (if i *ORG-BG* bgcolor))
    (setf i (not i)) t))

(setf *TIMEOUT* (timeout-add 200 #'flash :data (list button "#FFCC66")))

(format t "Set ~a~%" *TIMEOUT*)

(show window :all t)
(gtk-main)

      