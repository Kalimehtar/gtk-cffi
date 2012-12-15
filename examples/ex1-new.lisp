(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage #:test-ex1n
  (:use #:common-lisp #:gtk-cffi #:g-object-cffi))
(in-package #:test-ex1n)

(gtk-init)
(defparameter *window*
  (gtk-model
   'window :width 80
           :title "Hello world!"
           :signals `(:destroy 
                      :gtk-main-quit
                      :enter-notify-event 
                      ,(lambda (widget event) 
                        (declare (ignore widget event))
                        (format t "Entered~%")))
   ('button :label "Hello!"
            :signals (list :clicked 
                           (let ((count 0)) 
                             (lambda (widget) 
                               (declare (ignore widget))
                               (format t "Pressed ~a times~%" 
                                       (incf count))))))))
(show *window*)
(gtk-main)
                                      
