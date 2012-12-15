(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage #:test-ex11
  (:use #:common-lisp #:gtk-cffi #:g-object-cffi))
(in-package #:test-ex11)

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
   ('v-box 
    ('button :label "Hello!"
            :signals (list :clicked 
                           (let ((count 0)) 
                             (lambda (widget) 
                               (declare (ignore widget))
                               (format t "Pressed ~a times~%" 
                                       (incf count))))))
    ('button :label "About"
             :signals (list :clicked
                            (lambda (widget)
                              (declare (ignore widget))
                              (run (make-instance 'about-dialog
                                                  :authors 
                                                  '("Roman Klochkov")
                                                  :program-name "Test"
                                                  :licence-type :gpl-3-0))))))))
(show *window*)
(gtk-main)