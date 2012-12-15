;;; Converted from http://dmitry-vk.livejournal.com/

(asdf:oos 'asdf:load-op :gtk-cffi)
(asdf:oos 'asdf:load-op :closer-mop)

(defpackage #:test
  (:use #:common-lisp #:gtk-cffi #:g-object-cffi))
(in-package #:test)

(defun main ()
  (gtk-init)
  (let* ((slots-model (make-instance 'list-store :columns '(:string)))
         (window
          (gtk-model
           'window :width 400 :height 600
           :title "Class browser"
           :win-position :center
           :signals '(:destroy :gtk-main-quit) 
           ('v-box
            :expand nil
            ('h-box
             ('entry :id :search) 
             ('button :label "Search" 
               :signals 
               (list :clicked 
                 (labels ((display-class-slots (class)
                            (format t "Displaying ~A~%" class)
                            (clear slots-model)
                            (closer-mop:finalize-inheritance class)
                            (loop
                               for slot in (closer-mop:class-slots class)
                               do (append-values slots-model 
                                    (list 
                                     (format nil "~S" 
                                             (closer-mop:slot-definition-name 
                                              slot)))))))
                   (lambda (button) 
                     (declare (ignore button))
                     (let* ((class-name 
                             (read-from-string 
                              (text (object-by-id :search))))
                            (class (find-class class-name)))
                       (display-class-slots class)))))))
            :expand t
            ('scrolled-window
             ('tree-view :model slots-model
                         :columns '("Slot name")))))))
    (show window))
  (gtk-main))

(main)