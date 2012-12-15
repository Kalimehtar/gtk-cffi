(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage :test-dialog
  (:use #:common-lisp #:gtk-cffi))

(in-package :test-dialog)

(gtk-init)

(defun login ()
  (loop 
     :for count :from 0 :to 2
     :do (destructuring-bind (username password) 
             (get-data "Login" '("Username:" "Password:"))
           (if (and (string= username "user1")
                    (string= password "pass1"))
               (return t)
               (show-message 
                nil 
                (format 
                 nil 
                 (format nil "~@{~A~%~}"
                         "Incorrect username and password!"
                         "Entered: username=~a"
                         "password=~a") username password)))))
  nil)
     

(defun get-data (title field-labels)
  (let ((dialog (make-instance 'dialog :name title :flags :modal)))
    (setf (win-position dialog) :center-always)
    (let ((top-area (v-box dialog))
          (h-box (make-instance 'h-box)))
      (pack top-area h-box)
      (let ((stock (make-instance 'image 
                                  :stock-id "dialog-question"
                                  :icon-size :dialog)))
        (pack h-box stock :fill nil :expand nil)
        (let ((table (make-instance 'table))
              (input nil)
              (row 0))
          (mapc 
           (lambda (field-label)
             (let ((label (make-instance 'label :text field-label)))
               (setf (alignment label) '(0 0))
               (attach table label 
                       :left 0 :right 1
                       :top row :bottom (+ row 1))
               (push (make-instance 'entry) input)
               (attach table (car input)
                       :left 1 :right 2
                       :top row :bottom (+ row 1))
               (when (search "password" field-label :test #'char-equal)
                 (setf (visibility (car input)) nil)))
             (incf row)) field-labels)
          (pack h-box table)
          (add-button dialog :ok :ok)
          (setf (has-separator dialog) nil)
          (show dialog)
          (run dialog)
          (let ((data (mapcar (lambda (x) (text x)) (nreverse input))))
            (destroy dialog)
            data))))))
          
(format t "~a~%" (login))
(g-lib-cffi:timeout-add :idle #'gtk-main-quit)
(gtk-main)
      
        
