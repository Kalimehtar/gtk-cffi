(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage :calculator
  (:use #:common-lisp #:gtk-cffi #:gdk-cffi)
  (:export #:run))

(in-package :calculator)

(defparameter *display* "0")

(defun find-number (str)
  (cond
   ((eql (length str) 0) "")
   ((string= str "0") "")
   ((digit-char-p (elt str 0)) (princ-to-string (read-from-string str)))
   (t (find-number (subseq str 1)))))
    

(defun do-all (oper)
;;   (let ((op (operations calculator)))
;;     (setf (operations calculator) "test2 ok")
;;     op))
  (setf *display*
        (cond
          ((numberp oper) (format nil "~a~a"
                                  (find-number *display*)
                                  oper))
          (t "0")))
  (update-display))
  
(defun build-buttons (table)
  (mapcar
   (lambda (row)
     (mapcar 
      (lambda (elem)
        (when elem
          (make-instance 'button
           :label (princ-to-string elem)
           :signals 
           (list :clicked
                 (lambda (&rest rest)
                   (declare (ignore rest))
                   (do-all elem))))))
      row)) table))

(defun to-right (str &optional (len 20))
  (if (>= (length str) len)
      str
    (concatenate 'string
                 (make-string
                  (- len (length str)) :initial-element #\ ) str)))

(gtk-init)

(defparameter *calculator*  
  (gtk-model
   'window :title "GTK test"
   :signals (list :key-press-event 
                  (lambda (widget event)
;                    (declare (ignore widget ))
                    (do-all (parse-key  (get-slot event :keyval)))
                    (format t "~a ~a~%" widget event))
                  :destroy :gtk-main-quit)
   ('table
    :kids (append
           (list (list 4 (make-instance 'label :text "Калькулятор")))
           (list (list 4 (make-instance 'label :id :display)))
           (build-buttons
            '((7 8 9 +)
              (4 5 6 -)
              (1 2 3 *)
              (0 nil = /)))))))

(defun to-display (str)
  (with-markup
      (:background :cyan)
    (to-right str)))

(defun update-display ()
  (setf (text (object-by-id :display) :markup t) (to-display *display*)))

(defun parse-key (key)
  (cond
   ((<= 65456 key 65465) (- key 65456)) ; numpad number
   ((<= 48 key 57) (- key 48)) ; top row number
   (t key)))


(show *calculator*)
(gtk-main)


  
