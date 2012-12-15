;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gtk-cffi.lisp --- Common functions of gtk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defcfun ("gtk_init" %gtk-init) :void (argc :pointer) (argv :pointer))

(defun gtk-init ()
  ;(load-gtk)
  #+sbcl (sb-ext::set-floating-point-modes :traps nil)
  (with-foreign-objects ((argc :int) (argv :pointer))
    (setf (mem-ref argc :int) 0
          (mem-ref argv :pointer) (foreign-alloc :string 
                                                 :initial-element "program"))
    (%gtk-init argc argv)))

(defcfun "gtk_main" :void)

(defcfun "gtk_main_quit" :void)

(defun defmodel (body)
"
Source:
 `(window :height 100
         :width 100
         :title ,(get-title)
         :signals ,(list ...)
         (:h-box
          (:label :id :label1)
          (:button :id :button1)))

Dest:          

 (make-instance 'window 
               :height 100
               :width 100
               :title (get-title)
               :signals (list ...)
         :kid (make-instance 'h-box
                             :kids
                             (list
                              (make-instance 'label :id :label1)
                              (make-instance 'button :id :button1))))
"
  (labels
      ((rest-translate (l)
         "(:height 100 (:label) (:h-box)) -> (:height 100 :kids (list ....))"
         (when l
           (if (listp (car l))
               (if (cdr l)
                   `(:kids (list ,@(mapcar #'translate l)))
                   `(:kid ,(translate (car l))))
               (cons (first l)
                     (cons (second l)
                           (rest-translate (cddr l)))))))
       (translate (l)
         "(:widget ...) -> (make-instance 'widget ...)"
         (if (keywordp (first l))
             (apply #'make-instance
                    (intern (symbol-name (first l)))
                    (rest-translate (rest l)))
             (translate (eval l)))))
    (translate body)))


(defmacro gtk-model (&body body)
"Structure of BODY is ('widget :param1 val1 :param2 val2 ... :paramn valn ('subwidget1 ...) ('subwidget2 ...))"
  (macrolet
      ((pushkids (x &optional always)
                 (if always
                     `(prog1 nil (push ,x kids))
                   `(if kids (pushkids ,x t) (list ,x)))))
    (labels
        ((process
          (node)
          (let* ((kids)
                 (head (mapcan
                        (lambda (x)
                          (cond
                           ;; ... atom ...  = param or value
                           ((not (consp x)) (pushkids x))
                           ;; ... ((...) ...) ... is a subwidget
                           ((consp (car x)) (pushkids (process x) t))
                           ;; (quote atom) is a widget
                           ((and
                             (eq (car x) 'quote)
                             (atom (second x))) (list 'make-instance x))
                           (t (pushkids x))))
                        node)))
            (append head
                    (when kids
                      (if (cdr kids)
                          (list :kids (cons 'list (nreverse kids)))
                          (list :kid (car kids))))))))
    (process body))))


