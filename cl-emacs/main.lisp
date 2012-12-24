;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: emacs; -*-
(in-package :emacs)
(declaim (optimize safety debug))

(defvar *file*)
(defvar *region*)
(defvar *encoding*)


(defun on-key-press (widget event &rest rest)
  (declare (ignore widget rest))
  
  (let ((ret (when *entered-sequence* t)))
    (setf *entered-sequence* 
          (nconc *entered-sequence*
                 (list (list (parse-event event :hardware-keycode)
                             (sort (intersection (parse-event event :state)
                                                 '(:shift :control :mod1)) 
                                   #'symbol-name<)))))
    
    (if (seq-processed *entered-sequence*)
        (setf *entered-sequence* nil)
        (setf ret t))
    
    (let ((statusbar (object-by-id :status)))
      (when statusbar
        (let ((context-id (context-id statusbar :key-seq)))
          (if *entered-sequence*
              (statusbar-push statusbar context-id 
                              (keyseq->string *entered-sequence*))
              (statusbar-remove statusbar context-id)))))
    ret))

(defun trap-error-handler (condition buf)
  (with-output-to-string (s buf)
    (format s "*** ~a~%" condition)))
     

(defun run-if-paired-parens ()
  (let ((text (text (buffer (object-by-id :command))))
        (parens 0))
;    (format t "~a~%" text)
    (iter
      (for c in-string text)
      (case c
        (#\( (incf parens))
        (#\) (progn
               (incf parens -1)
               (when (< parens 0) (return-from run-if-paired-parens nil))))))
    (when (eql parens 0)      
        (let ((repl (text (buffer (object-by-id :repl))))
              (buf (make-array '(0) :element-type 'base-char
                               :fill-pointer 0 :adjustable t)))
          (with-output-to-string (s buf)
            (princ repl s)
            (format s "~&=> ~a~%" text)
            (handler-case
                (handler-bind 
                    ((warning (lambda (condition)
                                (with-output-to-string (s buf)
                                  (format s "* ~a~%" condition))
                                (muffle-warning condition))))
                  (let ((res (eval (read-from-string text))))
                    (setf *** ** ** * * res)
                    (format s "~a~%" res)))
              (t (var) (trap-error-handler var buf))))          
          (setf (text (buffer (object-by-id :repl))) buf))
        (setf (text (buffer (object-by-id :command))) ""))
    parens))
      
      

(defun on-command-key-press (widget event &rest rest)
  (declare (ignore widget rest))
  (let ((statusbar (object-by-id :status)))
    (if (eql (parse-event event :keyval) (key "Return"))
        (let ((parens (run-if-paired-parens)))
          (statusbar-push statusbar (context-id statusbar :command) 
                          (cond
                            ((null parens) "Close bracket without open")
                            ((> parens 0) "No close bracket")
                            ((< parens 0) "No open bracket")
                            (t "OK"))))
        (statusbar-pop statusbar (context-id statusbar :command)))))

(defun open-file ()
  (let ((d (make-instance 'file-chooser-dialog
                          :action :open
                          :parent (object-by-id :main)
                          :title "Open file")))
    (when (eq (run d) :accept)
      (setf (text (buffer (object-by-id :text)))
            (with-open-file (s (filename d) :element-type '(unsigned-byte 8)
                               :if-does-not-exist :create)
              (setf *file* (filename d))
              (destroy d) ; filename fetched
              (let ((res (make-array (file-length s) 
                                       :element-type '(unsigned-byte 8))))
                  (read-sequence res s)
                  (handler-case
                      (prog1
                          (babel:octets-to-string res :encoding :utf-8)
                        (setf *encoding* :utf-8))
                    (t nil 
                      (prog1 
                          (flexi-streams:octets-to-string 
                           res :external-format :koi8-r)
                        (setf *encoding* :koi8-r)))))))
        (let* ((statusbar (object-by-id :status))
               (context-id (context-id statusbar :file)))
          (statusbar-pop statusbar context-id)
          (statusbar-push statusbar context-id 
                          (format nil "Loaded ~a @ ~a" *file* *encoding*))))))
     
(defun save-file ()
  (with-open-file (s *file* :element-type '(unsigned-byte 8)
                     :direction :output 
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (let ((text (text (buffer (object-by-id :text)))))
      (write-sequence  (if (eq *encoding* :utf-8)
                           (babel:string-to-octets text :encoding :utf-8)
                           (flexi-streams:string-to-octets 
                            text :external-format *encoding*)) s)))
  (let* ((statusbar (object-by-id :status))
         (context-id (context-id statusbar :file)))
    (statusbar-pop statusbar context-id)
    (statusbar-push statusbar context-id 
                    (format nil "Saved ~a @ ~a" *file* *encoding*)))) 



(defmacro act (&body body)
  `(lambda (&rest rest)
     (declare (ignore rest))
     ,@body))


(defun run-emacs ()
  (gtk-init)
  (global-set-key "C-x C-f" 'open-file)
  (global-set-key "C-x C-c" (lambda () (destroy (object-by-id :main))))
  (global-set-key "C-x C-s" 'save-file)
  (show 
   (gtk-model 
     'window :signals '(:destroy :gtk-main-quit
                        :key-press-event on-key-press)
     :width 800 :height 600 :title "Editor" :id :main
     ('v-box
      :expand nil
      ('menu-bar
       ('menu-item 
        :label "File"
        :submenu 
        (gtk-model 
          'menu 
          ('menu-item :label "Open"
                      :signals `(:activate ,(act (open-file))))
          ('menu-item :label "Save"
                      :signals `(:activate ,(act (save-file))))
          ('menu-item :label "Quit" 
                      :signals `(:activate 
                                 ,(act (destroy (object-by-id :main))))))))
      :expand t
      ('h-paned
       :resize t
       ('scrolled-window
        ('text-view :id :text))
       ('v-paned
        ('scrolled-window 
         :min-content-height 100
         ('text-view :id :command
                     :signals '(:key-press-event on-command-key-press)))
        ('scrolled-window
         ('text-view :id :repl))))
      :expand nil
      ('statusbar :id :status))))  
  (gtk-main))

