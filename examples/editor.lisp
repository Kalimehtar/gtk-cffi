(asdf:oos 'asdf:load-op :gtk-cffi)
(asdf:oos 'asdf:load-op :babel)
(asdf:oos 'asdf:load-op :flexi-streams)


(defpackage #:editor
  (:use #:common-lisp #:gtk-cffi #:g-object-cffi))
(in-package #:editor)


(gtk-init)

(defvar *window*)

(defun open-file (&rest rest)
  (declare (ignore rest))
  (let ((d (make-instance 'file-chooser-dialog
                          :action :open
                          :parent *window*
                          :title "Open file")))
    (when (eq (run d) :accept)
      (setf (text (buffer (object-by-id :main-text)))
            (with-open-file (s (filename d) :element-type '(unsigned-byte 8))
              (destroy d) ; filename fetched
              (let ((res (make-array (file-length s) 
                                     :element-type '(unsigned-byte 8))))
                (read-sequence res s)
                (handler-case (babel:octets-to-string res :encoding :utf-8)
                  (t nil (flexi-streams:octets-to-string 
                          res :external-format :koi8-r)))))))))


(defun save-file (&rest rest)
  (format t "~a" rest))


(setq *window*
  (gtk-model 
    'window :signals '(:destroy :gtk-main-quit)
    :width 950 :height 600 :title "Editor"
    ('v-box
     :expand nil
     ('menu-bar
      ('menu-item 
       :label "File"
       :submenu 
       (gtk-model 
         'menu 
         ('menu-item :label "Open"
                     :signals '(:activate open-file))
         ('menu-item :label "Save"
                     :signals '(:activate save-file))
         ('menu-item :label "Quit" 
                     :signals `(:activate ,(lambda (&rest rest)
                                                   (declare (ignore rest))
                                                   (destroy *window*)))))))
      :expand t
     ('h-box
      :expand nil
      ;('h-paned
      ('scrolled-window
       ('tree-view))
      :expand t
      ('frame
       ('v-box
       :expand nil
       ('label :text "Main window")
       :expand t
       ('scrolled-window
        ('text-view :id :main-text))))
      ('v-box
       :expand nil
       ('label :text "REPL")
       :expand t
       ('scrolled-window
        ('text-view :id :text3))))
     :expand nil
     ('statusbar))))

;(setf ;(text (buffer (object-by-id :text1))) "1"
;      (text (buffer (object-by-id :text2))) "2"
;      (text (buffer (object-by-id :text3))) "3")

(show *window*)
(gtk-main)
      
      
     