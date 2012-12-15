(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage :test-ex2
  (:use #:common-lisp #:gtk-cffi #:cffi-object #:g-object-cffi))

(in-package :test-ex2)

(gtk-init)

(defparameter *apps* (make-hash-table :test 'equal))

(defparameter *mods* '(("main" (400 200))
                       ("sales" (600 400))
                       ("purchase" (400 100))
                       ("inventory" (720 640))
                       ("finance" (480 360))))

(cffi:defcallback clicked
                  :void ((widget :pointer) (activated-module gtk-string))
  (declare (ignore widget))
  (declare (ignorable widget))
  (format t "button_clicked: ~a~%" activated-module)
  (show (gethash activated-module *apps*) :all t)
  (mapcar (lambda (module)
            (unless (string= activated-module (car module))
              (hide (gethash (car module) *apps*))))
          *mods*)
  (run (gethash activated-module *apps*)))
  

(cffi:defcallback on-delete :boolean ((widget :pointer)
                                      (event :pointer)
                                      (module gtk-string))
  (declare (ignore widget event))
  (unless (string= module "main")
    (hide (gethash module *apps*))
    (show (gethash "main" *apps*) :all t)
    (run (gethash "main" *apps*)) t))

(cffi:defcallback on-key :boolean ((widget :pointer)
                                   (event :pointer)
                                   (module gtk-string))
  (declare (ignore widget))
  (when (eq (gdk-cffi:parse-event event :keyval) (gdk-cffi:key :f12))
    (format t "~a~%" module)
    (if (string= module "main")
        (destroy (gethash "main" *apps*))
        (progn
          (hide (gethash module *apps*))
                       (show (gethash "main" *apps*) :all t)
                       (run (gethash "main" *apps*))))))

   
                  

(defun show-buttons (v-box cur-module)
  (let ((h-box (make-instance 'h-box)))
    (pack v-box h-box)
    (pack h-box (make-instance 'label) :fill t :expand t)
    (mapcar (lambda (module)
              (let ((button
                     (make-instance 'button
                                    :label (string-capitalize (car module)))))
                (setf (size-request button) '(80 32))
                (when (string= (car module) cur-module)
                  (mapcar (lambda (x)
                            (setf (color button :type :bg :state x) "#95DDFF"))
                          '(:normal :active :prelight)))
                (pack h-box button)
                (pack h-box (make-instance 'label) :fill t :expand t)
                (setf (gsignal button :clicked
                               :data (cffi:convert-to-foreign 
                                      (car module) 'gtk-string))
                      (cffi:callback clicked))))
            *mods*)))

                  

(defun setup-app (module)
  (let ((dialog (make-instance 'dialog :title (car module) :flags :modal)))
    (setf (window-position dialog) :center-always)
    (setf (size-request dialog) (second module))
    ;(setf (property dialog :content-area-border) 10)
    (let ((top-area (content-area dialog)))
      (flet ((print-out (str)
                        (pack top-area (make-instance 'label
                                          :text str)
                              :fill t :expand t)))
        (mapcar #'print-out
                (if (string= (car module) "main")
                    '("this is main menu" "Press F12 to quit")
                  (cons (format nil
                                "This is ~a module"
                                (car module))
                        '("Press F12 to return to main menu"
                          "or just close the window")))))
      (pack top-area
            (make-instance 'label) :fill t :expand t)
      (show-buttons top-area (car module)))
    ;(setf (has-separator dialog) nil)
    (setf (gsignal dialog :delete-event 
                   :data (cffi:convert-to-foreign (car module) 'gtk-string))
          (cffi:callback on-delete)
          (gsignal dialog :key-press-event 
                   :data (cffi:convert-to-foreign (car module) 'gtk-string))
          (cffi:callback on-key))
    dialog))




(mapcar (lambda (module)
          (setf (gethash (car module) *apps*)
                (setup-app module))) *mods*)

(let ((main-dialog (gethash "main" *apps*)))
  (show main-dialog :all t)
  (run main-dialog)
  (format t "here end~%")
  (destroy main-dialog))

;; Cleanup after dialog
(g-object-cffi::timeout-add :idle #'gtk-main-quit)  
(gtk-main)

;(setf window (make-instance 'window))

;(set-border-width window 6)

;;(modify-bg window :normal "#B2D2DE")

;(modify-bg-pixmap window :normal "/usr/share/pixmaps/gqview.png")

;(cffi:defcfun "gtk_rc_parse_string" :void (str :gtk-string))

;(gtk-rc-parse-string "style \"my-style\" {
;    bg_pixmap[NORMAL] = \"gqview.png\"
;}")

;(set-signal window "destroy" (cffi:foreign-symbol-pointer "gtk_main_quit"))

;(setf v-box (make-instance 'v-box))

;(setf label
;      (make-instance 'label :text "this is centered"))

;(set-alignment label 0.5 0)

;(pack v-box label :start)

;(show-all window)

