;; 424

(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage #:test
  (:use #:common-lisp #:gdk-cffi #:gtk-cffi #:g-object-cffi)
  (:shadowing-import-from #:gtk-cffi #:image #:window))
(in-package #:test)

(gtk-init)

(defvar window)
(defvar vbox)
(defvar title)
(defvar hbox)
(defvar vbox-right)

(setf window (make-instance 'window))

(setf (gsignal window :destroy) :gtk-main-quit
      (size-request window) '(600 240))

(add window (setf vbox (make-instance 'v-box)))

(let ((title (make-instance 'label :text
                            "  Place a background image in GtkEventBox\n
Part 2 - using GdkDrawable::draw_pixbuf()")))
  (setf (font title) "Times New Roman Italic 10"
        (color title) "#0000ff"
        (size-request title) '(-1 40))
  (pack* vbox title
         ((make-instance 'label))
         ((setf hbox (make-instance 'h-box :homogeneous t))
          :expand t :fill t)))

(defun expose-event (widget context &optional (img "none"))
  (format t "~a ~a ~a~%" widget context img)
  (let* ((pixbuf (make-instance 'pixbuf :file img))
         (w (width pixbuf))
         (dest-x (- (width (allocation widget)) w))
         (dest-y 0))
    (format t "~a~%" pixbuf)
      (cl-cairo2:with-context ((make-instance 'cl-cairo2:context 
                                              :pointer context))
        (unless (cffi:null-pointer-p (cffi-objects:pointer pixbuf))
          (cairo-set-source-pixbuf pixbuf dest-x dest-y)
          (cl-cairo2:paint))
        (let ((ch (child widget)))
          (when ch (propagate-draw widget ch)))))
  t)
    
;    (draw-pixbuf (gdk-window widget)
;                 (style-field widget :bg-gc) pixbuf 0 0 dest-x dest-y)
    ;(let ((ch (child widget)))
    ;  (when ch
    ;    (propagate- widget ch event)))
      

(let  ((eventbox-left (make-instance 'event-box))
       (vbox-left (make-instance 'v-box :homogeneous t)))
  (pack hbox eventbox-left :expand t :fill t)
  (add eventbox-left vbox-left)
  (pack* vbox-left
         ((make-instance 'label :text "This is left eventbox."))
         ((make-instance 'label :text "The green ball is the bg image."))
         ((make-instance 'label :text "Note that this eventbox"))
         ((make-instance 'label :text "uses the default gray backgd color.")))
  (setf (gsignal eventbox-left :draw :data "ball_green3.png")
        #'expose-event))

(let  ((eventbox-right (make-instance 'event-box)))
  (pack hbox eventbox-right :expand t :fill t)
  (add eventbox-right (setf vbox-right (make-instance 'v-box :homogeneous t)))
  (pack* vbox-right
         ((make-instance 'label :text "This is right eventbox."))
         ((make-instance 'label :text "The blue ball is the bg image."))
         ((make-instance 'label :text "Note that you can also set"))
         ((make-instance 'label :text "backgd color for the eventbox!")))
  (setf (color eventbox-right :type :bg) "#BAFFB3")
  (setf (gsignal eventbox-right :draw :data "ball_blue3.png")
        #'expose-event))

(show window :all t)
(gtk-main)




        