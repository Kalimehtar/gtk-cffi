;;;
;;; window.lisp --- GtkWindow
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;
;;; Some conventions
;;; gtk_window_set_position -> (setf (position-type ...))
;;; gtk_window_get_position/gtk_window_move -> window-position (setf'able)
;;; gtk_window_get_default_widget/gtk_window_set_default -> default-widget

(in-package :gtk-cffi)

(defcenum window-type
  :top-level :popup)

(defclass window (bin)
  ())

(defcfun gtk-window-new :pointer (type window-type))

(defmethod gconstructor ((window window)
                         &key (type :top-level) &allow-other-keys)
  (gtk-window-new type))

(defslots window
  title :string
  role :string
  resizable :boolean
  modal :boolean
  gravity gravity
  transient-for pobject
  destroy-with-parent :boolean
  focus pobject
  decorated :boolean
  deletable :boolean
  mnemonic-modifier modifier-type
  type-hint window-type-hint
  skip-taskbar-hint :boolean
  skip-pager-hint :boolean
  urgency-hint :boolean
  accept-focus :boolean
  focus-on-map :boolean
;  default-icon-list g-list-object
;  default-icon-name :string
  icon pobject
  icon-list g-list-object
  icon-name :string
  opacity :double
  mnemonics-visible :boolean
  #+gtk3.2 focus-visible #+gtk3.2 :boolean
  has-resize-grip :boolean
  application pobject
  screen pobject)

(defcfun gtk-window-set-icon-from-file :boolean
  (window pobject) (filename cffi-pathname) (g-error object))

(defmethod (setf icon) ((value pathname) (window window))
  (setf (icon window) (namestring value)))

(defmethod (setf icon) ((value string) (window window))
  (with-g-error g-error
    (unless
        (gtk-window-set-icon-from-file window value g-error)
      (cerror "Continue" "Window icon load error: ~a" g-error))))

  

(defcfun gtk-window-set-default-size
  :void (window pobject) (w :int) (h :int))

(defcfun gtk-window-get-default-size
  :void (window pobject) (w :pointer) (h :pointer))

(defcfun gtk-window-set-default-geometry
  :void (window pobject) (w :int) (h :int))

(defgeneric (setf default-size) (coords window &key geometry &allow-other-keys)
  (:method (coords (window window) &key geometry &allow-other-keys)
    (destructuring-bind (width height) coords
      (if geometry
          (gtk-window-set-default-geometry window (round width) (round height))
          (gtk-window-set-default-size window (round width) (round height))))))

(defgeneric default-size (window)
  (:method ((window window))
    (with-foreign-outs-list ((width :int) (height :int)) :ignore
      (gtk-window-get-default-size window width height))))


(defcenum position
  :none
  :center
  :mouse
  :center-always
  :center-on-parent)

(deffuns window
  (:set (position-type . position) position)
  (add-accel-group :void (accel-group pobject))
  (remove-accel-group :void (accel-group pobject))
  (activate-focus :boolean)
  (activate-default :boolean)
  (set-geometry-hints :void (widget pobject) (geometry (struct geometry))
                      (mask window-hints))
  (is-active :boolean)
  (has-toplevel-focus :boolean)
  (list-toplevels (g-list :free-from-foreign nil))
  (add-mnemonic :void (keyval key) (target pobject))
  (remove-mnemonic :void (keyval key) (target pobject))
  (mnemonic-activate :boolean &key (keyval key) (modifier modifier-type))
  (activate-key :boolean (event event))
  (propagate-key-event :boolean (event event))
  (:get default-widget pobject)
  (:set (default-widget . default) pobject)
  (present :void)
  (present-with-time :void (timestamp :uint32))
  (iconify :void)
  (deiconify :void)
  (stick :void)
  (unstick :void)
  (maximize :void)
  (unmaximize :void)
  (fullscreen :void)
  (unfullscreen :void)
  (:set keep-above :boolean)
  (:set keep-below :boolean)
  (begin-resize-drag :void (edge window-edge) (button :int) (root-x :int) 
                    (root-y :int) (timestamp :uint32))
  (begin-move-drag :void  (button :int) (root-x :int) 
                    (root-y :int) (timestamp :uint32))
  (:get window-type window-type &key)
  (parse-geometry :boolean (geometry :string))
  (reshow-with-initial-size :void)
  (:set auto-startup-notification :boolean)
  (resize-grip-is-visible :boolean)
  (:get group pobject)
  (has-group :boolean)
  (:set startup-id :string))
  
(defcfun gtk-window-get-resize-grip-area :boolean 
  (window pobject) (rect (struct rectangle :out t)))

(defgeneric resize-grip-area (window)
  (:method ((window window))
    (let ((dest (make-instance 'rectangle)))
      (when (gtk-window-get-resize-grip-area window dest)
        dest))))

(defcfun gtk-window-get-position :void (window pobject) 
         (x :pointer) (y :pointer))

(defgeneric window-position (window)
  (:method ((window window))
    (with-foreign-outs-list ((x :int) (y :int)) :ignore
      (gtk-window-get-position window x y))))

(defcfun gtk-window-move :void (window pobject) (x :int) (y :int))

(defgeneric (setf window-position) (coords window)
  (:method (coords (window window))
    (destructuring-bind (x y) coords
      (gtk-window-move window x y))))

(defcfun gtk-window-get-size :void (window pobject) 
         (width :pointer) (height :pointer))

(defcfun gtk-window-resize :void (window pobject) 
         (width :int) (height :int))

(defcfun gtk-window-resize-to-geometry :void (window pobject) 
         (width :int) (height :int))

(defgeneric (setf window-size) (coords window &key geometry &allow-other-keys)
  (:method (coords (window window) &key geometry &allow-other-keys)
    (destructuring-bind (width height) coords
      (if geometry
          (gtk-window-resize-to-geometry window (round width) (round height))
          (gtk-window-resize window (round width) (round height))))))

(defgeneric window-size (window)
  (:method ((window window))
    (with-foreign-outs-list ((width :int) (height :int)) :ignore
      (gtk-window-get-size window width height))))

(defcfun gtk-window-set-default-icon :void (icon pobject))
(defcfun gtk-window-set-default-icon-from-file :boolean 
  (filename cffi-pathname) (g-error object))
(defgeneric (setf default-icon) (icon)
  (:method ((icon string))
    (with-g-error g-error
      (unless (gtk-window-set-default-icon-from-file icon g-error)
        (cerror "Continue" "Default icon load error: ~a" g-error))))
  (:method ((icon pathname))
    (setf (default-icon) (namestring icon)))
  (:method (icon)
    (gtk-window-set-default-icon icon)))

(defcfun (default-icon-list "gtk_window_get_default_icon_list") g-list-object)
(defcfun gtk-window-set-default-icon-list :void (icons g-list-object))
(defun (setf default-icon-list) (value)
  (gtk-window-set-default-icon-list value))

(defcfun (default-icon-name "gtk_window_get_default_icon_name") :string)
(defcfun gtk-window-set-default-icon-name :void (name :string))
(defun (setf default-icon-name) (name)
  (gtk-window-set-default-icon-name name))


(init-slots window ((width -1) (height -1) geometry resize)
  (when (or (/= width -1) (/= height -1))
    (let ((sizes (list width height)))
      (if resize
          (setf (window-size window :geometry geometry) sizes)
          (setf (default-size window :geometry geometry) sizes)))))

