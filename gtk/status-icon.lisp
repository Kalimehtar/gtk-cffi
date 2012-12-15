(in-package :gtk-cffi)

(defclass status-icon (g-object)
  ())

(defcfun gtk-status-icon-new-from-file :pointer (filename cffi-pathname))
(defcfun gtk-status-icon-new-from-pixbuf :pointer (pixbuf pobject))
(defcfun gtk-status-icon-new-from-icon-name :pointer (icon-name :string))
(defcfun gtk-status-icon-new-from-stock :pointer (stock-id cffi-keyword))
(defcfun gtk-status-icon-new-from-gicon :pointer (gicon pobject))
(defcfun gtk-status-icon-new :pointer)

(defmethod gconstructor ((status-icon status-icon) 
                         &key file pixbuf stock-id gicon icon-name)
  (cond
    (file (gtk-status-icon-new-from-file file))
    (pixbuf (gtk-status-icon-new-from-pixbuf pixbuf))
    (stock-id (gtk-status-icon-new-from-stock stock-id))
    (icon-name (gtk-status-icon-new-from-icon-name icon-name))
    (gicon (gtk-status-icon-new-from-gicon gicon))
    (t (gtk-status-icon-new))))

(defcfun gtk-status-icon-set-from-file :pointer 
  (status-icon pobject) (filename cffi-pathname))
(defcfun gtk-status-icon-set-from-pixbuf :pointer 
  (status-icon pobject) (pixbuf pobject))
(defcfun gtk-status-icon-set-from-icon-name :pointer 
  (status-icon pobject) (icon-name :string))
(defcfun gtk-status-icon-set-from-stock :pointer 
  (status-icon pobject) (stock-id :string))
(defcfun gtk-status-icon-set-from-gicon :pointer 
  (status-icon pobject) (gicon pobject))

(defmethod reinitialize-instance ((status-icon status-icon) 
                                  &key file pixbuf stock-id gicon icon-name)
  (cond
    (file (gtk-status-icon-set-from-file status-icon file))
    (pixbuf (gtk-status-icon-set-from-pixbuf status-icon pixbuf))
    (stock-id (gtk-status-icon-set-from-stock status-icon stock-id))
    (icon-name (gtk-status-icon-set-from-icon-name status-icon icon-name))
    (gicon (gtk-status-icon-set-from-gicon status-icon gicon))))

(defslots status-icon
  screen pobject
  tooltip-text :string
  tooltip-markup :string
  has-tooltip :boolean
  title :string
  visible :boolean)

(deffuns status-icon
  ((name . get-icon-name) :string)
  (:set name :string)
  (is-embedded :boolean)
  (:get x11-window-id :uint32)
  (:get storage-type image-type)
  (:get pixbuf pobject)
  (:get stock :string)
  (:get gicon pobject)
  (:get size :int))
  

(defcfun gtk-status-icon-get-geometry :boolean 
  (status-icon pobject) (screen :pointer) (area (struct rectangle :out t)) 
  (orientation :pointer))

(defgeneric geometry (status-icon)
  (:method ((status-icon status-icon))
    (let ((area (make-instance 'rectangle)))
      (with-foreign-objects ((screen :pointer) (orientation 'orientation))
        (when (gtk-status-icon-get-geometry status-icon screen area orientation)
          (list (make-instance 'screen :pointer (mem-ref screen :pointer))
                area (mem-ref orientation 'orientation)))))))

;; gtk_status_icon_position_menu can be used 
;; in menu-popup as :gtk-status-icon-position-menu


(init-slots status-icon)