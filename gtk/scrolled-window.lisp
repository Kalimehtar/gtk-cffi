(in-package :gtk-cffi)

(defclass scrolled-window (bin)
  ())

(defcfun "gtk_scrolled_window_new" :pointer (hadj pobject) (vadj pobject))

(defmethod gconstructor ((scrolled-window scrolled-window)
                         &key hadj vadj &allow-other-keys)
  (gtk-scrolled-window-new hadj vadj))

(defgtkslots scrolled-window
    hadjustment pobject
    vadjustment pobject
    shadow-type shadow-type
    placement corner-type
    min-content-width :int
    min-content-height :int)

(defgtkfuns scrolled-window
  (unset-placement :void)
  (add-with-viewport :void (child pobject)))


(defcfun gtk-scrolled-window-set-policy :void 
  (win pobject) (hpol policy) (vpol policy))

(defgeneric (setf policy) (policy scrolled-window))
(defmethod (setf policy) (policy (scrolled-window scrolled-window))
  (gtk-scrolled-window-set-policy scrolled-window
                                  (first policy)
                                  (second policy)))
(save-setter scrolled-window policy)

(defcfun gtk-scrolled-window-get-policy :void
  (win pobject) (hpol :pointer) (vpol :pointer))


(defgeneric policy (scrolled-window))
(defmethod policy ((scrolled-window scrolled-window))
  (with-foreign-outs-list ((hpol 'policy) (vpol 'policy)) :ignore
      (gtk-scrolled-window-get-policy scrolled-window hpol vpol)))

(init-slots scrolled-window)
