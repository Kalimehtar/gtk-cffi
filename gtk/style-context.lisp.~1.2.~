(in-package :gtk-cffi)

(defclass style-context (g-object)
  (provider (styles :initform nil)))

(defcfun gtk-style-context-new :pointer)

(defmethod gconstructor ((style-context style-context) &key &allow-other-keys)
  (gtk-style-context-new))

(defgtkgetter direction text-direction style-context)
(defgtkgetter junction-sides junction-sides style-context)
(defgtkgetter screen pobject style-context)
(defgtkgetter state state-flags style-context)

(defcfun gtk-style-context-get-color :void 
  (style-context pobject) (state state-flags) (color :pointer))

(defcfun gtk-style-context-get-background-color :void 
  (style-context pobject) (state state-flags) (color :pointer))

(defcfun gtk-style-context-get-border-color :void 
  (style-context pobject) (state state-flags) (color :pointer))

(defgeneric color (object &key type state))
(defmethod color ((style-context style-context) 
                  &key type (state :normal))
  (with-foreign-object (color 'prgba)
    (funcall
     (case type
       (:bg #'gtk-style-context-get-background-color)
       (:border #'gtk-style-context-get-border-color)
       (t #'gtk-style-context-get-color)) style-context state color)
    (convert-from-foreign color 'prgba)))

(defcfun gtk-style-context-get-font pango-cffi:font
  (style-context pobject) (state state-flags))

(defgeneric font (object &key state))
(defmethod font ((style-context style-context) 
                  &key (state :normal))
  (gtk-style-context-get-font style-context state))

(defgtkfun add-provider :void style-context 
           (style-provider pobject) (priority :uint))

(defgeneric load-css (style-context text))
(defmethod load-css ((style-context style-context) text)
  (if (slot-boundp style-context 'provider)
    (css-provider-load (slot-value style-context 'provider) :data text)
    (progn
      (let ((provider (make-instance 'css-provider)))
        (setf (slot-value style-context 'provider) provider)
        (css-provider-load provider :data text)
        (add-provider style-context provider 600)))))

(defun make-css (style-context type state value)
  (let ((found (assoc (list type state) (slot-value style-context 'styles)
                      :test #'equal)))
    (if found
        (setf (cdr found) value)
        (push (cons (list type state) value) 
              (slot-value style-context 'styles))))
  (with-output-to-string (s)
    (mapc (lambda (x)
            (destructuring-bind ((type state) . value) x
              (format s "~a {~a: ~a}"
                      (if (eq state :normal) "*" state)
                      (case type
                        (:bg "background-color")
                        (:border "border-color")
                        (:font "font")
                        ;(:bg-image "border-image")
                        (:bg-image "background-image")
                        (t "color"))
                      value)))
          (slot-value style-context 'styles))))

(defgeneric (setf color) (value object &key type state))
(defmethod (setf color) (value (style-context style-context) 
                         &key type (state :normal))
  (check-type type (member :bg :border nil))
  (load-css style-context (make-css style-context type state value)))

(defgeneric (setf font) (value object &key state))
(defmethod (setf font) (value (style-context style-context) 
                         &key (state :normal))
  (load-css style-context (make-css style-context :font state value)))

(defgeneric (setf bg-pixmap) (value object &key state))
(defmethod (setf bg-pixmap) (value (style-context style-context) 
                         &key (state :normal))
  (load-css style-context 
            (make-css style-context :bg-image state 
                      (format nil 
                              "url('~a')" value))))

(defgeneric bg-pixmap (object &key state))
(defmethod bg-pixmap ((style-context style-context) &key (state :normal))
  (cdr (assoc (list :bg-image state) (slot-value style-context 'styles)
              :test #'equal)))