;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; button.lisp --- Wrappers for GtkButton, GtkCheckButton, GtkToggleButton, 
;;;                          GtkScaleButton, GtkRadioButton, GtkVolumeButton,
;;;                          GtkLockButton
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass button (bin actionable activatable)
  ())

(defcfun gtk-button-new :pointer)
(defcfun gtk-button-new-with-label :pointer (label :string))
(defcfun gtk-button-new-with-mnemonic :pointer (label :string))
(defcfun gtk-button-new-from-stock :pointer (label cffi-keyword))

(defmethod gconstructor ((button button)
                         &key label type &allow-other-keys)
  "type can be :stock or :mnemonic, any other means button with label"
  (initialize button '(label type))
  (if label
      (let ((creator
             (case type
               (:stock #'gtk-button-new-from-stock)
               (:mnemonic #'gtk-button-new-with-mnemonic)
               (otherwise #'gtk-button-new-with-label))))
        (funcall creator label))
    (gtk-button-new)))

(defslots button
  relief relief-style
  label :string
  use-stock :boolean
  use-underline :boolean
  focus-on-click :boolean
  image pobject
  image-position position-type)

(deffuns button
  (clicked :void)
  (:get event-window pobject))

(defcfun gtk-button-set-alignment :void (button pobject) (x :float) (y :float))
(defmethod (setf alignment) (coords (button button))
  (gtk-button-set-alignment button
                          (float (first coords))
                          (float (second coords))))
(save-setter button alignment)

(defcfun gtk-button-get-alignment :void 
  (button pobject) (x :pointer) (y :pointer))

(defmethod alignment ((button button))
  (with-foreign-outs-list ((x :float) (y :float)) :ignore
    (gtk-button-get-alignment button x y)))

(init-slots button)

(defclass toggle-button (button)
  ())

(defcfun gtk-toggle-button-new :pointer)
(defcfun gtk-toggle-button-new-with-label :pointer (label :string))
(defcfun gtk-toggle-button-new-with-mnemonic :pointer (label :string))

(defmethod gconstructor ((toggle-button toggle-button) &key label type)
  (initialize toggle-button '(label type))
  (if label
      (case type
        (:mnemonic (gtk-toggle-button-new-with-mnemonic label))
        (otherwise (gtk-toggle-button-new-with-label label)))
    (gtk-toggle-button-new)))

(defslots toggle-button
  mode :boolean
  active :boolean
  inconsistent :boolean)

(deffuns toggle-button
  (toggled :void))

(init-slots toggle-button)

(defclass check-button (toggle-button)
  ())

(defcfun gtk-check-button-new :pointer)
(defcfun gtk-check-button-new-with-label :pointer (label :string))
(defcfun gtk-check-button-new-with-mnemonic :pointer (label :string))

(defmethod gconstructor ((check-button check-button) &key label type)
  (initialize check-button '(label type))
  (if label
      (case type
        (:mnemonic (gtk-check-button-new-with-mnemonic label))
        (otherwise (gtk-check-button-new-with-label label)))
    (gtk-check-button-new)))

(defclass radio-button (check-button)
  ())

(defcfun gtk-radio-button-new :pointer)
(defcfun gtk-radio-button-new-with-label :pointer (label :string))
(defcfun gtk-radio-button-new-with-mnemonic :pointer (label :string))

(defcfun gtk-radio-button-new-from-widget :pointer (group-member pobject))
(defcfun gtk-radio-button-new-with-label-from-widget :pointer 
  (group-member pobject) (label :string))
(defcfun gtk-radio-button-new-with-mnemonic-from-widget :pointer 
  (group-member pobject) (label :string))


(defmethod gconstructor ((radio-button radio-button) &key label type widget)
  (initialize radio-button '(label type widget))
  (if label
      (case type
        (:mnemonic (if widget 
                       (gtk-radio-button-new-with-mnemonic-from-widget 
                        widget label)
                       (gtk-radio-button-new-with-mnemonic label)))
        (otherwise (if widget 
                       (gtk-radio-button-new-with-label-from-widget widget 
                                                                    label)
                       (gtk-radio-button-new-with-label label))))
      (if widget 
          (gtk-radio-button-new-from-widget widget) 
          (gtk-radio-button-new))))

(defclass radio-group (object)
  ())

(defgeneric as-list (object)
  (:method ((radio-button radio-button))
    (convert-from-foreign (pointer radio-button) 
                          '(g-slist :free-from-foreign nil))))

(defslot radio-button group (object radio-group))
(deffuns radio-button
  (join-group :void (group-source pobject)))

(init-slots radio-button)

(defclass link-button (button)
  ())

(defcfun gtk-link-button-new :pointer (uri :string))
(defcfun gtk-link-button-new-with-label :pointer (uri :string) (label :string))


(defmethod gconstructor ((link-button link-button) &key uri label)
  (initialize link-button '(label uri))
  (if label
      (gtk-link-button-new-with-label uri label)
      (gtk-link-button-new uri)))

(defslots link-button
  uri :string
  visited :boolean)

(init-slots link-button)

(defclass scale-button (button)
  ())

(defcfun gtk-scale-button-new :pointer)

(defmethod gconstructor ((scale-button scale-button) &key)
  (gtk-scale-button-new))

(defslots scale-button
  adjustment pobject
  value :double)

(deffuns scale-button
  (:set icons (null-array :string))
  (:get popup pobject)
  (:get plus-button pobject)
  (:get minus-button pobject))

(init-slots scale-button)
  
(defclass volume-button (scale-button)
  ())

(defcfun gtk-volume-button-new :pointer)

(defmethod gconstructor ((volume-button volume-button) &key)
  (gtk-volume-button-new))


(defclass lock-button (button)
  ())

(defcfun gtk-lock-button-new :pointer (permission pobject))

(defmethod gconstructor ((lock-button lock-button) &key permission)
  (initialize lock-button 'permission)
  (gtk-lock-button-new permission))

(defslot lock-button permission pobject)
