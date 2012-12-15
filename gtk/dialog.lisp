;;;
;;; dialog.lisp -- GtkDialog
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass dialog (window)
  ())

(defbitfield dialog-flags
  :modal
  :destroy-with-parent
  :no-separator)

(defcenum dialog-response
  (:help -11)
  :apply :no :yes :close :cancel :ok :delete :accept :reject :none)

(defcfun gtk-dialog-new-with-buttons
  :pointer (title :string)
  (parent pobject) (flags dialog-flags) (null :pointer))

(defcfun gtk-dialog-new :pointer)

(defmethod gconstructor ((dialog dialog)
                         &key title parent (flags 0) with-buttons 
                         &allow-other-keys)
  (prog1
      (if title
          (gtk-dialog-new-with-buttons title parent flags (null-pointer))
          (gtk-dialog-new))
    (dolist (button-description with-buttons)
      (destructuring-bind (str resp) button-description
        (add-button dialog str resp)))))
                         
(defcfun gtk-dialog-run dialog-response (dialog pobject))

(defgeneric run (dialog &key)
  (:method ((dialog dialog) &key cleanup)
    (prog1 (gtk-dialog-run dialog)
      (when cleanup
        (destroy dialog)))))

;(defcfun gtk-dialog-add-button pobject (dialog pobject)
;  (str :string) (resp dialog-response))

;(defgeneric add-button (dialog string response)
;  (:method ((dialog dialog) str response)
;    (gtk-dialog-add-button dialog (if (keywordp str) (string-downcase str) str)
;                           response)))

(deffuns dialog
  (response :void (resp dialog-response))
  (add-button pobject (name cffi-keyword) (response dialog-response))
  (add-action-widget :void (child pobject) &key (response dialog-response))
  (:set default-response dialog-response)
  (:set-last response-sensitive :boolean (response dialog-response))
  (:get response-for-widget dialog-response (widget pobject))
  (:get widget-for-response pobject (response dialog-response))
  (:get action-area pobject)
  (:get content-area pobject))

(defcfun gtk-alternative-dialog-button-order :boolean (screen pobject))

(defgeneric alternative-dialog-button-order (screen)
  (:method ((screen screen))
    (gtk-alternative-dialog-button-order screen)))

(defcfun gtk-dialog-set-alternative-button-order-from-array
    :void (dialog pobject) (n-params :int) (new-order (carray :int)))

(defgeneric (setf alternative-button-order) (order dialog) 
  (:method (order (dialog dialog))
    (gtk-dialog-set-alternative-button-order-from-array 
         dialog (length order) order)
    order))
(save-setter dialog alternative-button-order)

(init-slots dialog)