(in-package :gtk-cffi)

(defclass info-bar (box)
  ())

(defslot info-bar message-type message-type)

(deffuns info-bar
  (add-action-widget :void (child pobject) &key (response dialog-response))
  (add-button pobject (name cffi-keyword) (response dialog-response))
  (:set default-response dialog-response)
  (:set-last response-sensitive :boolean (response dialog-response))
  (response :void (resp dialog-response))
  (:get action-area pobject)
  (:get content-area pobject))

(init-slots info-bar)