(in-package :gtk-cffi)

(defclass message-dialog (dialog)
  ())

(defcenum buttons-type
  :none :ok :close :cancel :yes-no :ok-cancel)

(defcenum message-type
  :info :warning :question :error :other)

(defcfun gtk-message-dialog-new :pointer (parent pobject)
  (flags dialog-flags) (type message-type) (buttons buttons-type)
  (message :string))

(defcfun gtk-message-dialog-new-with-markup :pointer (parent pobject)
  (flags dialog-flags) (type message-type) (buttons buttons-type)
  (message :string))


(defmethod gconstructor ((message-dialog message-dialog)
                         &key parent (flags 0)
                         (type :info) (buttons :ok)
                         (message "") markup &allow-other-keys)
  (funcall
   (if markup #'gtk-message-dialog-new-with-markup
     #'gtk-message-dialog-new)
   parent flags type buttons message))

(defun show-message (parent message &key (type :info) (buttons :ok) markup)
  (run (make-instance 'message-dialog :parent parent
                      :message message
                      :type type :buttons buttons :markup markup) 
       :keep-alive nil))

(defslot message-dialog image pobject)
(deffuns message-dialog
  (:set markup :string)
  (:get message-area pobject)
  (format-secondary-text :void (message :string))
  (format-secondary-markup :void (message :string)))
