;;;
;;; widget-path.lisp -- GtkWidgetPath
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass widget-path (object)
  ())

(defgtkfun free :void widget-path)

(defcfun gtk-widget-path-new :pointer)

(defmethod gconstructor ((widget-path widget-path) &key &allow-other-keys)
  (gtk-widget-path-new))

(deffuns widget-path
  (to-string :string)
  (append-type :int (type g-type))
  (append-for-widget :int (widget pobject))
  (prepend-type :int (type g-type)))

  