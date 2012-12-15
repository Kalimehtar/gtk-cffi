;;;
;;; progress-bar.lisp -- GtkProgressBar
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass progress-bar (widget orientable)
  ())

(defcfun gtk-progress-bar-new :pointer)

(defmethod gconstructor ((progress-bar progress-bar) &key)
  (gtk-progress-bar-new))

(defslots progress-bar
  fraction :double
  inverted :boolean
  show-text :boolean
  ellipsize pango-cffi:ellipsize-mode
  pulse-step :double)

(deffuns progress-bar
  (:get text :string &key)
  (:set text :string &key)
  (pulse :void))

(init-slots progress-bar)
