;;;
;;; activatable.lisp -- GtkActivatable
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass activatable (object)
  ())

(defslots activatable
  related-action pobject
  use-action-appearance :boolean)

(deffuns activatable
  (do-set-related-action :void (action pobject))
  (sync-action-properties :void (action pobject)))

(init-slots activatable)
  