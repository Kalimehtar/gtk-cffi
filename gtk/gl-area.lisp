;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gl-area.lisp --- Wrapper for GtkGlArea
;;;
;;; Copyright (C) 2016, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass gl-area (widget)
  ())

(defcfun gtk-gl-area-new :pointer)

(defmethod gconstructor ((gl-area gl-area) &key &allow-other-keys)
  ""
  (gtk-gl-area-new))

(defslots gl-area
  (g-error . error) :pointer
  has-alpha :boolean
  has-depth-buffer :boolean
  has-stencil-buffer  :boolean
  auto-render :boolean
  use-es :boolean)

(defcfun gtk-gl-area-get-required-version
    :void (area pobject) (major :pointer) (minor :pointer))
(defmethod get-required-version ((gl-area gl-area))
  (with-foreign-outs ((major :int) (minor :int)) :return 
    (gtk-gl-area-get-required-version gl-area major minor)))

(deffuns gl-area
  (:get context pobject)
  (make-current :void)
  (queue-render :void)
  (attach-buffers :void)
  (set-required-version :void (major :int) (minor :int))
  (:get event-window pobject))


