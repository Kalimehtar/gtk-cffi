;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; text-tag.lisp --- GtkTextTag
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;
(in-package :gtk-cffi)

(defclass text-tag (g-object)
  ())

(defgtkslot text-tag priority :int)
(defgtkfun event :boolean text-tag 
           (event-object pobject) (event pobject) (text-iter pobject))

(defcenum wrap-mode
  :none :char :word :word-char)

(defcstruct* text-appearance
  (bg-color pcolor)
  (fg-color pcolor)
  (rise :int)
  (bitfield :unsigned-char))

(defbitaccessors text-appearance bitfield
  (underline :int 4)
  (strikethrough :boolean 1)
  (draw-bg :boolean 1)
  (inside-selection :boolean 1)
  (is-text :boolean 1))
  

(defcstruct* text-attributes
  (appearance (:struct text-appearance))
  (justification justification)
  (direction text-direction)
  (text-attributes-font pango-cffi:font)
  (font-scale :double)
  (left-margin :int)
  (right-margin :int)
  (indent :int)
  (pixels-above-lines :int)
  (pixels-below-lines :int)
  (pixels-inside-wrap :int)
  (tabs pango-cffi:tab-array)
  (wrap-mode wrap-mode)
  (language pango-cffi:language)
  (bitfield :char))

(defbitaccessors text-attributes bitfield
  (invisible :boolean 1)
  (bg-full-height :boolean 1)
  (editable :boolean 1))

(defgtkfuns text-attributes
  (ref (object text-attributes))
  (unref :void))

(defcfun gtk-text-attributes-new :pointer)

(defmethod new-struct ((class (eql 'text-attributes)))
  (gtk-text-attributes-new))

(defmethod free-struct ((class (eql 'text-attributes)) value)
  (unref value))


