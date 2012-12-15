;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; text-view.lisp --- GtkTextView, GtkTextChildAnchor
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass text-view (container scrollable)
  ())

(defcfun gtk-text-view-new-with-buffer :pointer (buffer pobject))

(defcfun gtk-text-view-new :pointer)

(defmethod gconstructor ((text-view text-view)
                         &key buffer &allow-other-keys)
  (initialize text-view 'buffer)
  (if buffer
      (gtk-text-view-new-with-buffer buffer)
    (gtk-text-view-new)))

(defcenum text-window-type
  :private :widget :text
  :left :right :top :bottom)

(defslots text-view
    buffer pobject
    wrap-mode wrap-mode
    editable :boolean
    cursor-visible :boolean
    overwrite :boolean
    pixels-above-lines :int
    pixels-below-lines :int
    pixels-inside-wrap :int
    justification justification
    left-margin :int
    right-margin :int
    indent :int
    tabs pango-cffi:tab-array
    accepts-tab :boolean)


(deffuns text-view
  (scroll-to-mark :void 
                  (text-mark pobject) (within-margin :double)
                  (use-align :boolean) (xalign :double) (yalign :double))
  (scroll-to-iter :void 
                  (text-iter pobject) (within-margin :double)
                  (use-align :boolean) (xalign :double) (yalign :double))
  (scroll-mark-onscreen :void (text-mark pobject))
  (move-mark-onscreen :boolean (text-mark pobject))
  (place-cursor-onscreen :boolean)
  ((text-view-window . get-window) pobject (win text-window-type))
  (:get window-type text-window-type &key (window pobject))
  (:get border-window-size :int (type text-window-type))
  (:set-last border-window-size :int (type text-window-type))
  (forward-display-line :boolean (text-iter pobject))
  (backward-display-line :boolean (text-iter pobject))
  (forward-display-line-end :boolean (text-iter pobject))
  (backward-display-line-start :boolean (text-iter pobject))
  (starts-display-line :boolean (text-iter pobject))
  (move-visually :boolean (text-iter pobject) (count :int))
  (add-child-at-anchor :void (child pobject) (anchor pobject))
  (add-child-in-window :void 
                       (child pobject) (win text-window-type)
                       (xpos :int) (ypos :int))
  (move-child :void (child pobject) (xpos :int) (ypos :int))
  (:get default-attributes (struct text-attributes))
  (im-context-filter-keypress :boolean (event pobject))
  (reset-im-context :void))

(defclass text-child-anchor (g-object)
  ())

(defcfun gtk-text-child-anchor-new :pointer)
(defmethod gconstructor ((text-child-anchor text-child-anchor) &key 
                         &allow-other-keys)
  (gtk-text-child-anchor-new))

(defgtkgetter widgets g-list-object text-child-anchor)



(init-slots text-view)



  