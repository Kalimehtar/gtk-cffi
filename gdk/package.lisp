;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for gdk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cl-user)

(defpackage #:gdk-cffi
  (:use #:common-lisp #:alexandria
        #:cffi-objects #:g-lib-cffi #:g-object-cffi)
  (:import-from #:cl-cairo2 #:x #:y #:width #:height #:cairo_rectangle_t)
  (:export
   ; types
   #:event-mask
   #:extension-mode
   #:pcolor
   #:prgba
   #:color-struct
   #:event
   ;; methods of event
   #:get-slot
   #:event-type

   #:parse-event
   
   #:rectangle
   #:intersect
   #:union

   #:screen
   ;; slots of screen
   #:height
   #:width

   #:window
   #:modifier-type
   #:window-hints
   #:gravity
   #:geometry
   #:window-edge
   #:window-type-hint

   #:pixmap
   
   #:image
   
   #:pixbuf
   ;; slots of pixbuf
   #:width

   #:with-threads

   #:key
   #:unichar
   
   #:keymap
   #:have-bidi-layouts
   #:caps-lock-state
   #:num-lock-state
   #:keycode
   #:group
   #:level
   #:entries-for-keyval
   #:entries-for-keycode
   #:lookup-key
   #:direction

   #:keyval-name
   #:keyval-from-name
   #:keyval-to-unicode
   #:unicode-to-keyval
   #:keyval-to-upper
   #:keyval-to-lower

   #:gatom

   #:cairo-create
   #:cairo-set-source-pixbuf

   #:drag-action
   ))

(in-package #:gdk-cffi)
(register-package "Gdk" *package*)
(register-prefix *package* 'gdk)
;(register-package "Cairo" (find-package "CL-CAIRO2"))
