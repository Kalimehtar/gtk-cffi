(in-package :gdk-cffi)

(defgeneric get-slot (event field))
;(defgeneric width (widget))
;(defgeneric height (widget))
(defgeneric draw-pixbuf (drawable gc pixbuf
                                  &optional src-x src-y
                                  dst-x dst-y width height
                                  dither x-dither y-dither))
