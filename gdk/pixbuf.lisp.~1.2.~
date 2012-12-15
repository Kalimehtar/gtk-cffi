(in-package :gdk-cffi)

(defclass pixbuf (g-object)
  ())

(defcfun "gdk_pixbuf_new" :pointer (colorspace :int) (has-alpha :boolean)
  (bits-per-sample :int) (width :int) (height :int))

(defcfun "gdk_pixbuf_new_from_file" :pointer
  (filename gtk-string) (gerror pobject))

(defcfun "gdk_pixbuf_new_from_file_at_scale" :pointer
  (filename gtk-string) (width :int) (height :int)
  (preserve-aspect :boolean) (gerror pobject))

(defcfun "gdk_pixbuf_new_subpixbuf" :pointer
  (pixbuf pobject) (src-x :int) (src-y :int) (dst-x :int) (dst-y :int))

(defcfun "gdk_pixbuf_copy" :pointer
  (pixbuf pobject))

(defgeneric new-from-image (image width height src-x src-y))

;; (defcfun "gdk_pixbuf_get_from_image" :void (pixbuf pobject) (image pobject)
;;   (colormap pobject) (src-x :int) (src-y :int) (dst-x :int) (dst-y :int)
;;   (width :int) (height :int))

;; (defcfun "gdk_pixbuf_get_from_drawable" :void (pixbuf pobject)
;;   (drawable pobject) (colormap pobject)
;;   (src-x :int) (src-y :int) (dst-x :int) (dst-y :int)
;;   (width :int) (height :int))

;(defmethod new-from-image ((image image) width height src-x src-y)
;  (gdk-pixbuf-get-from-image (null-pointer) image (null-pointer)
;                             src-x src-y 0 0 width height))

;(defmethod new-from-image ((drawable drawable) width height src-x src-y)
;  (gdk-pixbuf-get-from-drawable (null-pointer) drawable (null-pointer)
;                             src-x src-y 0 0 width height))

(defmethod gconstructor ((obj-pixbuf pixbuf)
                         &key file loader pixbuf
                         height width
                         has-alpha (bits-per-sample 8)
                         (preserve-aspect-ratio t)
                         src-x src-y
                         &allow-other-keys)
  (declare (ignorable loader))
  (cond
   (file (with-g-error 
           g-error
           (if (and width height)
               (gdk-pixbuf-new-from-file-at-scale file
                                                  width height
                                                  preserve-aspect-ratio g-error)
               (gdk-pixbuf-new-from-file file g-error))))
   
   ;; from GdkImage or GdkDrawable
;   (image (new-from-image image width height src-x src-y))
   
   ;(loader (new-from-loader loader))
   (pixbuf
    (if (and src-x src-y width height)
        (gdk-pixbuf-new-subpixbuf pixbuf src-x src-y width height)
        (gdk-pixbuf-copy pixbuf)))
   (t (gdk-pixbuf-new 0 has-alpha bits-per-sample width height))))

(defcfun "gdk_pixbuf_get_width" :int (pixbuf pobject))
(defcfun "gdk_pixbuf_get_height" :int (pixbuf pobject)) 

(defmethod width ((pixbuf pixbuf))
  (gdk-pixbuf-get-width pixbuf))

(defmethod height ((pixbuf pixbuf))
  (gdk-pixbuf-get-height pixbuf))

(defcenum rgb-dither
  :none :normal :max)

;; (defcfun "gdk_draw_pixbuf" :void (drawable pobject) (gc pobject)
;;   (pixbuf pobject) (src-x :int) (src-y :int) (dst-x :int) (dst-y :int)
;;   (width :int) (height :int) (dither rgb-dither)
;;   (x-dither :int) (y-dither :int))

;; (defmethod draw-pixbuf ((drawable drawable) (gc gc) (pixbuf pixbuf)
;;                         &optional (src-x 0) (src-y 0)
;;                         (dst-x 0) (dst-y 0) (width -1) (height -1)
;;                         (dither :none) (x-dither 0) (y-dither 0))
;;   (gdk-draw-pixbuf drawable gc pixbuf src-x src-y dst-x dst-y
;;                    width height dither x-dither y-dither))
  
