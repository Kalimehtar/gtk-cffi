(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage :test-paned
  (:use #:common-lisp #:gtk-cffi))

(in-package :test-paned)

(gtk-init)
;; GtkWidget *hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
;; GtkWidget *frame1 = gtk_frame_new (NULL);
;; GtkWidget *frame2 = gtk_frame_new (NULL);
;; gtk_frame_set_shadow_type (GTK_FRAME (frame1), GTK_SHADOW_IN);
;; gtk_frame_set_shadow_type (GTK_FRAME (frame2), GTK_SHADOW_IN);

;; gtk_widget_set_size_request (hpaned, 200, -1);

;; gtk_paned_pack1 (GTK_PANED (hpaned), frame1, TRUE, FALSE);
;; gtk_widget_set_size_request (frame1, 50, -1);

;; gtk_paned_pack2 (GTK_PANED (hpaned), frame2, FALSE, FALSE);
;; gtk_widget_set_size_request (frame2, 50, -1);

(let ((window (make-instance 'window :width 200 :height 200 
                             :signals '(:destroy :gtk-main-quit)))
      (hpaned (make-instance 'h-paned))
      (frame1 (make-instance 'frame))
      (frame2 (make-instance 'frame)))
  (setf (shadow-type frame1) :in
        (shadow-type frame2) :in
        (size-request hpaned) '(200 -1))
  (pack hpaned frame1 :pane-type 1 :resize t :shrink nil)
  (setf (size-request frame1) '(50 -1))
  (pack hpaned frame2 :resize nil :shrink nil)
  (setf (size-request frame2) '(50 -1))
  (add window hpaned)
  (show window)
  (gtk-main))
        