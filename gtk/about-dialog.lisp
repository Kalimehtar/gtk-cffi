;;;
;;; about-dialog.lisp --- GtkAboutDialog
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass about-dialog (dialog)
  ())

(defcfun gtk-about-dialog-new :pointer)

(defmethod gconstructor ((about-dialog about-dialog) &key &allow-other-keys)
  (gtk-about-dialog-new))

(defcenum license :unknown :custom :gpl-2-0 :gpl-3-0 :lgpl-2-0 :lgpl-3-0
          :bsd :mit-x11 :artistic)

(defslots about-dialog
  program-name :string
  version :string
  copyright :string
  comments :string
  license :string
  license-type license
  website :string
  website-label :string
  authors string-array
  artists string-array
  documenters string-array
  translator-credits :string
  logo pobject
  logo-icon-name :string)


(defmethod run ((dialog about-dialog) &key (keep-alive nil))
  (call-next-method dialog :keep-alive keep-alive))

(init-slots about-dialog)