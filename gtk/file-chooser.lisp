(in-package :gtk-cffi)

(defclass file-chooser (object)
  ())

(defcenum file-chooser-action
  :open :save :select-folder :create-folder)

(defslots file-chooser
  filename :string
  action file-chooser-action
  local-only :boolean
  select-multiple :boolean
  show-hidden :boolean
  do-overwrite-confirmation :boolean
  create-folders :boolean
  current-folder-uri :string
  preview-widget pobject
  preview-widget-active :boolean
  use-preview-label :boolean
  extra-widget pobject
  filter pobject)
  