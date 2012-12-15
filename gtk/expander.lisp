(in-package :gtk-cffi)

(defclass expander (bin)
  ())

(defcfun gtk-expander-new-with-mnemonic :pointer (label :string))
(defcfun gtk-expander-new :pointer (label :string))

(defmethod gconstructor ((expander expander) 
                         &key label mnemonic &allow-other-keys)
  (if mnemonic
      (gtk-expander-new-with-mnemonic mnemonic)
      (gtk-expander-new label)))

(defgtkslots expander
    label :string
    spacing :int
    expanded :boolean
    use-underline :boolean
    use-markup :boolean
    label-widget pobject
    label-fill :boolean)

(init-slots expander)
    