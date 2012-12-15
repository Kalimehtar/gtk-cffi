(in-package :gtk-cffi)

(defclass menu-bar (menu-shell)
  ())

(defcfun "gtk_menu_bar_new" :pointer)

(defmethod gconstructor ((menu-bar menu-bar) &key  &allow-other-keys)
  (gtk-menu-bar-new))

(defcenum pack-direction
  :ltr :rtl :ttb :btt)

(defgtkslots menu-bar
    pack-direction pack-direction
    child-pack-direction pack-direction)

(init-slots menu-bar nil)