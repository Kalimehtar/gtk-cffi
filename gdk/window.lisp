(in-package :gdk-cffi)

(defclass window (g-object)
  ())

(defclass x11-window (window)
  ())

(defbitfield modifier-type
  :shift :lock :control :mod1 :mod2 :mod3 :mod4 :mod5
  :button1 :button2 :button3 :button4 :button5
  (:super #.(ash 1 26)) :hyper :meta (:release #.(ash 1 30)))

(defbitfield window-hints
  :pos :min-size :max-size :base-size :aspect :resize-inc :win-gravity
  :user-pos :user-size)

(defcenum gravity
  (:north-west 1) :north :north-east :west :center :east
  :south-west :south :south-east :static)

(defcenum window-edge
  :north-west :north :north-east :west :east
  :south-west :south :south-east)

(defcenum window-type-hint
  :normal :dialog :menu :toolbar :splashscreen :utility
  :dock :desktop :dropdown-menu :popup-menu :tooltip :notification :combo :dnd)

(defcstruct* geometry
  (min-width :int)
  (min-height :int)
  (max-widht :int)
  (max-height :int)
  (base-width :int)
  (base-height :int)
  (width-inc :int)
  (height-inc :int)
  (min-aspect :double)
  (max-aspect :double)
  (win-gravity gravity))
