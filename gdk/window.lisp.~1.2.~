(in-package :gdk-cffi)

(defclass window (g-object)
  ())

(defclass x11-window (window)
  ())

(defbitfield modifier-type
  :shift :lock :control :mod1 :mod2 :mod3 :mod4 :mod5
  :button1 :button2 :button3 :button4 :button5
  (:super #.(ash 1 26)) :hyper :meta (:release #.(ash 1 30)))
