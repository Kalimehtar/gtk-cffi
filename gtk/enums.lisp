(in-package :gtk-cffi)

(defcenum text-direction
  :none :ltr :rtl)

(defbitfield junction-sides
  (:none 0) :corner-topleft :corner-topright 
  :corner-bottomleft :corner-bottomright
  (:top #b0011) (:bottom #b1100) (:left #b0101) (:right #b1010))

(defbitfield state-flags
  (:normal 0) :active :prelight :selected :insensitive :inconsistent :focused)

(defcenum direction-type
  :tab-forward :tab-backward :up :down :left :right)

(defcenum orientation
  :horizontal :vertical)

(defcenum policy
  :always :automatic :never)

(defcenum shadow-type
  :none :in :out :etched-in :etched-out)

(defcenum corner-type
  :top-left :bottom-left :top-right :bottom-right)

(defcenum justification
  :left :right :center :fill)

(defcenum pack-type :start :end)

(defcenum relief-style :normal :half :none)

(defcenum position-type :left :right :top :bottom)

(defcenum sort-type :ascending :descending)