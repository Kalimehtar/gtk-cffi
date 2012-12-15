(in-package :g-object-cffi)

(defgeneric (setf value) (val g-value))
(defgeneric value (g-value))
(defgeneric g-type (g-value &rest flags))
(defgeneric init (g-value &rest keys))
(defgeneric unset (g-value))

(defgeneric list-properties (g-object-class))
(defgeneric find-property (g-object-class key))
(defgeneric name (g-object-class))
(defgeneric blurb (g-object-vlass))
(defgeneric nick (g-object-class))
(defgeneric flags (g-object-class))

(defgeneric connect (g-object handler &rest keys))
(defgeneric (setf signals) (signals g-object))
(defgeneric (setf properties) (properties g-object))
