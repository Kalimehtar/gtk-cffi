(in-package :gtk-cffi)

(defclass combo-box-text (combo-box)
  ())

(defcfun gtk-combo-box-text-new-with-entry :pointer)
(defcfun gtk-combo-box-text-new :pointer)

(defmethod gconstructor ((combo-box-text combo-box-text) &key with-entry)
  (if with-entry
      (gtk-combo-box-text-new-with-entry)
      (gtk-combo-box-text-new)))

(deffuns combo-box-text
  ((combo-box-append . append) :void (id :string) (text :string))
  ((combo-box-insert . insert) 
   :void (position :int) (id :string) (text :string))
  ((combo-box-prepend . prepend) :void (id :string) (text :string))
  (append-text :void (text :string))
  (insert-text :void (position :int) (text :string))
  (prepend-text :void (text :string))
  ((combo-box-remove . remove) :void (position :int))
  (remove-all :void)
  (:get active-text :string))