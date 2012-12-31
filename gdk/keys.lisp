(in-package :gdk-cffi)

(defcfun (keyval-from-name "gdk_keyval_from_name") :uint (val :string))
(defcfun (keyval-name "gdk_keyval_name") :string (val :uint))
(defcfun (keyval-to-upper "gdk_keyval_to_upper") :uint (val :uint))
(defcfun (keyval-to-lower "gdk_keyval_to_lower") :uint (val :uint))

(define-foreign-type unichar ()
  ()
  (:actual-type :uint32)
  (:simple-parser unichar))

(defmethod translate-to-foreign (value (unichar unichar))
  (char-code value))

(defmethod translate-from-foreign (value (unichar unichar))
  (code-char value))

(defcfun (keyval-to-unicode "gdk_keyval_to_unicode") unichar (val :uint))
(defcfun (unicode-to-keyval "gdk_unicode_to_keyval") :uint (val unichar))

(defun key (value)
  (keyval-from-name (string value)))

(define-foreign-type key ()
  ()
  (:actual-type :uint)
  (:simple-parser key))

(defmethod translate-to-foreign (value (key key))
  (typecase value
    (integer value)
    (t (key value))))

(defclass keymap (g-object)
  ())

(defcfun gdk-keymap-get-default :pointer)
(defcfun gdk-keymap-get-for-display :pointer (display pobject))

(defmethod gconstructor ((keymap keymap) &key display)
  (if display
      (gdk-keymap-get-for-display display)
      (gdk-keymap-get-default)))

(defclass keymap-key (struct) ())
(defcstruct* keymap-key
  (keycode :uint)
  (group :int)
  (level :int))

(defgdkfuns keymap
  (lookup-key :uint (key :pointer))
  (:get direction pango-cffi:direction)
  (have-bidi-layouts :boolean)
  (:get caps-lock-state :boolean)
  (:get num-lock-state :boolean))

(defcfun gdk-keymap-translate-keyboard-state :boolean
  (keymap pobject) (hardware-keycode :uint) (state modifier-type) (group :int) 
  (keyval :pointer) (effective-group :pointer) (level :pointer)
  (consumed-modifier :pointer))

(defgeneric translate-keyboard-state (keymap hardware-keycode state group))
(defmethod translate-keyboard-state ((keymap keymap) 
                                     hardware-keycode state group)
  (with-foreign-outs ((keyval :uint) (effective-group :int) (level :int)
                      (consumed-modifier 'modifier-type)) :if-success
    (gdk-keymap-translate-keyboard-state keymap hardware-keycode state group
                                         keyval effective-group level
                                         consumed-modifier)))

(defcfun gdk-keymap-get-entries-for-keyval :boolean
  (keymap pobject) (keyval key) (keys :pointer) (n-keys :pointer))

(defgeneric entries-for-keyval (keymap keyval))
(defmethod entries-for-keyval ((keymap keymap) keyval)
    (with-foreign-out (keys '(garray (struct keymap-key))) :if-success
      (gdk-keymap-get-entries-for-keyval 
       keymap keyval keys *array-length*)))

(defcfun gdk-keymap-get-entries-for-keycode :boolean
  (keymap pobject) (hardware-keycode :uint) 
  (keys :pointer) (keyvals :pointer) (n-keys :pointer))


(defgeneric entries-for-keycode (keymap keycode))
(defmethod entries-for-keycode ((keymap keymap) keycode)
    (with-foreign-outs ((keys '(garray (struct keymap-key)))
                        (keyvals '(garray :uint))) :if-success
      (gdk-keymap-get-entries-for-keycode
       keymap keycode keys keyvals *array-length*)))
