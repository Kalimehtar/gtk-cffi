;;;
;;; entry.lisp -- GtkEditable, GtkEntry, GtkEntryBuffer, GtkEntryCompletion
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass editable (object)
  ())

(defslots editable
  (editable-position . position) :int
  (is-editable . editable) :boolean)

(deffuns editable
  (select-region :void (start :int) (end :int))
  (delete-text :void (start :int) (end :int))
  (:get chars :string (start :int) (end :int))
  (cut-clipboard :void &key)
  (copy-clipboard :void &key)
  (paste-clipboard :void &key)
  (delete-selection :void &key))

(init-slots editable)

(defcfun gtk-editable-get-selection-bounds :void (editable pobject) 
         (start :pointer) (end :pointer))

(defmethod selection-bounds ((editable editable) &key)
  (with-foreign-outs-list ((start :int) (end :int)) :ignore
    (gtk-editable-get-selection-bounds editable start end)))

(defcfun gtk-editable-insert-text :uint (editable pobject) 
         (new-text :string) (new-text-length :int) (position :uint))

(defgeneric insert-text (editable position text)
  (:method ((editable editable) position text)
    (gtk-editable-insert-text editable text (length text) position)))

(defclass entry-buffer (g-object)
  ())

(defcfun gtk-entry-buffer-new :pointer)

(defmethod gconstructor ((entry-buffer entry-buffer)
                         &key &allow-other-keys)
  (gtk-entry-buffer-new))

(defslots entry-buffer
  max-length :int)

(deffuns entry-buffer
  (:get text :string &key)
  (:set text :string &key)
  (:get bytes :int)
  ((entry-buffer-length . get-length) :uint)
  (delete-text :uint (position :uint) (n-chars :int))
  (emit-deleted-text :void (poistion :uint) (n-chars :int)))

(defcfun gtk-entry-buffer-insert-text :uint 
  (entry-buffer pobject) (position :uint) (chars :string) (n-chars :int))

(defmethod insert-text ((entry-buffer entry-buffer) position text)
  (gtk-entry-buffer-insert-text entry-buffer position text (length text)))

(defcfun gtk-entry-buffer-emit-inserted-text :uint 
  (entry-buffer pobject) (position :uint) (chars :string) (n-chars :int))

(defgeneric emit-inserted-text (entry-buffer position text)
  (:method ((entry-buffer entry-buffer) position text)
    (gtk-entry-buffer-emit-inserted-text entry-buffer position 
                                         text (length text))))

(init-slots entry-buffer)

(defclass entry-completion (g-object)
  ())

(defcfun gtk-entry-completion-new :pointer)
(defcfun gtk-entry-completion-new-with-area :pointer (area pobject))

(defmethod gconstructor ((entry-completion entry-completion)
                         &key area &allow-other-keys)
  (initialize entry-completion 'area)
  (if area
      (gtk-entry-completion-new-with-area area)
      (gtk-entry-completion-new)))

(defslots entry-completion
  model pobject
  minimum-key-length :int
  text-column :int
  inline-completion :boolean
  inline-selection :boolean
  popup-completion :boolean
  popup-set-width :boolean
  popup-single-match :boolean)

(deffuns entry-completion
  (:get entry pobject)
  (compute-prefix (:string :free-from-foreign t) (key :string))
  (complete :void)
  (:get completion-prefix :string)
  (insert-prefix :void)
  (insert-action-text :void (index :int) (text :string))
  (insert-action-markup :void (index :int) (markup :string))
  (delete-action :void (index :int)))

(defcfun gtk-entry-completion-set-match-func :void
  (entry-completion pobject) (func pfunction) (data pdata) (notify pfunction))

(defcallback cb-match-func :boolean 
    ((entry-completion pobject) (key :string) (tree-iter (object tree-iter)) 
     (data pdata))
  (funcall data entry-completion key tree-iter))

(defgeneric (setf match-func) (func entry-completion &key data destroy-notify)
  (:method (func (entry-completion entry-completion) &key data destroy-notify)
    (set-callback entry-completion gtk-entry-completion-set-match-func
                  cb-match-func func data destroy-notify)))
(save-setter entry-completion match-func)

(init-slots entry-completion)


(defclass entry (widget editable)
  ())

(defcfun gtk-entry-new :pointer)
(defcfun gtk-entry-new-with-buffer :pointer (buffer pobject))

(defmethod gconstructor ((entry entry)
                         &key buffer &allow-other-keys)
  (initialize entry 'buffer)
  (if buffer
      (gtk-entry-new-with-buffer buffer)
      (gtk-entry-new)))

(defslots entry
    visibility :boolean
    max-length :int
    buffer pobject
    activates-default :boolean
    has-frame :boolean
    inner-border (struct border)
    width-chars :int
    alignment :float
    placeholder-text :string
    overwrite-mode :boolean
    completion pobject
    cursor-hadjustment pobject
    progress-fraction :double
    progress-pulse-step :double)

(defcenum entry-icon-position :primary :secondary)

(deffuns entry
  (:get text :string &key)
  (:set text :string &key)
  (:get text-length :uint16)
  (:set invisible-char unichar)
  (unset-invisible-char :void)
  (:get layout pobject)
  (layout-index-to-text-index :int (layout-index :int))
  (text-index-to-layout-index :int (layout-index :int))
  (progress-pulse :void)
  (im-context-filter-keypress :boolean (event pobject))
  (reset-im-context :void)
  (:get icon-storage-type image-type (icon-pos entry-icon-position))
  (set-icon-drag-source :void (icon-pos entry-icon-position) 
                        (target-list (object target-list)) 
                        (actions drag-action))
  (:get current-icon-drag-source :int)
  (:get icon-at-pos :int (x :int) (y :int)))


(defcfun gtk-entry-get-text-area :void (entry pobject) 
         (area (struct rectangle :out t)))

(defgeneric text-area (entry)
  (:method ((entry entry))
    (let ((r (make-instance 'rectangle)))
      (gtk-entry-get-text-area entry r)
      r)))

(defcfun gtk-entry-get-icon-area :void (entry pobject) 
         (icon-pos entry-icon-position) (area (struct rectangle :out t)))

(defgeneric icon-area (entry icon-pos)
  (:method ((entry entry) icon-pos)
    (let ((r (make-instance 'rectangle)))
      (gtk-entry-get-icon-area entry icon-pos r)
      r)))


(defcfun gtk-entry-get-layout-offsets :void (entry pobject) 
         (x :pointer) (y :pointer))

(defmethod layout-offsets ((entry entry))
  (with-foreign-outs-list ((x :int) (y :int)) :ignore
    (gtk-entry-get-layout-offsets entry x y)))    


(template (item type from) ((pixbuf pobject t)
                            (stock :string t)
                            (icon-name :string t)
                            (gicon pobject t)
                            (activatable :boolean nil)
                            (sensitive :boolean nil)
                            (tooltip-text :string nil)
                            (tooltip-markup :string nil))
  (let ((set-name (if from
                      (symbolicate 'gtk-entry-set-icon-from- item)
                      (symbolicate 'gtk-entry-set-icon- item)))
        (get-name (symbolicate 'gtk-entry-get-icon- 
                               (if (eq item 'icon-name) 'name item)))
        (lisp-name (symbolicate 'icon- item)))
    `(progn
       (defcfun ,set-name :void
         (entry pobject) (icon-pos entry-icon-position) (,item ,type))

       (defgeneric (setf ,lisp-name) (value entry icon-pos)
         (:method (value (entry entry) icon-pos)
           (,set-name entry icon-pos value)))
       
       (defcfun ,get-name ,type
         (entry pobject) (icon-pos entry-icon-position))

       (defgeneric ,lisp-name (entry icon-pos)
         (:method ((entry entry) icon-pos)
           (,get-name entry icon-pos))))))

(init-slots entry)


