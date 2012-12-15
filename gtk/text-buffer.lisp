;;;
;;; text-buffer.lisp -- GtkTextTagTable, GtkTextIter, GtkTextBuffer
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;
(in-package :gtk-cffi)

(defclass text-tag-table (g-object)
  ())

(defcfun gtk-text-tag-table-new :pointer)

(defmethod gconstructor ((text-tag-table text-tag-table) &key 
                         &allow-other-keys)
  (gtk-text-tag-table-new))

(deffuns text-tag-table
  (add :void (tag pobject))
  ((text-tag-table-remove . remove) :void (tag pobject))
  (lookup pobject (name :string))
  (:get size :int))

(make-foreach text-tag-table (tag (object text-tag)) (data pdata))

(defcstruct* text-iter
  (u1 :pointer)
  (u2 :pointer)
  (u3 :int)
  (u4 :int)
  (u5 :int)
  (u6 :int)
  (u7 :int)
  (u8 :int)
  (u9 :pointer)
  (u10 :pointer)
  (u11 :int)
  (u12 :int)
  (u13 :int)
  (u14 :pointer))

;(defcfun gtk-text-iter-free :void (iter pobject))

;(defmethod free-struct ((class (eql 'text-iter)) value)
;  (gtk-text-iter-free value))

(defslots text-iter
  line :int
  offset :int
  line-offset :int
  line-index :int
  visible-line-index :int
  visible-line-offset :int)

(defbitfield text-search-flags
  :visible-only :text-only :case-insensitive)

(deffuns text-iter
  ((text-iter-char . get-char) unichar)
  (:get slice :string (end pobject))
  ((text-iter-text . get-text) :string (end pobject))
  (:get visible-slice :string (end pobject))
  (:get visible-text :string (end pobject))
  (:get pixbuf pobject)
  (:get marks (g-slist :elt pobject))
  (:get toggled-tags (g-slist :elt pobject) (toggle-on :boolean))
  (:get child-anchor pobject)
  (begins-tag :boolean (tag pobject))
  (ends-tag :boolean (tag pobject))
  (toggles-tag :boolean (tag pobject))
  (has-tag :boolean (tag pobject))
  (:get tags (g-slist :elt pobject))
  ((text-iter-editable . editable) :boolean (default-setting :boolean))
  (can-insert :boolean (default-editability :boolean))
  (starts-word :boolean)
  (ends-word :boolean)
  (inside-word :boolean)
  (starts-line :boolean)
  (starts-sentence :boolean)
  (ends-sentence :boolean)
  (inside-sentence :boolean)
  (is-cursor-position :boolean)
  (:get chars-in-line :int)
  (:get bytes-in-line :int)
  (get-attributes :boolean (attrib (struct text-attributes :out t)))
  (:get language pango-cffi:language)
  (is-end :boolean)
  (is-start :boolean)
  (forward-char :boolean)
  (backward-char :boolean)
  (forward-chars :boolean (count :int))
  (backward-chars :boolean (count :int))
  (forward-line :boolean)
  (backward-line :boolean)
  (forward-lines :boolean (count :int))
  (backward-lines :boolean (count :int))
  (forward-word-end :boolean)
  (backward-word-start :boolean)
  (forward-word-ends :boolean (count :int))
  (backward-word-starts :boolean (count :int))
  (forward-cursor-position :boolean)
  (backward-cursor-position :boolean)
  (forward-cursor-positions :boolean (count :int))
  (backward-cursor-positions :boolean (count :int))
  (backward-sentence-start :boolean)
  (forward-sentence-end :boolean)
  (backward-sentence-starts :boolean (count :int))
  (forward-sentence-ends :boolean (count :int))
  (forward-visible-word-end :boolean)
  (backward-visible-word-start :boolean)
  (forward-visible-word-ends :boolean (count :int))
  (backward-visible-word-starts :boolean (count :int))
  (forward-visible-cursor-position :boolean)
  (backward-visible-cursor-position :boolean)
  (forward-visible-cursor-positions :boolean (count :int))
  (backward-visible-cursor-positions :boolean (count :int))
  (forward-visible-line :boolean)
  (backward-visible-line :boolean)
  (forward-visible-lines :boolean (count :int))
  (backward-visible-lines :boolean (count :int))
  (forward-to-end :void)
  (forward-to-line-end :boolean)
  (forward-to-tag-toggle :boolean (tag pobject))
  (backward-to-tag-toggle :boolean (tag pobject))
  (forward-search :boolean 
                  (str :string) (flags text-search-flags)
                  (match-start (struct text-iter :out t))
                  (match-end (struct text-iter :out t)) (limit pobject))
  (backward-search :boolean 
                   (str :string) (flags text-search-flags)
                   (match-start (struct text-iter :out t))
                   (match-end (struct text-iter :out t)) (limit pobject))
  ((text-iter-equal . equal) :boolean (rhs (struct text-iter)))
  (compare :int (rhs (struct text-iter)))
  (in-range :boolean (start (struct text-iter) (end (struct text-iter))))
  (order :void (rhs pobject)))

(defcallback cb-char-predicate :boolean ((ch unichar) (data :pointer))
  (funcall *callback* ch data))

(defcfun gtk-text-iter-forward-find-char :boolean 
  (text-iter pobject) (pred pfunction) (data (pdata :free-to-foreign t)) 
  (limit pobject))

(defgeneric forward-find-char (text-iter pred &key data limit)
  (:method ((text-iter text-iter) pred &key data limit)
    (if (functionp pred)
        (let ((*callback* pred))
          (gtk-text-iter-forward-find-char text-iter 
                                           (callback cb-char-predicate) 
                                           data limit))
        (gtk-text-iter-forward-find-char text-iter pred data limit))))

(defcfun gtk-text-iter-backward-find-char :boolean 
  (text-iter pobject) (pred pfunction) (data (pdata :free-to-foreign t)) 
  (limit pobject))

(defgeneric backward-find-char (text-iter pred &key data limit)
  (:method ((text-iter text-iter) pred &key data limit)
    (if (functionp pred)
        (let ((*callback* pred))
          (gtk-text-iter-backward-find-char text-iter 
                                            (callback cb-char-predicate) 
                                            data limit))
        (gtk-text-iter-backward-find-char text-iter pred data limit))))



(defclass text-buffer (g-object)
  ())

(defcfun gtk-text-buffer-new :pointer (tag-table pobject))

(defmethod gconstructor ((text-buffer text-buffer)
                         &key tag-table &allow-other-keys)
  (gtk-text-buffer-new tag-table))

(defslots text-buffer
  modified :boolean)

(defcenum text-buffer-target-info
  (:buffer-content -1)
  (:rich-text -2)
  (:info-text -3))

(deffuns text-buffer
  (:get line-count :int)
  (:get char-count :int)
  (:get tag-table pobject)
  (insert-pixbuf :void (text-iter pobject) (pixbuf pobject))
  (insert-child-anchor :void (text-iter pobject) (child-anchor pobject))
  (create-child-anchor pobject (text-iter pobject))
  (create-mark pobject (mark-name :string) (where (struct text-iter)) 
               (left-gravity :boolean))
  (add-mark :void &key (mark pobject) (where (struct text-iter)))
  (:get mark pobject (name :string))
  (get-insert pobject)
  (:get selection-bound pobject)
  (:get has-selection :boolean)
  (place-cursor :void (where (struct text-iter)))
  (select-range :void (ins (struct text-iter)) (bound (struct text-iter)))
  (remove-all-tags :void 
                   (start (struct text-iter)) (end (struct text-iter)))
  (delete-selection :boolean &key (interactive :boolean) 
                    (default-editable :boolean))
  (paste-clipboard :void &key (clipboard pobject) (location pobject) 
                   (default-editable :boolean))
  (copy-clipboard :void &key (clipboard pobject))
  (cut-clipboard :void &key (clipboard pobject) (default-editable :boolean))
  (begin-user-action :void)
  (end-user-action :void)
  (add-selection-clipboard :void (clipboard pobject))
  (remove-selection-clipboard :void (clipboard pobject))
  ((deserialize-can-create-tags . deserialize-get-can-create-tags) 
   :boolean (format gatom))
  (:get copy-target-list (object target-list))
  (:get paste-target-list (object target-list))
  (register-deserialize-tagset gatom (tagset-name :string))
  (register-serialize-tagset gatom (tagset-name :string))
  (unregister-deserialize-format :void (format gatom))
  (unregister-serialize-format :void (format gatom)))

(defcfun gtk-text-buffer-deserialize-set-can-create-tags :void
  (buffer pobject) (format gatom) (val :boolean))

(defgeneric (setf deserialize-can-create-tags) (value text-buffer format)
  (:method (value (text-buffer text-buffer) format)
    (gtk-text-buffer-deserialize-set-can-create-tags text-buffer format value)))

(defcfun gtk-text-buffer-get-start-iter :void 
  (buffer pobject) (text-iter (struct text-iter :out t)))

(defgeneric start-iter (text-buffer &optional text-iter)
  (:method ((text-buffer text-buffer) 
            &optional (text-iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-start-iter text-buffer text-iter)
    text-iter))

(defcfun gtk-text-buffer-get-end-iter :void 
  (buffer pobject) (text-iter (struct text-iter :out t)))

(defgeneric end-iter (text-buffer &optional text-iter)
  (:method ((text-buffer text-buffer) 
            &optional (text-iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-end-iter text-buffer text-iter)
    text-iter))

(defcfun gtk-text-buffer-get-text :string (buffer pobject)
  (start pobject) (end pobject) (include-hidden :boolean))

(defmethod text ((text-buffer text-buffer) &key 
                 (start (start-iter text-buffer)) 
                 (end (end-iter text-buffer)) include-hidden)
;  (format t "got text (~a ~a ~a)~%" text-buffer start end)
  (gtk-text-buffer-get-text text-buffer start end include-hidden))

(defcfun gtk-text-buffer-set-text :void (buffer pobject)
  (str :string) (length :int))

(defmethod (setf text) (text (text-buffer text-buffer) &key (length -1))
  (gtk-text-buffer-set-text text-buffer text length))

(save-setter text-buffer text)

(defcfun gtk-text-buffer-insert :void (buffer pobject) (iter pobject)
         (text :string) (len :int))
(defcfun gtk-text-buffer-insert-at-cursor :void (buffer pobject)
         (text :string) (len :int))
(defcfun gtk-text-buffer-insert-interactive :boolean (buffer pobject) 
         (iter pobject) (text :string) (len :int) 
         (default-editable :boolean))
(defcfun gtk-text-buffer-insert-interactive-at-cursor :boolean (buffer pobject)
         (text :string) (len :int) (default-editable :boolean))

(defgeneric insert (text-buffer place text 
                                &key length interactive default-editable))

(defmethod insert ((text-buffer text-buffer) (text-iter (eql :at-cursor)) 
                   text &key (length -1) interactive default-editable)
  (if interactive
      (gtk-text-buffer-insert-interactive-at-cursor text-buffer text 
                                                    length default-editable)
      (gtk-text-buffer-insert-at-cursor text-buffer text length)))
  
(defmethod insert ((text-buffer text-buffer) text-iter text 
                   &key (length -1) interactive default-editable)
  (if interactive
      (gtk-text-buffer-insert-interactive text-buffer text-iter text 
                                          length default-editable)
      (gtk-text-buffer-insert text-buffer text-iter text length)))

(defcfun gtk-text-buffer-insert-range :void 
  (buffer pobject) (text-iter pobject) 
  (start (struct text-iter)) (end (struct text-iter)))

(defcfun gtk-text-buffer-insert-range-interactive :boolean
  (buffer pobject) (text-iter pobject) (start (struct text-iter)) 
  (end (struct text-iter)) (default-editable :boolean))


(defgeneric insert-range (text-buffer text-iter start end 
                                      &key interactive default-editable)
  (:method ((text-buffer text-buffer) text-iter start end 
            &key interactive default-editable)
    (if interactive
        (gtk-text-buffer-insert-range-interactive text-buffer text-iter
                                                  start end default-editable)
        (gtk-text-buffer-insert-range text-buffer text-iter start end))))


(defcfun gtk-text-buffer-delete :void 
  (buffer pobject) (start pobject) (end pobject))

(defcfun gtk-text-buffer-delete-interactive :boolean
  (buffer pobject) (start pobject) (end pobject) (default-editable :boolean))

(defgeneric text-buffer-delete (text-buffer start end 
                                            &key interactive default-editable)
  (:method ((text-buffer text-buffer) start end
            &key interactive default-editable)
    (if interactive
        (gtk-text-buffer-delete-interactive text-buffer
                                            start end default-editable)
        (gtk-text-buffer-delete text-buffer start end))))

(defcfun gtk-text-buffer-backspace :boolean                  
  (buffer pobject) (text-iter pobject) (interactive :boolean)
  (default-editable :boolean))

(defgeneric backspace (text-buffer text-iter &key interactive default-editable)
  (:method ((text-buffer text-buffer) text-iter 
            &key interactive default-editable)
    (gtk-text-buffer-backspace text-buffer text-iter 
                               interactive default-editable)))

(defcfun gtk-text-buffer-get-slice :string (buffer pobject)
         (start pobject) (end pobject) (include-hidden :boolean))

(defgeneric text-buffer-slice (text-buffer &key start end)
  (:method ((text-buffer text-buffer) &key 
            (start (start-iter text-buffer)) 
            (end (end-iter text-buffer)) include-hidden)
    (gtk-text-buffer-get-slice text-buffer start end include-hidden)))

(macrolet ((by-name-accessor (name tag-name &rest params)
             (let ((cars-params (mapcar #'car params))
                   (by-obj (symbolicate 'gtk-text-buffer- name))
                   (by-name (symbolicate 'gtk-text-buffer- name '-by-name)))
               `(progn
                  (defcfun ,by-obj :void
                    (buffer pobject) (,tag-name pobject) ,@params)
                  (defcfun ,by-name :void
                    (buffer pobject) (,tag-name :string) ,@params)
                  (defgeneric ,name (text-buffer ,tag-name ,@cars-params)
                    (:method ((text-buffer text-buffer) (,tag-name string) 
                              ,@cars-params)
                      (,by-name text-buffer ,tag-name ,@cars-params))
                    (:method ((text-buffer text-buffer) ,tag-name 
                              ,@cars-params)
                      (check-type ,tag-name (or foreign-pointer object))
                      (,by-obj text-buffer ,tag-name ,@cars-params)))))))
             
  (by-name-accessor move-mark mark (where (struct text-iter)))
  (by-name-accessor delete-mark mark (where (struct text-iter)))
  (by-name-accessor apply-tag tag
                    (start (struct text-iter)) (end (struct text-iter)))
  (by-name-accessor remove-tag tag
                    (start (struct text-iter)) (end (struct text-iter))))
  
(defcfun gtk-text-buffer-create-tag :pointer (buffer pobject)
         (name :string) (null :pointer))

(defgeneric create-tag (text-buffer name &rest properties)
  (:method ((text-buffer text-buffer) name &rest properties)
    (let ((res (make-instance 
                'text-tag :pointer
                (gtk-text-buffer-create-tag text-buffer name (null-pointer)))))
      (setf (properties res) properties))))


(defcfun gtk-text-buffer-get-iter-at-line-offset :void
  (buffer pobject) (text-iter pobject) (line :int) (offset :int))
(defcfun gtk-text-buffer-get-iter-at-offset :void
  (buffer pobject) (text-iter pobject) (offset :int))
(defcfun gtk-text-buffer-get-iter-at-line :void
  (buffer pobject) (text-iter pobject) (line :int))
(defcfun gtk-text-buffer-get-iter-at-line-index :void
  (buffer pobject) (text-iter pobject) (line :int) (index :int))
(defcfun gtk-text-buffer-get-iter-at-mark :void
  (buffer pobject) (text-iter pobject) (mark pobject))
(defcfun gtk-text-buffer-get-iter-at-child-anchor :void
  (buffer pobject) (text-iter pobject) (child-anchor pobject))


(defgeneric text-buffer-iter (text-buffer text-iter 
                                          &key line offset index 
                                          mark child-anchor)
  (:documentation "Sets the TEXT-ITER to given position:
priority is CHILD-ANCHOR, MARK, LINE+INDEX, LINE+OFFSET, LINE, OFFSET
OFFSET may be also :start or :end, the sama as 0 and -1")
  (:method ((text-buffer text-buffer) text-iter
            &key line offset index mark child-anchor)
    (unless text-iter
      (setf text-iter (make-instance 'text-iter)))
    (cond
      (child-anchor 
       (gtk-text-buffer-get-iter-at-child-anchor text-buffer 
                                                 text-iter child-anchor))
      (mark
       (gtk-text-buffer-get-iter-at-mark text-buffer text-iter mark))
      (line
       (cond
         (index (gtk-text-buffer-get-iter-at-line-index text-buffer 
                                                        text-iter line index))
         (offset (gtk-text-buffer-get-iter-at-line-offset text-buffer 
                                                          text-iter 
                                                          line offset))
         (t (gtk-text-buffer-get-iter-at-line text-buffer text-iter line))))
      (t 
       (case offset
         ((0 :start) (gtk-text-buffer-get-start-iter text-buffer text-iter))
         ((-1 :end) (gtk-text-buffer-get-end-iter text-buffer text-iter))
         (t (gtk-text-buffer-get-iter-at-offset text-buffer 
                                                text-iter offset)))))
    text-iter))

(defcfun gtk-text-buffer-get-bounds :void 
  (buffer pobject) (start pobject) (end pobject))

(defgeneric bounds (text-buffer &key start end)
  (:method ((text-buffer text-buffer) &key start end)
    (let ((start (or start (make-instance 'text-iter)))
          (end (or end (make-instance 'text-iter))))
      (gtk-text-buffer-get-bounds text-buffer start end)
      (values start end))))

(defcfun gtk-text-buffer-get-selection-bounds :void 
  (buffer pobject) (start pobject) (end pobject))

(defmethod selection-bounds ((text-buffer text-buffer) &key start end)
  (let ((start (or start (make-instance 'text-iter)))
        (end (or end (make-instance 'text-iter))))
    (let ((res (gtk-text-buffer-get-selection-bounds text-buffer start end)))
      (values res start end))))

(defcfun gtk-text-buffer-deserialize :boolean 
  (register-buffer pobject) (content-buffer pobject) (format gatom) 
  (text-iter pobject) (data (garray :uint8)) (length :int) 
  (err (:pointer (:struct g-error))))

(define-condition deserialize-warning (warning) 
  ((g-error :initarg g-error))
  (:report (lambda (condition stream)
             (format stream "GError: ~a" (slot-value condition 'g-error)))))

(defgeneric deserialize (register-buffer content-buffer format text-iter data)
  (:method ((register-buffer text-buffer) (content-buffer text-buffer) 
            format text-iter data)
    (with-g-error g-error
      (gtk-text-buffer-deserialize register-buffer content-buffer format
                                   text-iter data (length data) g-error)
      (when (get-error g-error)
        (signal 'deserialize-warning :g-error g-error)))))

(defcfun gtk-text-buffer-get-deserialize-formats (garray gatom)
  (text-buffer pobject) (n-formats :int))

(defgeneric deserialize-formats (text-buffer)
  (:method ((text-buffer text-buffer))
    (gtk-text-buffer-get-deserialize-formats text-buffer *array-length*)))

(defcfun gtk-text-buffer-get-serialize-formats (garray gatom)
  (text-buffer pobject) (n-formats :int))

(defgeneric serialize-formats (text-buffer)
  (:method ((text-buffer text-buffer))
    (gtk-text-buffer-get-serialize-formats text-buffer *array-length*)))

(defcfun gtk-text-buffer-register-deserialize-format gatom
  (buffer pobject) (mime-type :string) (func pfunction)
  (user-data pdata) (user-data-destroy pfunction))

(defcfun gtk-text-buffer-register-serialize-format gatom
  (buffer pobject) (mime-type :string) (func pfunction)
  (user-data pdata) (user-data-destroy pfunction))

(defcallback cb-serialize (garray :uint8)
    ((register-buffer pobject) (content-buffer pobject) 
     (start (struct text-iter)) (end (struct text-iter)) (size :pointer)
     (user-data pdata))
  (destructuring-bind (func data data-destroy) user-data
    (declare (ignore data-destroy))
    (let ((res (funcall func register-buffer content-buffer start end data)))
      (setf (mem-ref size :int) (length res))
      res)))

(defcallback cb-serialize-destroy :void 
    ((user-data pdata :free-from-foreign t))
  (destructuring-bind (func data data-destroy) user-data
    (declare (ignore func))
    (funcall data-destroy data)))

(defcallback cb-deserialize :boolean
    ((register-buffer pobject) (content-buffer pobject)
     (iter (object text-buffer)) ;; object saves pointer, struct -- doesn't
     (array-data :pointer) (length :ulong)
     (create-tags :boolean) (user-data pdata) 
     (g-error (:pointer (:struct g-error))))
  (destructuring-bind (func data data-destroy) user-data
    (declare (ignore data-destroy))
    (funcall func register-buffer content-buffer iter 
             (progn
               (setf (mem-ref *array-length* :uint) length)
               (convert-from-foreign array-data '(garray :uint8)))
             create-tags data g-error)))


(defgeneric register-deserialize-format (text-buffer mime-type func 
                                                     &key data data-destroy)
  (:method ((text-buffer text-buffer) mime-type func &key data data-destroy)
    (if (pointerp func)
        (gtk-text-buffer-register-deserialize-format text-buffer
                                                     mime-type func 
                                                     data data-destroy)
        (gtk-text-buffer-register-deserialize-format 
         text-buffer mime-type (callback cb-deserialize)
         (list func data data-destroy) (callback cb-serialize-destroy)))))
                                                     
(defgeneric register-serialize-format (text-buffer mime-type func 
                                                     &key data data-destroy)
  (:method ((text-buffer text-buffer) mime-type func &key data data-destroy)
    (if (pointerp func)
        (gtk-text-buffer-register-serialize-format text-buffer
                                                     mime-type func 
                                                     data data-destroy)
        (gtk-text-buffer-register-serialize-format 
         text-buffer mime-type (callback cb-serialize)
         (list func data data-destroy) (callback cb-serialize-destroy)))))

(defcfun gtk-text-buffer-serialize (garray :uint8)
  (register-buffer pobject) (content-buffer pobject) (format gatom)
  (start (struct text-iter)) (end (struct text-iter)) (length :pointer))

(defgeneric serialize (register-buffer content-buffer format &key start end)
  (:method ((register-buffer text-buffer) (content-buffer text-buffer) format 
            &key start end)
    (gtk-text-buffer-serialize register-buffer content-buffer format
                               start end *array-length*)))

(init-slots text-buffer)