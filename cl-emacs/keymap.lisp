(in-package :emacs)

(defparameter *entered-sequence* nil)
(defvar *global-keymap* nil)

(let (keymap) 
  (defun gdk-keymap ()
    (unless keymap
      (setf keymap (make-instance 'keymap)))
    keymap))

(defun base-keycode (key)
  (let ((keys (entries-for-keyval (gdk-keymap) key)))
    (unless keys
      (warn "No keycode. Bad key description ~a" key)
      (return-from base-keycode nil))

    (let ((filtered (delete-if-not (compose (curry #'eql 0) #'group) keys)))
      (unless (= (length filtered) 1)
        (warn "No unique latin keycode for ~a" key)
        (return-from base-keycode nil))
      (keycode (aref filtered 0)))))

(defun base-keyval (keycode)
  (multiple-value-bind (keys keyvals) 
      (entries-for-keycode (gdk-keymap) keycode)
    (iter
      (for key in-vector keys)
      (for keyval in-vector keyvals)
      (when (and (zerop (group key)) (zerop (level key)))
        (return-from base-keyval keyval)))))

(defmacro current-global-map ()
  '*global-keymap*)

(defun symbol-name< (x y)
  (string< (symbol-name x)
           (symbol-name y)))

(defun %define-key (keymap key-seq binding)
  "KEY-SEQ is a string in format C-x or C-Return or M-period. 
Or S-M-period = M-> from emacs. Keys are for latin keymap. 
Key names from X Window"
  (let* ((new-key 
          (mapcar
           (lambda (key)
             (let (flags)
               (macrolet 
                   ((find-prefix (prefix flag)
                      `(when (and (> (length key) 1) 
                                  (string= (subseq key 0 2) ,prefix))
                         (setf key (subseq key 2))
                        (push ,flag flags)
                        (setf changed t))))
                 (do ((changed t nil)) ((not changed))
                   (find-prefix "C-" :control)
                   (find-prefix "M-" :mod1)
                   (find-prefix "S-" :shift)))
               (list (base-keycode key) (sort flags #'symbol-name<))))
           
           (delete-if (curry #'string= "")
                      (split-sequence:split-sequence #\Space key-seq))))
         (try-find (assoc new-key keymap :test #'equal)))
    (if binding 
        (if try-find
            (setf (cdr try-find) binding)
            (push (cons new-key binding) keymap))
        (when try-find
          (setf keymap (delete new-key keymap :test #'equal :key #'car))))
    keymap))

(defmacro define-key (keymap key-seq binding)
  "KEY-SEQ is a string in format C-x or C-Return or M-period. 
Or S-M-period = M-> from emacs. Keys are for latin keymap. 
Key names from X Window"
  `(setf ,keymap (%define-key ,keymap ,key-seq ,binding)))

(defun seq-processed (seq)
  "If found -- run, return t. if no partial found, return t, both means
sequence processed"
  (let ((res t))
    (mapc (lambda (x)
            (let ((l (length seq)) (lx (length (car x))))
              (cond 
                ((eql l lx)
                 (when (equal (car x) seq)
                 (progn (funcall (cdr x)) (return-from seq-processed t))))
                ((> lx l)
                 (if (equal (subseq (car x) 0 l) seq)
                   (setf res nil))))))
          *global-keymap*)
    res))
   

(defun global-set-key (key-seq binding)
  (define-key (current-global-map) key-seq binding))

(defun keyseq->string (seq)
  (with-output-to-string (s)
    (mapc (lambda (x)
            (destructuring-bind (key flags) x
              (when (member :control flags) (princ "C-" s))
              (when (member :mod1 flags) (princ "M-" s))
              (when (member :shift flags) (princ "S-" s))
              (princ (keyval-name (base-keyval key)) s)
              (princ "-" s)))
          seq)))
