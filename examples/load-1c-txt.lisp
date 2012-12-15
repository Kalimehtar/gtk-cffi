(asdf:oos 'asdf:load-op :gtk-cffi)
;(declaim (optimize speed))
(defpackage #:load-1c-text
  (:use #:common-lisp #:gtk-cffi))
(in-package #:load-1c-text)

(gtk-init)

(defparameter *model*
  (make-instance 'list-store :columns '(:string :string :string :boolean)))
(defparameter *window* nil)

(defconstant +space+
  (if (boundp '+space+) +space+
      '(#\Space #\Tab #\Newline)))

(defun empty (str)
  (string=
   (string-trim +space+ str) ""))

(defun load-file (button)
  (declare (ignore button))
  (let ((f (filename (object-by-id :filename))) records)
    (when (string= f "")
      (return-from load-file))
    
    (let ((filename (probe-file f))
          progress-dialog
          progress-bar maxpos)
      (unless filename
        (show-message *window* (format nil "No such file ~s" f)
                      :type :error)
        (return-from load-file))
      (format t "Loading file ~a~%" filename)
      (setf progress-dialog
            (gtk-model
             'window
             :title "Загрузка данных"
             :transient-for *window*
             :win-position :center-on-parent
             :width 400
             :kid (setf progress-bar (make-instance 'progress-bar))))
      (show progress-dialog)
      (with-open-file (s filename
                         #+sbcl :external-format #+sbcl :cp1251
                         #+clisp :external-format #+clisp charset:cp1251)
        (setf maxpos (file-length s))              
        (handler-case 
         (do ((state :begin)
              (text "")
              (code "")
              (descr "")
              (str (read-line s) (read-line s))) (nil)
           (macrolet
               ((add-text (place str)
                          `(unless (empty ,str)
                            (setf ,place
                                  (concatenate
                                   'string
                                   ,place
                                   (format nil "~%")
                                   (string-right-trim +space+ ,str))))))
             (labels
                 ((prefix (str prefstr)
                          (and (> (length str) (length prefstr))
                               (string= (subseq str 0 (length prefstr))
                                        prefstr)))
                  (sep (str)
                       (prefix str "------"))

                  (get-code (str)
                            (string-trim +space+
                                         (subseq str (1+ (position #\: str)))))
                
                  (save ()
                        (setf (fraction progress-bar)
                              (/ (file-position s) maxpos))
                        (yield)
                        (push (list code descr
                                    text (if (empty text) nil t)) records)
                        (setf code ""
                              descr ""
                              text "")))
               ;(format t "~A ~A ~A~%" state (sep str) str)
               (setf
                state
                (case state
                  (:begin (if (sep str) :code :begin))
                  (:code (if (prefix str "Код ошибки")
                             (progn
                               (setf code (get-code str)) :waittext)
                           :code))
                  (:waittext (if (prefix str "Описание") :descr :waittext))
                  (:descr (if (empty str) :text
                            (progn (add-text descr str) :descr)))
                  (:text (if (sep str) (progn (save) :code)
                           (progn (add-text text str) :text))))))))
           (end-of-file () nil)))
      (clear *model*)
      (mapc (lambda (row) (append-values *model* row)) records)
      (destroy progress-dialog))))

(defun select-file (button)
  (declare (ignore button))
  (let ((d (make-instance 'file-chooser-dialog
                          :action :open
                          :parent *window*
                          :title "Выберите файл ошибок 1С")))
    (setf (filename d) (text (object-by-id :filename)))
    (when (eq (run d :keep-alive t) :accept)
        (setf (text (object-by-id :filename)) (filename d)))
    (destroy d)))

;(import 'gtk-cffi::expand)
(setf *window*
  (gtk-model
   'window :width 800
           :height 600
           :title "Загрузка из 1С"
           :signals '(:destroy :gtk-main-quit)
   ('v-box
    :expand nil
    :padding 5
;;     ('h-box
;;      :expand nil
;;      :padding 10
;;      ('label :text "Имя файла")
;;      :expand t
;;      ('entry :id :filename)
;;              nil
;;      :padding 0
;;      ('button :label "gtk-open"
;;               :type :stock :signals (list :clicked #'select-file))
;;      ('button :label "Загрузить" :signals (list :clicked #'load-file)))
    ('file-chooser-button :title "Файл ошибок 1С" :action :open
                          :signals (list :file-set #'load-file)
                          :id :filename)
    :expand t
    ('v-paned :vexpand t
     ('scrolled-window
      ('tree-view :model *model*
                  :columns (list "Код ошибки" "Текст"
                                 (list :title "Подробности"
                                  :cell (make-instance 'cell-renderer-toggle)
                                  :active 3))
                  :id :tree-view
                  :on-select (lambda (model iter &rest rest)
                               (declare (ignore rest))
                               (setf (text (buffer (object-by-id :text)))
                                     (car (model-values model
                                                        :iter iter :col 2))))))
     ('scrolled-window :vexpand t
      ('text-view :id :text :vexpand t))))))
            

(show *window* :all t)
(gtk-main)

