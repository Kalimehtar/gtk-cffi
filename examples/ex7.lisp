(proclaim '(optimize debug))
(asdf:oos 'asdf:load-op :gtk-cffi)
(defpackage #:ex7
  (:use #:common-lisp #:gtk-cffi #:g-object-cffi))
(in-package #:ex7)

(defvar *window*)
(defvar *cell-pix*)

(defun main ()
  (declare (optimize debug safety))
  (gtk-init)
;;   (rc-parse-string "style \"my\" {
;; GtkTreeView::even-row-color = \"#E7EDF6\"
;; GtkTreeView::odd-row-color = \"#FBFBFB\"
;; }
;; widget \"*\" style \"my\"")
        
  (let ((window (make-instance 'window :width 400 :height 280)))
    (setf (gsignal window :destroy) :gtk-main-quit)
    (setf *window* window)
    
    (let ((v-box (make-instance 'v-box))
          (data '(("01-01-08" "Some event")
                  ("10-02-08" "Another event withe very long description.
Description of this event. And this description is very long")
                  ("15-02-08" "Чуть-чуть напишем по-русски"))))
      (add window v-box)

      (let ((title (make-instance 'label :text "Use of GtkCellEditable")))
        (setf (font title) "Times New Roman Italic 12"
              (color title) "#0000ff"
              (color title :type :bg) "#ff0000")
        ;(setf (size-request title) '(-1 40))
        (pack v-box title))

      (let* ((model (make-instance 'list-store :columns
                                   '(:string :string)))
             (frame (make-instance 'frame))
             (view (make-instance 'tree-view :model model)))
        (setf (color view :state :selected) "#ff0000") 
        (pack v-box frame :pack-fill nil :expand t)
        (pack v-box (make-instance 'label) :pack-fill t :expand t)
        (add frame view)
        (let ((field-header '("Date" "Event")))
          (loop :for col :from 0 :below (length field-header) :do
                (let* ((cell-renderer (make-instance 'cell-renderer-text))
                       (column (make-instance 'tree-view-column
                                         :title (nth col field-header);""
                                         :cell cell-renderer
                                         :attributes `(:text ,col))))
                  (let ((label (make-instance 'label
                                              :text (nth col field-header))))
                    (setf (font label) "Arial")
                                        ;(setf (color label) "#666666")
                    (setf (widget column) label) (show label))
                  
                  (when (= col 1)
                    (setf (property cell-renderer :editable) t)
                    (setf *cell-pix* (make-instance 'cell-renderer-pixbuf))
                    (pack column *cell-pix*)
                    (setf (property *cell-pix* :pixbuf)
                          (make-instance 'gdk-cffi:pixbuf
                                         :file "list.png")))

                  (setf (gsignal cell-renderer :edited)
                        (let ((%col col))
                          (lambda (cell path new-text)
                            (declare (ignore cell))
                            (format t "path: ~a new-text:~a~%" path new-text)
                            (path->iter model path)
                            (setf (model-values model :column %col)
                                  (list new-text)))))

                  (append-column view column))))

        (setf
         (gsignal view :button-press-event)
         (lambda (view event)
           (when (and (eq (gdk-cffi:get-slot event :type) :button-press)
                      (= (the integer (gdk-cffi:get-slot event :button)) 1))
             (on-click view
                       (path-at-pos view
                                    (round (gdk-cffi:get-slot event :x))
                                    (round (gdk-cffi:get-slot event :y))))))
         (gsignal view :cursor-changed)
         (lambda (view)
           (set-bold view (second (get-cursor view)))))
        
        (setf (property view :enable-grid-lines) :both
              (property view :rules-hint) t)
        (loop :for row :below (length data) :do
              (let ((values (nth row data)))
                (append-values model values)))))
    (show window :all t)
    (gtk-main)))

(defun set-bold (view column)
  (format t "set ~A~%" column)
  (loop :for col :in (columns view)
        :for i :from 0 :to (length (columns view))
        :do (progn
              (setf (font (widget col))
                    (if (equal col column)
                        "Arial Bold" "Arial"))
              (when (equal (column view i) column)
                (setf (search-column view) i)))))

(defun on-click (view path-list)
  (format t "on-click: ~a~%" path-list)
  (when path-list
    (destructuring-bind (path column x y) path-list
      (declare (ignore y))
      (let ((cell (get-cell-at column x)))  
        (format t "cell: ~A~%" cell)
        (when (equal cell *cell-pix*)
          (let ((dialog (make-instance 'dialog :title "Edit text"
                                       :parent *window*
                                       :buttons '((:gtk-ok :ok)
                                                  (:gtk-cancel :cancel)))))
            (let ((text-view (make-instance 'text-view))
                  (iter (path->iter (model view) path)))
              (setf (text (buffer text-view))
                    (car (model-values (model view) :columns '(1) 
                                       :tree-iter iter)))
              (let ((top-area (content-area dialog)))
                (pack top-area text-view :pack-fill t :expand t)
                (show text-view)) 
              (setf (position-type dialog) :center-on-parent)
              
                                        ;(pack top-area text-view :fill t :expand t))
              (run dialog)
              (setf (model-values (model view) :columns '(1) :tree-iter iter)
                    (list (text (buffer text-view))))
              (destroy dialog))))))))
                                                        
(main)
        
        
             