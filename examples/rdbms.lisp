(asdf:oos 'asdf:load-op :cl-rdbms)
(asdf:oos 'asdf:load-op :gtk-cffi)

(defpackage :test
  (:use #:common-lisp #:gtk-cffi #:cffi-object #:g-object-cffi #:cl-rdbms)
  (:shadowing-import-from #:gtk-cffi #:column-type))

(in-package :test)

(defvar *database* 
  (make-instance 'postgresql-postmodern 
                 :connection-specification '(:database "monk" 
                                             :user-name "monk" 
                                             :password "")))
;(enable-sql-syntax)


(gtk-init)

(defvar *model*
  (make-instance 'list-store :columns '(:string :string 
                                        :string :string :uint)))

(defun process (row)
   (list 
    (local-time:format-timestring nil 
                                  (aref row 0) 
                                  :format local-time:+asctime-format+)
    (case (aref row 1)
      (6 "TCP")
      (17 "UDP")
      (t (format nil "~a" (aref row 1))))
    (format nil "~a:~a" (aref row 2) (aref row 3))
    (format nil "~a:~a" (aref row 4) (aref row 5))
    (aref row 6)))

;(map nil (lambda (row)
;           (append-values *model* (process row)))
;     (with-transaction (execute (sql (select * traffic)))))
          


(defvar *window*
  (gtk-model
   'window :width 800
           :height 600
           :title "Траффик"
           :signals '(:destroy :gtk-main-quit)
   ('scrolled-window
      ('tree-view :id :tree :model *model*
                  :columns '("Дата" "Протокол" 
                             "Источник" "Приемник" "Размер")))))

(setf (gsignal *window* :show)
      (lambda (&rest rest)
        (declare (ignore rest))
        (format t "Realized~%")
        (let* (progress-bar
               (progress-dialog
                (gtk-model
                 'window
                 :title "Загрузка данных"
                 :transient-for *window*
                 :win-position :center-on-parent
                 :width 400
                 :kid (setf progress-bar (make-instance 'progress-bar)))))
          (show progress-dialog)
          (setf (model (object-by-id :tree)) nil)
          (let*
              ((table (with-transaction (execute (sql (select * traffic)))))
               (ltable (- (length table) 1))
               (table2 (loop 
                          :for i :from 0 :to ltable
                          :collecting (process (aref table i))))
               (i 0) (pos 0))
            (mapcar 
             (lambda (x)
               (when (> (incf i) 1024)
                 (setf i 0
                       (fraction progress-bar)
                       (/ (incf pos 1024) ltable))
                 (yield))
               (append-values *model* x))
             table2))   
          (setf (model (object-by-id :tree)) *model*)
          (destroy progress-dialog))))
;;       (gsignal *window* :realize)
;;       (lambda (&rest rest)
;;         (declare (ignore rest))
;;         (format t "Realized~%")))

        

(show *window*)

(gtk-main)



