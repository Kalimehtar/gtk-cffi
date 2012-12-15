(in-package :gtk-cffi-ext)

(defmacro with-progress ((&key parent (title "") (width 400)) &body body)
  (let ((widget-var (gensym))
        (progress-var (gensym)))
    `(let* ((,progress-var (make-instance 'progress-bar))
            (,widget-var
             (gtk-model
               'window
               :title ,title
               :transient-for ,parent
               :position-type :center-on-parent
               :width ,width
               :kid ,progress-var)))
       (flet ((set-progress (frac) 
                (setf (fraction ,progress-var) frac)
                (draw ,progress-var)))
         (show ,widget-var)
         ,@body
         (destroy ,widget-var)))))
      