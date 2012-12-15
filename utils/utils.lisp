(in-package :gtk-cffi-utils)

(defmacro debug-out (&body body)
;  (declare (ignore body))
  `(format t ,@body)
  )

(defmacro memo (place &body body)
  `(or ,place
       (setf ,place ,@body)))

(defun find-key (key seq)
  (when seq
    (if (eq key (car seq)) 
        (list (first seq) (second seq))
        (find-key key (cddr seq)))))

(defmacro with-hash (hash key &body body)
  "If found KEY in HASH, return corresponding value,
else use BODY to calculate the value and save to HASH.
NIL values not saved"
  (let ((try (gensym)))
    `(or (gethash ,key ,hash)
         (let ((,try (progn ,@body)))
           (when ,try
             (setf (gethash ,key ,hash) ,try))))))

(defmacro bitmask (&rest flags)
  "Returns list from lisp values as keywords:
 Example: (bitmask after swapped)
 -> nil, when after=nil and swapped=nil
 -> (:after), when after=t and swapped=nil
 -> (:swapped), when after=nil and swapped=t
 -> (:after :swapped), when both are t"
  `(flatten
    (list ,@(iter
             (for flag in flags)
             (collect `(when ,flag
                         ,(make-keyword flag)))))))

(defmacro template (vars args &body body)
  "Universal template macro. For every ARG in ARGS binded to VARS generates
body. ARGS is list. If VARS also list, then every element in ARGS is
a list of the same length.
  BODY of template should be as of DEFMACRO. 
It should return list (resulting program chunk)."
  (with-gensyms (%do %vars)
    (cond
      ((null vars)
       `(macrolet ((,%do () ,@body))
          (,%do)))
      ((consp vars)
       `(template ,%vars ,args
          (destructuring-bind ,vars ,%vars
            ,@body)))
      (t `(macrolet ((,%do ()
                       `(progn
                          ,@(mapcar (lambda (,vars) ,@body) ',args))))
            (,%do))))))

