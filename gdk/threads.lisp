(in-package :gdk-cffi)

(defcfun "gdk_threads_enter" :void)
(defcfun "gdk_threads_leave" :void)

(defmacro with-threads (&rest body)
  `(unwind-protect
	(progn
	  (gdk-threads-enter)
	  ,@body)
     (gdk-threads-leave)))