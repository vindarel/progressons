(defpackage :progressons
  (:use :cl)
  (:export :progressbar
           :make-progress
           :step!
           :*default-width*
           :*default-fill-character*
	   :*default-background-character*))
