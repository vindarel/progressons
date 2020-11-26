(defpackage progressons
  (:use :cl)
  (:export :progressbar
           :step!))

#|
Usage:

(loop for line in (progressbar (list 1 2 3 4 5))
   do (format t "~&")
      (step!))
|#

(in-package #:progressons)

(defparameter *progress* nil
  "the current progress bar.")

(defclass progress ()
  ((data :accessor progress-data
         :initarg :data)
   (width :accessor progress-width
          :initform 80
          :documentation "Screen width.")
   (step-width :accessor step-width
               :documentation "length (width, characters count) of each step to print.")
   (percent-width :accessor percent-width
                  :documentation "The percent to increment each step.")
   (steps-counter :accessor steps-counter
                  :initform 0
                  :documentation "A counter to increment each time we increase the bar.")
   (finished :accessor progress-finished
             :initform nil
             :documentation "Boolean. T after 100% of completion.")))

(defmethod progress-length ((obj progress))
  (length (progress-data obj)))

(defun make-progress (data)
   (make-instance 'progress :data data))

(defmethod initialize-instance :after ((obj progress) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (with-slots (step-width percent-width) obj
    (setf step-width (/ (progress-width obj)
                        (progress-length obj)))
    (setf percent-width (/ 100
                        (progress-length obj)))))

(defmethod print-object ((obj progress) stream)
  (print-unreadable-object (obj stream)
    (format stream "PROGRESS BAR, length ~a, step ~a"
            (progress-length obj)
            (step-width obj))))

(defmethod progress-percent ((obj progress))
  (/ (* (percent-width obj) (steps-counter obj))
     100))

(defmethod progress-finished ((obj progress))
  (>= (steps-counter obj)
      (progress-length obj)))

(defmethod step ((obj progress))
  (unless (progress-finished obj)
    (incf (steps-counter obj)))
  (print-step obj)
  (values (steps-counter obj)
          (progress-percent obj)
          (progress-finished obj)))

(defun progress ()
  "Only works on the terminal."
  (loop for percent in (progress-data *progress*) do
       (format t "~a~a[~a]"
               (make-string percent :initial-element #\>)
               (make-string (- 80 percent) :initial-element #\ )
               percent)
       (write-char #\return)
       (force-output)
       (sleep 0.2))
  (write-char #\newline))

(defmethod current-width ((obj progress))
  "Rounded to nearest integer."
  (round (* (steps-counter obj)
            (step-width obj))))

(defmethod current-percent ((obj progress))
  "Rounded"
  (round (* (steps-counter obj)
            (percent-width obj))))

(defmethod reinit ((obj progress))
  (setf (steps-counter 0)))

(defmethod print-step ((obj progress) &key (stream t))
  "Print the bar at the right length."
  ;; composed of the bar indicator + whitespace + percent indicator.
  (format stream "~a~a[~a]"
          (make-string (current-width obj)
                       :initial-element #\>)
          (make-string (- (progress-width obj)
                          (current-width obj))
                       :initial-element #\ )
          (current-percent obj)))

(defun progressbar (data)
  (loop for elt in (progress-data (make-progress data))
     do (sleep 0.1)
     finally (print-step *progress*)))
