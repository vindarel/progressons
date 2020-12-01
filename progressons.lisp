#|
Usage:

(loop for line in (progressbar (list 1 2 3 4 5))
   do (sleep 0.1)
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
  (setf *progress* (make-instance 'progress :data data)))

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

(defun step! (&optional (obj *progress*))
  "Increment and print the bar."
  (print-opening obj)
  (unless (progress-finished obj)
    (incf (steps-counter obj))

    ;; print a new step:
    ;; erase the last one, print anew.
    ;; (print-step obj)
    (if (tty-p)
        (print-step obj)
        (print-step-dumb obj))

  (values (steps-counter obj)
          (progress-percent obj)
          (progress-finished obj))))

(defmethod print-opening ((obj progress) &key (stream t))
  (when (and (not (tty-p))
             (zerop (steps-counter obj)))
    (format stream "[0/~a]" (progress-length obj))))


(defmethod print-step-dumb ((obj progress) &key (stream t))
  (format stream "~a" (make-string
                       ;; If we have more than a hundred elements,
                       ;; we can't print a step of size < 1.
                       ;; We'll draw one character, even if we can't respect the 100 characters line limit.
                       ;; We don't have this limitation on a real terminal.
                       (or (plusp (round (step-width obj)))
                           1)
                       :initial-element #\>))
  (when (progress-finished obj)
    (format stream "[100%]~&")))

(defmethod print-step ((obj progress) &key (stream t))
  "Print the bar at the right length."
  ;; composed of the bar indicator + whitespace + percent indicator.
  (let ((data-digits (length (format nil "~a" (progressons::progress-length obj)))))
    (format stream "[~v@a/~v@a]~a~a[~3@a%]"
            data-digits
            (progressons::steps-counter obj)
            data-digits
            (progressons::progress-length obj)
            (make-string (current-width obj)
                         :initial-element #\>)
            (make-string (- (progress-width obj)
                            (current-width obj))
                         :initial-element #\ )
            (current-percent obj))

    (if (tty-p)
        ;; Only erase the line on real terminals.
        ;; Otherwise, Emacs' output is messy
        ;; (it prints ^M without erasing the line).
        ;; We could handle the whole progress bar differently, like printing only the steps in a row, without the %.
        (write-char #\return)
        ;; if we don't print Return, print a new line.
        (terpri))
    (force-output)))

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
  (setf (steps-counter obj) 0))

(defun progressbar (data)
  "Create a progress bar with this data. Return the data, so we can iterate over it.
  At the end of each iteration, you must call (step!) to print the progress."
  (setf *tty-p* (tty-p))
  (make-progress data)
  (values data
          *progress*))
