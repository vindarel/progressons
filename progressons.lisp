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
             :documentation "Boolean. T after 100% of completion.")
   ;; Style, preferences.
   (fill-character :accessor progress-fill-character
                   :initarg :fill-character
                   :initform #\FULL_BLOCK)
   (default-fill-character :accessor default-fill-character
     :allocation :class
     :initform #\FULL_BLOCK)            ; = █
   (rainbow :accessor progress-rainbow
            :initarg :rainbow
            :initform nil
            :documentation "If non-nil, print each step in a different color. It uses the dumb print method (it doesn't update the progress percent at the beginning of the line).")
   ))

(defparameter *rainbow-steps* nil
  "If non-nil, print each step character in a random color.

You should rather create a progressbar with this preference enabled:

   (progressbar :rainbow t)")

(defmethod progress-length ((obj progress))
  (length (progress-data obj)))

(defun make-progress (data &key fill-character rainbow)
  "A more manual way to create a progressbar than `progressbar'.

Experimental: if DATA is an integer, it creates a list of that length with `make-list'."
  (typecase data
    (integer
     (setf *progress* (make-instance 'progress
                                     :data (make-list data :initial-element #\0)
                                     :fill-character fill-character
                                     :rainbow rainbow)))
    (t
     (setf *progress* (make-instance 'progress :data data
                                     :fill-character fill-character
                                     :rainbow rainbow)))))

(defmethod initialize-instance :after ((obj progress) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (with-slots (step-width percent-width fill-character) obj
    (unless fill-character
      ;; mmh not the best way to accept a key param on the constructor and ensure it's set here.
      (setf fill-character (default-fill-character obj)))
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
    (cond
      ((or (progress-rainbow obj)
           *rainbow-steps*)
       (print-rainbow-step obj))
      ((tty-p)
       (print-step obj))
      (t
       (print-step-dumb obj)))
    ;; (if (tty-p)
    ;;     (print-step obj)
    ;;     (print-step-dumb obj))

  (values (steps-counter obj)
          (progress-percent obj)
          (progress-finished obj))))

(defmethod print-opening ((obj progress) &key (stream t))
  (when (and (not (tty-p))
             (zerop (steps-counter obj)))
    (format stream "[0/~a]" (progress-length obj))))

(defun make-string-with-random-color (width &key (initial-element #\FULL_BLOCK) print!)
  "Return a string with a random color (amongst the 8 basic ones)."
  (let* ((nb-colors 8)
         (colors cl-ansi-text::+cl-colors-basic-colors+)
         ;; see all X11 colors in cl-colors2:*x11-colors-list*
         (color (elt colors (random nb-colors)))
         (result (with-output-to-string (s)
                   (cl-ansi-text:with-color (color :stream s)
                     (format s "~a" (make-string
                                     width
                                     :initial-element initial-element))))))
    (if print!
      (print result)
      result)))

(let ((sub-steps-counter 0)
      sub-steps-for-one-progress)
  ;; If we have more than a hundred elements,
  ;; we can't print a step of size < 1.
  ;; We count how many items are required to print one progress character.
  ;; We don't have this limitation on a real terminal.

  (defmethod print-step-dumb ((obj progress) &key (stream t))
    "Print the progress step in a dumb terminal (like Emacs Slime).
We can't erase a line (it prints the ^M character instead), so we can't update
the percentage and the ratio of done items. We print progress indicators in a row,
one after the other, and we print the percent in the end."
    (if (> (step-width obj) 1)
        ;; easy case, the number of elements we have to draw the progress for
        ;; is smaller than 100, our line size.
        (format stream "~a"
                (if *rainbow-steps*
                    ;; random color…
                    (make-string-with-random-color
                     (floor (step-width obj)) ;; a step of 4/3 -> 1
                     :initial-element (progress-fill-character obj))
                    ;; normal mode.
                    (make-string
                     (floor (step-width obj))
                     :initial-element (progress-fill-character obj))))
        ;; The elements to draw progress for are more than 100 and our terminal is dubm:
        ;; we can't print a tiny step of width less than a character, so we print a progress
        ;; character every n step.
        (progn
          (unless sub-steps-for-one-progress
            ;; if step-width is 80/1100, we'll increment a counter
            ;; until it reaches 1100/80 = 13.75 rounded to 14, and we print one progress char.
            (setf sub-steps-for-one-progress (round (/ 1 (step-width obj)))))
          (if (>= sub-steps-counter sub-steps-for-one-progress)
              (progn
                (format stream "~a"
                        (if *rainbow-steps*
                            (make-string-with-random-color
                             1
                             :initial-element (progress-fill-character obj))
                            (progress-fill-character obj)))
                (setf sub-steps-counter 0))
              (incf sub-steps-counter))))
    (when (progress-finished obj)
      (format stream "[100%]~&")
      (setf sub-steps-counter 0
            sub-steps-for-one-progress nil))))

(defmethod print-rainbow-step ((obj progress) &key (stream t))
  "Print each bar in a different color."
  (let ((*rainbow-steps* t))
    (print-step-dumb obj :stream stream)))

(defmethod print-step ((obj progress) &key (stream t))
  "Print the bar at the right length."
  ;; composed of the bar indicator + whitespace + percent indicator.
  (let ((data-digits (length (format nil "~a" (progress-length obj)))))
    (format stream "[~v@a/~v@a]~a~a[~3@a%]"
            data-digits
            (steps-counter obj)
            data-digits
            (progress-length obj)
            (make-string (current-width obj)
                         :initial-element (progress-fill-character obj))
            (make-string (- (progress-width obj)
                            (current-width obj))
                         :initial-element #\-)
            (current-percent obj))

    (if (and (tty-p)
             (not (progress-finished obj)))
        ;; Only erase the line on real terminals.
        ;; Otherwise, Emacs' output is messy
        ;; (it prints ^M without erasing the line).
        ;; We could handle the whole progress bar differently, like printing only the steps in a row, without the %.
        (write-char #\Return)
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

(defun progressbar (data &key bar rainbow)
  "Create a progress bar with this data. Return the data, so we can iterate over it.
At the end of each iteration, you must call (step!) to print the progress.

If `rainbow' is non-nil, print the steps in a random color."
  (setf *tty-p* (tty-p))
  (make-progress data
                 :rainbow rainbow
                 :fill-character (if (stringp bar)
                                     (character bar)
                                     bar))
  (values (progress-data *progress*)
          *progress*))
