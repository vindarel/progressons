
(in-package #:progressons)

;; Are we running inside a real terminal window? Unbound by default.
(defvar *tty-p*)

(defun tty-p (&key force)
  "Return T if we run on a terminal.
  This must fail on Slime (on Emacs' default shell prompt) and succeed on a Lisp in a terminal window."
  (when (and (not force)
             (boundp '*tty-p*))
    (return-from tty-p *tty-p*))
  (let ((test (with-output-to-string (s)
                (uiop:run-program "echo [ -n \"$TERM\" ]" :output s))))
    (setf *tty-p* (not (str:containsp "dumb" test)))))
