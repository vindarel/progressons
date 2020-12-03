
(in-package #:progressons)

;; Are we running inside a real terminal window? Unbound by default.
(defvar *tty-p*)

(defun tty-p (&key force)
  "Return T if we run on a terminal.
  This must fail on Slime (on Emacs' default shell prompt) and succeed on a Lisp in a terminal window."
  (if (and (not force)
           (boundp '*tty-p*))
      *tty-p*
      (setf *tty-p* (not (equalp "dumb" (uiop:getenv "TERM"))))))
