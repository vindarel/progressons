(load "progressons.asd")
(ql:quickload '("progressons" "termp"))

(in-package :progressons)
(loop for line in (progressbar (list 1 2 3 4 5)) do (sleep 0.3) (step!))

(loop for line in (progressbar 60 :rainbow t) do (sleep 0.01) (step!))

(loop for elt in (progressbar (loop for i from 1 to 200 collect i)) do (sleep 0.005) (step!))

;; (experimetal)
;; make-progress accepts an integer.
;; It creates a list of that length. Avoid a very big number.
(let ((*progress* (make-progress 5)))
  (dotimes (i 5)
    do (sleep 0.3)
    (step!)))

(when (termp)
  (uiop:quit))
