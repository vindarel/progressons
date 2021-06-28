(load "progressons.asd")
(ql:quickload '("progressons" "termp"))

(in-package :progressons)
(loop for line in (progressbar (list 1 2 3 4 5)) do (sleep 0.3) (step!))

(loop for elt in (progressbar (loop for i from 1 to 200 collect i)) do (sleep 0.005) (step!))

(when (termp:termp)
  (termp:quit))
