(asdf:load-asd "progressons.asd")
(ql:quickload '("progressons" "termp"))

(in-package :progressons)

;; base case
(loop for line in (progressbar (list 1 2 3 4 5)) do (sleep 0.3) (step!))

(terpri)
;; rainbow mode, random colors
;; Only shows progress on Emacs.
(loop for line in (progressbar 60 :rainbow t) do (sleep 0.01) (step!))

(terpri)
;; test with a larger set
(loop for elt in (progressbar (loop for i from 1 to 200 collect i)) do (sleep 0.005) (step!))
(loop for elt in (progressbar (loop for i from 1 to 200 collect i) :rainbow t) do (sleep 0.005) (step!))

(terpri)
;; an ascii bar character
(loop for line in (progressbar (list 1 2 3 4 5) :bar ">") do (sleep 0.3) (step!))

(terpri)
;; rainbow and custom bar together
(loop for line in (progressbar 60 :rainbow t :bar "*") do (sleep 0.01) (step!))


(terpri)
;; (experimetal)
;; make-progress accepts an integer.
(loop for line in (progressbar 40) do (sleep 0.05) (step!))
(let ((*progress* (make-progress 5)))
  (dotimes (i 5)
    do (sleep 0.3)
    (step!)))

(when (termp)
  (uiop:quit))
