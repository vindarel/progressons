
(require "asdf")
(asdf:defsystem "progressons"
  :version "0.1"
  :author "vindarel"
  :license "MIT"
  :depends-on ("str")
  :components ((:file "packages")
               (:file "utils")
               (:file "progressons"))

  :description "Display a progress bar on one line."
  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  ;; :in-order-to ((test-op (test-op "progressons-test")))
  )
