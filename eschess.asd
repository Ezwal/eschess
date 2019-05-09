(defsystem "eschess"
  :version "0.3.0"
  :author "Ezwal"
  :license ""
  :depends-on ("array-operations")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "piece")
                 (:file "graphical"))))
  :description "Chess engine"
  :in-order-to ((test-op (test-op "eschess-project/tests"))))

(defsystem "eschess/tests"
  :author ""
  :license ""
  :depends-on ("eschess"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for eschess"

  :perform (test-op (op c) (symbol-call :rove :run c)))
