#|
  This file is a part of oke project.
  Copyright (c) 2017 Windymelt
|#

(defsystem "oke-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Windymelt"
  :license ""
  :depends-on ("oke"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "oke"))))
  :description "Test system for oke"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
