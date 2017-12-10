#|
  This file is a part of usuage project.
|#

(defsystem "usuage-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("usuage"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "usuage"))))
  :description "Test system for usuage"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
