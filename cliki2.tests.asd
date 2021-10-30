;;; -*- lisp -*-

(defsystem :cliki2.tests
  :license "AGPL-3.0-or-later"
  :description "Test for CLiki2"
  :components ((:module :tests
                        :serial t
                        :components ((:file "package")
                                     (:file "tests"))))
  :depends-on (:cliki2 :fiveam))
