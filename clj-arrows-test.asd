(in-package :cl-user)

(defpackage :clj-arrows-test-asd
  (:use :cl :asdf))

(in-package :clj-arrows-test-asd)

(defsystem :clj-arrows-test
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Tests for the :clj-arrows package."
  :depends-on (:clj-arrows :fiveam)
  :components ((:file "clj-arrows-test")))
