(in-package :cl-user)

(defpackage :clj-arrows-asd
  (:use :cl :asdf))

(in-package :clj-arrows-asd)

(defsystem :clj-arrows
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Implements Clojure-styled threading/transformation macros."
  :components ((:file "package")
               (:file "clj-arrows")))
