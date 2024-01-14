(in-package :cl-user)

(defpackage :clj-arrows
  (:use :cl)
  (:export
   
   ;; Clojure compatible macros

   #:->
   #:->>
   #:as->
   #:cond->
   #:cond->>
   #:some->
   #:some->>

   ;; Additional macros not in clojure at the time of this writing.

   #:-<>
   #:-<>>
   #:some-<>
   #:some-<>>
   )

  (:documentation
   "Arrow macros that follow clojure as closely as possible. Also adds some basic diamond
wand macros because I just couldn't resist the temptation."))


