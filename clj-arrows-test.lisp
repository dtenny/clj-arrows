(in-package :cl-user)

(defpackage :clj-arrows-test
  (:use :cl :clj-arrows :fiveam)
  (:export #:run-tests)
  (:documentation "Tests for the :clj-arrows package."))

(in-package :clj-arrows-test)
(def-suite test-suite :description ":clj-arrows tests")
(in-suite test-suite)

;;; With some tests borrowed from "https://gitlab.com/Harleqin/arrows", 
;;; thanks to the author and his use of CC0 license.

(test ->
  (is (= (-> 3 /) 1/3))
  (is (= (-> 3 (/)) 1/3))
  (is (= (-> 3 (/ 2)) 3/2))
  (is (= (-> 3 (/ 2) /) 2/3)))

(test ->>
  (is (= (->> 3 /) 1/3))
  (is (= (->> 3 (/)) 1/3))
  (is (= (->> 3 (/ 2)) 2/3))
  (is (= (->> 3 (/ 2) /) 3/2)))

(test as->
  (is (= 3 (as-> 3 x)))
  (is (= (as-> 3 $
               (* 5 $)
               (/ $ 7))
         15/7))
  (is (= (as-> 0 n
               (1+ n)
               (1+ n))
         2)))

(test cond->
  (is (= 3 (cond-> 3)))
  (is (= 6 (cond-> 1
             t incf
             nil (* 42)
             (= 2 2) (* 3))))
  (labels ((divisible-by? (divisor number)
             (zerop (mod number divisor)))
           (str (&rest args)
             (apply #'concatenate 'string 
                    (mapcar (lambda (exp)
                              (if (null exp) 
                                  ""
                                  (princ-to-string exp)))
                            args)))
           (say (n)
             (cond-> nil
               (divisible-by? 3 n) (str "Fizz")
               (divisible-by? 5 n) (str "Buzz")
               :always             (or (str n)))))
    (is (string= "1" (say 1)))
    (is (string= "Fizz" (say 3)))
    (is (string= "Buzz" (say 5)))
    (is (string= "FizzBuzz" (say 15))))

  (is (= 2
         (labels ((f (x) (declare (ignore x)) 1))
           ;; cond-> doesn't short circuit on nil, f is transformed to function call
           (cond-> nil
             t   f
             nil incf
             t   incf))))

  (is (= 0 (cond-> 0 t (prog1 1)))))

(test cond->>
  (is (= 3 (cond->> 3)))
  (is (= 3 (cond->> 1
             t /
             nil (/ 2)
             (= 2 2) (/ 3))))
  (is (equalp '(3 1)
              (cond->> nil
                (oddp 1) (cons 1)
                (oddp 2) (cons 2)
                (oddp 3) (cons 3)))))

(test some->
  (is (= 3 (some-> 3)))
  ;; (getf plist key)
  (is (= 3 (some-> '(:a 1 :b 2) (getf :b) 1+)))
  (is (null (some-> '(:a 1 :b 2) (getf :c) 1+))))

(test some->>
  (is (= 3 (some->> 3)))
  ;; (assoc key alist)
  (is (= 3 (some->> '((:a . 1) (:b . 2)) (assoc :b) cdr 1+)))
  (is (null (some->> '((:a . 1) (:b . 2)) (assoc :c) cdr 1+))))

(test -<>
  (is (= 3 (-<> 3)))
  (is (equalp '(1 2 3) (-<> 1 (list 2 3))))
  (is (equalp '(2 1 3) (-<> 1 (list 2 <> 3))))
  (is (equalp '(2 1 1) (-<> 1 (list 2 <> <>))))
  (let ((counter 0))
    (flet ((foo () (incf counter) 5))
      (is (equalp '(3 (1 5 5 2) (1 5 5 2) 4)
                  (-<> (foo) (list 1 <> <> 2) (list 3 <> <> 4))))
      (is (= counter 1)))))

(test -<>>
  (is (= 3 (-<>> 3)))
  (is (equalp '(2 3 1) (-<>> 1 (list 2 3))))
  (is (equalp '(2 1 3) (-<>> 1 (list 2 <> 3))))
  (is (equalp '(2 1 1) (-<>> 1 (list 2 <> <>))))
  (let ((counter 0))
    (flet ((foo () (incf counter) 5))
      (is (equalp '(3 (1 5 5 2) (1 5 5 2) 4)
                  (-<>> (foo) (list 1 <> <> 2) (list 3 <> <> 4))))
      (is (= counter 1)))))

(test some-<>
  (is (= 3 (some-<> 3)))
  ;; (getf plist key)
  (is (= 3 (some-<> '(:a 1 :b 2) (getf :b) 1+)))
  (is (null (some-<> '(:a 1 :b 2) (getf :c) 1+)))
  ;; (assoc key alist)
  (is (= 3 (some-<> '((:a . 1) (:b . 2)) (assoc :b <>) cdr 1+)))
  (is (null (some-<> '((:a . 1) (:b . 2)) (assoc :c <>) cdr 1+)))

  (is (equalp '((5 1 2) 3 4)
              (some-<> 5 (list 1 2) (list 3 4))))

  (let ((counter 0))
    (flet ((foo () (incf counter) 5))
      (is (equalp '(3 (1 5 5 2) (1 5 5 2) 4)
                  (some-<> (foo) (list 1 <> <> 2) (list 3 <> <> 4))))
      (is (= counter 1)))))

(test some-<>>
  (is (= 3 (some-<>> 3)))
  ;; (getf plist key)
  (is (= 3 (some-<>> '(:a 1 :b 2) (getf <> :b) 1+)))
  (is (null (some-<>> '(:a 1 :b 2) (getf <> :c) 1+)))
  ;; (assoc key alist)
  (is (= 3 (some-<>> '((:a . 1) (:b . 2)) (assoc :b) cdr 1+)))
  (is (null (some-<>> '((:a . 1) (:b . 2)) (assoc :c) cdr 1+)))

  (is (equalp '(3 4 (1 2 5))
              (some-<>> 5 (list 1 2) (list 3 4))))

  (let ((counter 0))
    (flet ((foo () (incf counter) 5))
      (is (equalp '(3 (1 5 5 2) (1 5 5 2) 4)
                  (some-<>> (foo) (list 1 <> <> 2) (list 3 <> <> 4))))
      (is (= counter 1)))))

(test diamonds-from-another-package
  ;; Make sure `<>` references in diamond macros resolve correctly in a package
  ;; that does not :USE the :CLJ-ARROWS package.  For now we assume CL-USER doesn't
  ;; :USE :CLJ-ARROWS.  If it does because someone changed it, these could be false 
  ;; positive test results.
  (intern "<>" :cl-user)
  (is (equalp '(2 1 3) (-<> 1 (list 2 cl-user::<> 3))))
  (is (equalp '(2 1 3) (-<>> 1 (list 2 cl-user::<> 3))))
  (is (= 3 (some-<> '((:a . 1) (:b . 2)) (assoc :b cl-user::<>) cdr 1+)))
  (is (= 3 (some-<>> '(:a 1 :b 2) (getf cl-user::<> :b) 1+))))

(defun run-tests ()
  "Run all :clj-arrows tests."
  (explain! (run 'test-suite)))
