(in-package :clj-arrows)

(defmacro -> (x &rest forms)
  "Threads the expr through the forms. Inserts x as the
second item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
second item in second form, etc.

E.g. `(-> 3 (/ 2) /)`
expands to `(/ (/ 3 2))`
=> 2/3"
  (reduce (lambda (x form)
            (if (listp form)
                `(,(first form) ,x ,@(rest form))
                (list form x)))
          forms :initial-value x))

(defmacro ->> (x &rest forms)
  "Threads the expr through the forms. Inserts x as the
last item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
last item in second form, etc.

E.g. `(->> 3 (/ 2) /)`
expands to `(/ (/ 2 3))`
=> 3/2"
  (reduce (lambda (x form)
            (if (listp form)
                `(,@form ,x)
                (list form x)))
          forms :initial-value x))

(defmacro as-> (expr name &rest forms)
  "Binds name to expr, evaluates the first form in the lexical context
of that binding, then binds name to that result, repeating for each
successive form, returning the result of the last form.

E.g. `(as-> 3 $ (* 5 $) (/ $ 7))`
expands to 
```
(LET* (($ 3)
       ($ (* 5 $)))
  (/ $ 7))
```
=> 15/7"
  `(let* ((,name ,expr)
          ,@(mapcar (lambda (form)
                      (list name form))
                    (butlast forms)))
     ,(if (null forms)
          name
          (first (last forms)))))

(defun cond-macro (threading expr clauses)
  "Refactored cond-> logic, varies only by threading semantics.
  THREADING is the symbol for either '-> or '->>'."
  (assert (evenp (length clauses)))
  (let* ((g (gensym))
         (steps (mapcar (lambda (pair)
                          (destructuring-bind (test step) pair
                            `(if ,test (,threading ,g ,step) ,g)))
                        (loop for sublist on clauses by #'cddr
                              collect (subseq sublist 0 2)))))
    `(let* ((,g ,expr)
            ,@(mapcar (lambda (step) (list g step)) (butlast steps)))
       ,(if (null steps)
            g
            (first (last steps))))))

(defmacro cond-> (expr &rest clauses)
  "Takes an expression and a set of test/form pairs. Threads expr (via ->)
through each form for which the corresponding test
expression is true. Note that, unlike cond branching, cond-> threading does
not short circuit after the first true test expression.

Note that the substitution is never in the tests, only the expression executed
if the test was true.   Also note that this macro, adopting Clojure's namesake,
does not have an additional set of parenthesis around the `test expr` pairs, you
may need to do a PROGN if you want to evaluate multiple expressions.

E.g. 
```
(cond-> 1
  t incf
  nil (* 42)
  (= 2 2) (* 3))
```
expands to 
```
(LET* ((#:G1492 1)
       (#:G1492
        (IF T
            (-> #:G1492 INCF)
            #:G1492))
       (#:G1492
        (IF NIL
            (-> #:G1492 (* 42))
            #:G1492)))
  (IF (= 2 2)
      (-> #:G1492 (* 3))
      #:G1492))
```
=> 6"
  (cond-macro '-> expr clauses))

(defmacro cond->> (expr &rest clauses)
  "Takes an expression and a set of test/form pairs. Threads expr (via ->>)
through each form for which the corresponding test expression
is true.  Note that, unlike cond branching, cond->> threading does not short circuit
after the first true test expression.

Note that the substitution is never in the tests, the threading only occurs
only the expression executed if the test was true.  Also note that this macro,
adopting Clojure's namesake, does not have an additional set of parenthesis
around the `test expr` pairs, you may need to do a PROGN if you want to
evaluate multiple expressions.

E.g.
```
(cond->> nil
  (oddp 1) (cons 1)
  (oddp 2) (cons 2)
  (oddp 3) (cons 3))
```
=> (3 1)"
  (cond-macro '->> expr clauses))

(defun some-macro (threading expr forms)
  "Refactored some-> logic, varies only by threading semantics.
  THREADING is the symbol for either '-> or '->>'."
  (let* ((g (gensym))
         (steps (mapcar (lambda (step)
                          `(if (null ,g) nil (,threading ,g ,step)))
                        forms)))
    `(let* ((,g ,expr)
            ,@(mapcar (lambda (step) (list g step)) (butlast steps)))
       ,(if (null steps)
            g
            (first (last steps))))))

(defmacro some-> (expr &rest forms)
  "When expr is not nil, threads it into the first form (via ->),
and when that result is not nil, through the next etc.

E.g. `(some-> '(:a 1 :b 2) (getf :b) 1+)` => 3
     `(some-> '(:a 1 :b 2) (getf :c) 1+)` => NIL"
  (some-macro '-> expr forms))


(defmacro some->> (expr &rest forms)
  "When expr is not nil, threads it into the first form (via ->>),
and when that result is not nil, through the next etc.

E.g. `(some->> '((:a . 1) (:b . 2)) (assoc :b) cdr 1+)` => 3
     `(some->> '((:a . 1) (:b . 2)) (assoc :c) cdr 1+)` => NIL"
  (some-macro '->> expr forms))

(defun diamond-symbol-p (exp)
  "Check for the diamond substitution symbol, ignoring the package in which it resides."
  (and (symbolp exp)
       (string= exp "<>")))

(defun maybe-subst-diamonds (threading x form)
  "Thread a form as if for `->` or `->>` unless the form contains diamonds to be substituted"
  (if (listp form)
      (let ((n-diamonds (count-if #'diamond-symbol-p form)))
        (cond
          ((= n-diamonds 0) `(,threading ,x ,form))
          ((= n-diamonds 1) (substitute-if x #'diamond-symbol-p form))
          (t (let ((g (gensym)))
               `(let ((,g ,x))
                  ,(substitute-if g #'diamond-symbol-p form))))))
      (list form x)))

(defun diamond-macro (threading x forms)
  "Utility routine to thread forms like `->` or `->>` unless there are `<>` substitutions."
  (reduce (lambda (x form)
            (maybe-subst-diamonds threading x form))
          forms :initial-value x))

(defmacro -<> (expr &rest forms)
   "So-called 'diamond wand' replaces top-level references to `<>` with the
threaded expr/form. If there is no reference to `<>`, behaves like the
thread-first (`->`) macro. Note that this implementation allows `<>` to be
referenced multiple times in a form, and will use a LET binding to avoid
re-evaluation of the substitued expression.

E.g. 
```
(defun foo () (print \"hey!\") 5)
(-<> (foo) (list 1 <> <> 2) (list 3 <> <> 4)) => (3 (1 5 5 2) (1 5 5 2) 4)
```
Note that the 'hey!' is printed only once.
Note also that the above example works identically in both `-<>` and `-<>>`,
the behave differently only in forms which do not use the `<>` substitution."
  (diamond-macro '-> expr forms))

(defmacro -<>> (expr &rest forms)
  "So-called 'diamond spear' replaces top-level references to `<>` with the
threaded expr/form. If there is no reference to `<>`, behaves like the
thread-last (`->>`) macro. Note that this implementation allows `<>` to be
referenced multiple times in a form, and will use a LET binding to avoid
re-evaluation of the substitued expression.

E.g. (-<>> 10 (list 1 <> 3)) => (1 10 3)
     (-<>> 10 (list 1 3))    => (1 3 10)

See `-<>` for more substitution examples which would work identically between the
first and last threading forms of diamond macros."
  (diamond-macro '->> expr forms))

(defun some-diamond-macro (threading expr forms)
  "Utility routine to thread forms like `some->` or `some->>` unless there are 
`<>` substitutions."
  (let* ((g (gensym))
         (steps (mapcar (lambda (step)
                          `(if (null ,g) nil ,(maybe-subst-diamonds threading g step)))
                        forms)))
    `(let* ((,g ,expr)
            ,@(mapcar (lambda (step) (list g step)) (butlast steps)))
       ,(if (null steps)
            g
            (first (last steps))))))

(defmacro some-<> (expr &rest forms)
  "When expr is not nil, threads it into the first form (via `-<>`),
and when that result is not nil, through the next etc, performing diamond substitutions
in the same way as `-<>`.

See `-<>` for more details on substitutions or lack thereof. Without `<>` substitutions
`some-<>` behaves identically to `some->`.

E.g. `(some-<> '((:a . 1) (:b . 2)) (assoc :b <>) cdr 1+)` => 3
     `(some-<> 5 (list 1 2) (list 3 4))` => ((5 1 2) 3 4)"
  (some-diamond-macro '-> expr forms))

(defmacro some-<>> (expr &rest forms)
  "When expr is not nil, threads it into the first form (via `-<>>`),
and when that result is not nil, through the next etc, performing diamond substitutions
in the same way as `-<>>`.

See `-<>>` for more details on substitutions or lack thereof.  Without `<>` substitutions
`some-<>>` behaves identically to `some->>`.

E.g. `(some-<>> '(:a 1 :b 2) (getf <> :b) 1+)` => 3
     `(some-<>> 5 (list 1 2) (list 3 4))` => (3 4 (1 2 5))"
  (some-diamond-macro '->> expr forms))


