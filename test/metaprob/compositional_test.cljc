(ns metaprob.compositional-test
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.trace :as trace]
            [metaprob.generative-functions :as gen :refer [gen]]
            [metaprob.distributions :as dist]
            [metaprob.prelude :as pre])
  (:refer-clojure :exclude [assoc dissoc]))

(defn ez-call [prob-prog & inputs]
  (let [inputs (if (= inputs nil) '() inputs)
        [value _ _]
        (pre/infer-and-score :procedure prob-prog :inputs inputs)]
    value))

(deftest apply-2
  (testing "Apply a procedure to one arg"
    (is (= (ez-call - 7)
           -7))))

(deftest thunk-1
  (testing "call a thunk"
    (is (= (ez-call (gen [] 7))
           7))))

;; N.b. this will reify the procedure to get stuff to eval

(deftest binding-1
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-call (gen [x] x) 5)
           5))))

(deftest binding-3
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-call (gen [] (let [x 17] x)))
           17))))

(deftest n-ary-1
  (testing "n-ary procedure, formal parameter list is [& y]"
    (is (= (first (ez-call (gen [& y] y)
                           8 9))
           8))))

(deftest n-ary-2
  (testing "n-ary procedure, & in formal parameter list"
    (let [result (ez-call  (gen [x & y] y)
                           7 8 9 10)]
      (is (seq? result))
      (is (= (count result) 3))
      (is (= (first result) 8)))))


(deftest traced-and-scored-execution
  (testing "traced and scored execution"
    (let [f (gen [p] (at "x" dist/flip p))
          p 0.4
          [v1 t1 s1] (pre/infer-and-score :procedure f :inputs [p])
          [v2 t2 s2] (pre/infer-and-score :procedure f :inputs [p] :observation-trace t1)]

      (is (boolean? (f 0.5)))
      (is (true? (f 1)))
      (is (false? (f 0)))
      (is (= '(("x")) (trace/addresses-of t1)))
      (is (not (trace/trace-has-value? t1)))
      (is (= v1 (trace/trace-value t1 "x")))
      (is (= s1 0.0M))
      (is (= s2 (if v1 (pre/log p) (pre/log (- 1 p)))))
      (is (= t1 t2))
      (is (= v1 v2)))))

(deftest control-flow
  (testing "infer-and-score with weird control flow"
    (let [bar (gen [mu] (at "a" dist/gaussian mu 1))
          baz (gen [mu] (at "b" dist/gaussian mu 1))
          foo
          (gen [mu]
            (if (at "branch" dist/flip 0.4)
              (do (at "x" dist/gaussian mu 1)
                  (at "u" bar mu))
              (do (at "y" dist/gaussian mu 1)
                  (at "v" baz mu))))

          mu
          0.123

          [_ first-branch-trace _]
          (pre/infer-and-score :procedure foo :inputs [mu] :observation-trace {"branch" {:value true}})

          x
          (trace/trace-value first-branch-trace "x")

          a
          (trace/trace-value first-branch-trace '("u" "a"))

          [_ fixed-choices]
          (trace/partition-trace first-branch-trace '(("branch")))]

      (loop [i 0]
        (when (< i 10)
          (let [[v t s] (pre/infer-and-score :procedure foo :inputs [mu] :observation-trace fixed-choices)]
            (if (trace/trace-value t "branch")
              (do (is (= t first-branch-trace))
                  (is (not= s 0.0M)))
              (do (is (trace/trace-has-value? t "y"))
                  (is (trace/trace-has-value? t '("v" "b")))
                  (is (= s 0.0M))
                  (is (= 3 (count (trace/addresses-of t))))))
            (recur (inc i))))))))

(deftest self-execution
  (testing "running infer-and-score on infer-and-score"
    (let [f (gen [] (and (at 1 dist/flip 0.1) (at 2 dist/flip 0.4)))
          [[inner-v inner-t inner-s] t s]
          (pre/infer-and-score
           :procedure pre/infer-and-score
           :inputs [:procedure f, :observation-trace {2 {:value true}}]
           :observation-trace {1 {:value true}})]
      (is (= (count (trace/addresses-of inner-t)) 2))
      (is (not (trace/trace-has-value? t 2)))
      (is (= s (pre/log 0.1)))
      (is (= inner-s (pre/log 0.4)))
      (is inner-v))))


;;; `case` expands to use clojure-internal `case*`, which can't work in
;;; metaprob until we implement that (or manually expand `case`). Until
;;; we do that, I'm going to leave this commented-out and file an
;;; issue (jmt)
#_
(deftest case-1
  (testing "case smoke test"
    (is (= (ez-eval (mp-expand '(case 1 2))) 2))
    (is (= (ez-eval (mp-expand '(case 1 1 2))) 2))
    (is (= (ez-eval (mp-expand '(case 1 1 2 3))) 2))
    (is (= (ez-eval (mp-expand '(case 1 2 3 1 4))) 4))))

#_
(define tst1
  (gen []
    (define x (if (distributions/flip 0.5) 0 1))
    (+ x 3)))

#_
(deftest intervene-4
  (testing "intervention value is recorded when it overwrites normally-traced execution"
    (let [intervene (trace-set-value {} '(0 "x") 5)
          [value out __prefix__] (comp/infer-apply
                                  tst1 []
                                  {:interpretation-id (clojure.core/gensym)
                                   :intervene intervene
                                   :target no-trace
                                   :active? true})]
      (is (= value 8)))))

;;; in situations where an intervention targets a non-random site,
;;; `infer`'s _value_ should be affected, but the returned trace should
;;; still contain the (unused) random choices
#_
(deftest intervene-target-disagree
  (testing "intervention and target traces disagree"
    (let [intervene (trace-set-value {} '(0 "x") 5)
          target (trace-set-value {} '(0 "x") 5)
          [value out s] (comp/infer-apply tst1 []
                                          {:interpretation-id (clojure.core/gensym)
                                           :intervene intervene
                                           :target target
                                           :active? true})]
      (is (= value 8))
      (is (= s 0))
      (trace-has-value? out '(0 "x" "predicate" "distributions/flip"))
      (is (and (trace-has-value? out '(0 "x" "predicate" "distributions/flip"))
               (clojure.core/contains?
                #{true false}
                (trace-value out '(0 "x" "predicate" "distributions/flip"))))))))


;;; an assert keeps this from working. if that's expected, this test
;;; should change to catch that AssertionError (jmt)
#_
(deftest intervene-target-disagree
  (testing "intervention and target traces disagree. this should throw."
    (let [intervene (trace-set-value {} '(0 "x") 6)
          target (trace-set-value {} '(0 "x") 5)]

      (is (thrown? AssertionError
                   (comp/infer-apply tst1 []
                                     {:interpretation-id (clojure.core/gensym)
                                      :intervene intervene
                                      :target target
                                      :active? true}))))))

;;; an assert keeps this from happening. if that's expected, this test
;;; should change to catch that AssertionError (jmt)
#_
(deftest impossible-target
  (testing "target is impossible value"
    (let [target (trace-set-value {} '(0 "x") 5)]

      (is (thrown? AssertionError
                   (comp/infer-apply tst1 []
                                     {:interpretation-id (clojure.core/gensym)
                                      :intervene {}
                                      :target target
                                      :active? true}))))))

#_
(deftest true-target
  (testing "target value is the true value"
    (let [form '(block (define x (+ 15 2)) (- x 2))
          target (trace-set-value {} '(0 "x" "+") 17)
          [value out s] (comp/infer-eval form top
                                         {:interpretation-id (clojure.core/gensym)
                                          :intervene no-trace
                                          :target no-trace
                                          :active? true})]
      (is (= value 15))
      (is (= s 0))
      (is (empty? out)))))

;;; Self-application

;;; These have to be defined at top level because only top level
;;; defined gens can be interpreted (due to inability to understand
;;; environments).

;;; TODO: Explore what it would take to remove this limitation.
#_
(define apply-test
  (gen [thunk]
    (define [val output score]
      (infer-apply thunk [] {:interpretation-id (clojure.core/gensym)
                             :intervene no-trace
                             :target no-trace
                             :active? true}))
    output))

#_
(define tst2 (gen [] (distributions/flip 0.5)))
#_
(define tst3 (gen [] (apply-test tst2)))

#_
(deftest infer-apply-self-application
  (testing "apply infer-apply to program that calls infer-apply"
    (binding [*ambient-interpreter* infer-apply]
      ;; When we interpret tst1 directly, the value of flip is
      ;; recorded at the length-1 address '(distributions/flip).
      (is (= (count (first (addresses-of (apply-test tst2)))) 1))

      ;; But when we trace the execution of the interpreter, the
      ;; address at which the random choice is recorded is
      ;; significantly longer, due to the complex chain of function
      ;; calls initiated by the interpreter.
      (is (> (count (first (addresses-of (apply-test tst3)))) 10)))))
