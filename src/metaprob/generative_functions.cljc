(ns metaprob.generative-functions
  #?(:cljs (:require-macros [metaprob.generative-functions :refer [gen]]))
  (:require #?(:cljs [cljs.analyzer :as ana])
            [metaprob.code-handlers :as code]
            [metaprob.trace :as trace]))

(defn at [& args] (assert false "Cannot invoke at outside of a (gen ...) form."))

(defn apply-at [& args] (assert false "Cannot invoke apply-at outside of a (gen ...) form."))

;; Most general way of creating a generative function: provide implementations of its
;; methods. All other ways of creating generative functions boil down, ultimately, to
;; a call to this function.
(defn make-generative-function
  ([run-in-clojure make-constrained-generator]
   (make-generative-function run-in-clojure make-constrained-generator {}))
  ([run-in-clojure make-constrained-generator others]
   (with-meta run-in-clojure (assoc others :make-constrained-generator make-constrained-generator))))

;; make-constrained-generator : generative function, observation trace -> generative function
(defn make-constrained-generator [procedure observations]
  ((or (get (meta procedure) :make-constrained-generator)
       (fn [observations]
         (fn [& args]
           [(apply procedure args) {} 0]))) observations))

(defn generative-function-from-traced-code
  [fn-accepting-tracers metadata]
  (make-generative-function
    ;; Implementation of "run in Clojure"
    (fn-accepting-tracers
      ;; Implementation of `at`:
      (fn [addr proc & args] (apply proc args))
      ;; Implementation of `apply-at`:
      (fn [addr proc args] (apply proc args)))

    ;; Implementation of "make constrained generator"
    (fn [observations]
      (generative-function-from-traced-code
        (fn [at apply-at]
          (fn [& args]
            (let [score (volatile! 0.0M)
                  trace (volatile! {})
                  apply-at-impl
                  (fn [addr gf args]
                    (let [[v tr s] (apply-at
                                    addr
                                    (make-constrained-generator
                                     gf
                                     (trace/maybe-subtrace observations addr)) args)]
                      (vswap! score unchecked-add s)
                      (vswap! trace trace/merge-subtrace addr tr)
                      v))
                  at-impl
                  (fn [addr gf & args] (apply-at-impl addr gf args))
                  result (apply (fn-accepting-tracers at-impl apply-at-impl) args)]
              [result (deref trace) (deref score)])))
        {:name 'make-constrained-generator-impl}))

    ;; Additional metadata
    metadata))

;; Create a generative function using executable code that samples from other generative functions.
(defmacro gen [& _]
  {:style/indent 1}
  (let [expr
        &form

        body
        (code/gen-body expr)

        name
        (code/gen-name expr)

        tracer-name
        'at

        apply-tracer-name
        'apply-at

        params
        (code/gen-pattern expr)

        thunk-name
        (if name (gensym (str name "thunk")) nil)

        named-fn-body
        (if name
          `((let [~name (~thunk-name)]
              ~@body))
          body)

        innermost-fn-expr
        `(fn ~params ~@named-fn-body)

        generative-function-expression
        `(generative-function-from-traced-code
           (fn [~tracer-name ~apply-tracer-name] ~innermost-fn-expr)
           {:name '~name, :generative-source '~expr})]

    (if name
      `((fn ~thunk-name [] ~generative-function-expression))
      generative-function-expression)))

(defmacro let-traced [bindings & body]
  (let [binding-pairs (partition 2 bindings)

        trace-with-name
        (fn trace-with-name [expr name]
          (cond
            (code/if-expr? expr)
            `(if ~(trace-with-name (code/if-predicate expr) name)
               ~(trace-with-name (code/if-then-clause expr) name)
               ~(trace-with-name (code/if-else-clause expr) name))

            (code/do-expr? expr)
            (cons 'do (map #(trace-with-name % name) (rest expr)))

            (or (not (seq? expr))
                (special-symbol? (first expr))
                (code/let-expr? expr)
                (code/let-traced-expr? expr)
                (code/fn-expr? expr)
                (code/gen-expr? expr))
            expr

            ;; If a macro, try expanding
            (not= #?(:clj (macroexpand-1 expr)
                     :cljs (ana/macroexpand-1 &env expr))
                  expr)
            (recur #?(:clj (macroexpand-1 expr)
                      :cljs (ana/macroexpand-1 &env expr))
                   name)

            true
            `(~'at ~name ~@expr)))

        convert-binding
        (fn [[lhs rhs]]
          (if (symbol? lhs)
            [lhs (trace-with-name rhs (str lhs))]
            [lhs rhs]))

        new-bindings (vec (apply concat (map convert-binding binding-pairs)))]
    `(let ~new-bindings ~@body)))
