(ns metaprob.examples.search-by-example
  (:require [metaprob.examples.cgpm :refer [cgpm-logpdf]]
            [metaprob.examples.nyt :as nyt :refer [generate-census-row]]
            [metaprob.examples.multimixture-dsl :as mmix]
            [metaprob.prelude :as prelude]
            [metaprob.trace :as trace]
            [metaprob.examples.data :refer [nyt-data]]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))

(defn- kli [p-in q-in]
  (let [p (if (<= p-in 0) 0.00000000000000000001 p-in)
        q (if (<= q-in 0) 0.00000000000000000001 q-in)]
    (* p (Math/log (/ p q)))))

(defnp kl [ps qs]
  "K-L divergence between two vectors of floating point numbers."
  (apply +
         (map (fn [p q] (kli p q))
              ps qs)))

(defnp constrain-by-row
  "Constrains the given trace such that the values chosen for each column are the
  ones in the provided row."
  [trace row]
  (reduce (fn [trace [column value]]
            (trace/trace-set-value trace column value))
          trace
          row))

(defnp constrain-by-view
  "Constrains the given trace with such that the view chosen is the one provided."
  [trace cluster view]
  (trace/trace-set-value trace (mmix/view-cluster-address view) cluster))

(defnp constrain-by-cluster
  "Constrains the given trace such that the cluster chosen is the one provided."
  [trace cluster columns]
  (reduce (fn [trace column]
            (trace/trace-set-value trace
                                   (mmix/column-cluster-address column)
                                   cluster))
          trace
          columns))

(defnp normalize
  "Normalizes a vector of numbers such that they sum to 1."
  [ns]
  (let [sum (apply + ns)]
    (mapv #(/ % sum)
          ns)))

(defnp probability-distribution-on-cluster
  [model row view]
  (let [columns (-> nyt/clusters second keys)
        cluster-addresses (range (nyt/cluster-count nyt/clusters))]
    (->> cluster-addresses
         (map (fn [cluster-address]
                (let [trace (-> {}
                                (constrain-by-view cluster-address view)
                                (constrain-by-cluster cluster-address columns)
                                (constrain-by-row row))
                      [_ _ score] (prelude/infer-and-score :procedure model
                                                           :observation-trace trace)]
                  (prelude/exp score))))
         (normalize))))

(def memoized-probability-distribution-on-cluster
  (memoize probability-distribution-on-cluster))

(defnp rowwise-similarity [cgpm view example-pfca row emphasis]
  (kl example-pfca (memoized-probability-distribution-on-cluster cgpm row view)))

(defnp search
  [cgpm rows example emphasis]
  (let [view (mmix/view-for-column emphasis)
        example-pfca (probability-distribution-on-cluster (:proc cgpm) example emphasis)]
    (->> rows
         (map-indexed (fn [index row]
                        [index (rowwise-similarity (:proc cgpm) view example-pfca row emphasis)]))
         (sort-by second))))

(comment

  (defn make-ex-row []
    (zipmap [:percent_married_children
             :percent_black
             :percent_college
             :percap]
            (generate-census-row)))

  (let [fix-table (fn fix-table [t]
                    (map
                     #(dissoc % :geo_fips :district_name)
                     (clojure.walk/keywordize-keys t)))]
    (tufte/add-basic-println-handler! {})
    (tufte/profile
     {}
     (search nyt/census-cgpm
             (fix-table nyt-data)
             {:percent_black 0.90}
             :cluster-for-percap)))

  )
