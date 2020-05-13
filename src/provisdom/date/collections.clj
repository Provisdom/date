(ns provisdom.date.collections
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [clojure.data.int-map :as int-map]
    [clojure.core.reducers :as reducers]))

;;;;This namespace was created to wrap the clojure.data.int-map ns for use with
;;;; dates (and ticks) -- mostly to help remember to use it. The collections in
;;;; this ns have the possibility to be significantly faster than normal sorted
;;;; collections.  See https://github.com/clojure/data.int-map.

(defn date-map
  "Creates a date map that can only have dates (ticks, longs) as keys."
  ([] (int-map/int-map))
  ([date v] (int-map/int-map date v))
  ([date v & rest] (apply int-map/int-map date v rest)))

(defn into-date-map
  "Fast 'into' function for date-map. `entries` can be a vector or a hash-map
  for fastest performance."
  [entries]
  (reducers/fold int-map/merge conj entries))

(defn map-merge
  "Merges together two date-maps, giving precedence to values from the
  right-most map."
  ([] (int-map/merge))
  ([date-map1 date-map2] (int-map/merge date-map1 date-map2))
  ([date-map1 date-map2 & rest] (apply int-map/merge date-map1 date-map2 rest)))

(defn map-merge-with
  "Merges together two date-maps, using `f` to resolve value conflicts."
  ([f] (int-map/merge-with f))
  ([f date-map1 date-map2] (int-map/merge-with f date-map1 date-map2))
  ([f date-map1 date-map2 & rest]
   (apply int-map/merge-with f date-map1 date-map2 rest)))

(defn map-update
  "Updates the value associated with the given date.  If no such date exists,
  `f` is invoked with `nil`."
  ([date-map date f] (int-map/update date-map date f))
  ([date-map date f & args] (apply int-map/update date-map date f args)))

(defn map-update!
  "A transient variant of [[map-update]]."
  ([date-map date f] (int-map/update! date-map date f))
  ([date-map date f & args] (apply int-map/update! date-map date f args)))

(defn date-set
  "Use [[dense-date-set]] where the elements are densely clustered (each element
  has multiple elements within +/- 1000), and [[date-set]] for everything else."
  ([] (int-map/int-set))
  ([dates] (int-map/int-set dates)))

(defn dense-date-set
  "Use [[dense-date-set]] where the elements are densely clustered (each element
  has multiple elements within +/- 1000), and [[date-set]] for everything else."
  ([] (int-map/dense-int-set))
  ([dates] (int-map/dense-int-set dates)))

(defn set-union
  "Returns the union of `date-set1` and `date-set2`."
  [date-set1 date-set2]
  (int-map/union date-set1 date-set2))

(defn set-intersection
  "Returns the intersection of `date-set1` and `date-set2`."
  [date-set1 date-set2]
  (int-map/intersection date-set1 date-set2))

(defn set-difference
  "Returns the difference of `date-set1` and `date-set2`."
  [date-set1 date-set2]
  (int-map/difference date-set1 date-set2))