(ns provisdom.date.collections
  (:require
    [clojure.data.int-map :as int-map]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.date.tick :as tick])
  (:import (clojure.data.int_map PersistentIntMap
                                 PersistentIntSet)))

;;;;This namespace was created to wrap the clojure.data.int-map ns for use with
;;;; dates (and ticks) -- mostly to help remember to use it. The two collections
;;;; in this ns (date-map and date-set) have the possibility to be significantly
;;;; faster than normal sorted collections. A dense date set is a special type
;;;; of date set. [[map-merge]] is fast, so you can use
;;;; (reducers/fold [[map-merge]] reducef coll) for fast parallelism of creating
;;;; and manipulating date-maps.
;;;; See https://github.com/clojure/data.int-map.

(declare date-set? date-map?)

(defmacro date-map-of
  [vpred & opts]
  (let [sform `(s/map-of ::tick/date ~vpred ~@opts)
        xform `(s/and ~sform date-map?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (int-map/int-map))
                  (s/gen ~sform)))))

(s/def ::date-set
  (s/coll-of ::tick/date
             :into (int-map/int-set)
             :kind date-set?))

(s/def ::dense-date-set
  (s/coll-of ::tick/date
             :into (int-map/dense-int-set)
             :kind date-set?))

;;;DATE MAP
(defn date-map?
  "Checks whether `x` is a ::date-map."
  [x]
  (instance? PersistentIntMap x))

(defn date-map
  "Creates a date map that can only have dates (ticks, longs) as keys."
  ([] (int-map/int-map))
  ([date v] (int-map/int-map date v))
  ([date v & rest] (apply int-map/int-map date v rest)))

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

;;;DATE SET
(defn date-set?
  "Checks whether `x` is a ::date-set."
  [x]
  (instance? PersistentIntSet x))

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
