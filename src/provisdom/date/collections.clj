(ns provisdom.date.collections
  "High-performance collections optimized for date operations using integer maps.

  Provides date-map and date-set collections that leverage clojure.data.int-map
  for significantly faster operations on dense date collections. Use dense-date-set
  when elements are clustered within +/- 1000 ticks of each other."
  (:require
    [clojure.data.int-map :as int-map]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.date.tick :as tick])
  (:import (clojure.data.int_map PersistentIntMap
                                 PersistentIntSet)))


(declare date-set? date-map?)

(defmacro date-map-of
  "Creates a spec for a date-map with values matching `vpred`.
  
  Example:
    (s/def ::price-map (date-map-of pos?))"
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
  "Returns true if `x` is a date-map."
  [x]
  (instance? PersistentIntMap x))

(defn date-map
  "Creates a date-map with dates as keys.
  
  Accepts pairs of date/value arguments.
  
  Example:
    (date-map date1 value1 date2 value2)"
  ([] (int-map/int-map))
  ([date v] (int-map/int-map date v))
  ([date v & rest] (apply int-map/int-map date v rest)))

(defn map-merge
  "Merges date-maps with right-most values taking precedence.

  Example:
    (map-merge dm1 dm2 dm3)"
  ([] (int-map/merge))
  ([date-map1 date-map2] (int-map/merge date-map1 date-map2))
  ([date-map1 date-map2 & rest] (apply int-map/merge date-map1 date-map2 rest)))

(defn map-merge-with
  "Merges date-maps using function `f` to resolve conflicts.
  
  When keys are present in multiple maps, `f` is called with
  the conflicting values.
  
  Example:
    (map-merge-with + dm1 dm2) ; adds values for duplicate keys"
  ([f] (int-map/merge-with f))
  ([f date-map1 date-map2] (int-map/merge-with f date-map1 date-map2))
  ([f date-map1 date-map2 & rest]
   (apply int-map/merge-with f date-map1 date-map2 rest)))

(defn map-update
  "Updates the value at `date` by applying function `f`.
  
  If `date` is not present, `f` is called with nil.
  
  Example:
    (map-update dm date inc)"
  ([date-map date f] (int-map/update date-map date f))
  ([date-map date f & args] (apply int-map/update date-map date f args)))

(defn map-update!
  "Transient version of map-update for performance-critical code.
  
  Mutates the transient date-map in place."
  ([date-map date f] (int-map/update! date-map date f))
  ([date-map date f & args] (apply int-map/update! date-map date f args)))

;;;DATE SET
(defn date-set?
  "Returns true if `x` is a date-set."
  [x]
  (instance? PersistentIntSet x))

(defn date-set
  "Creates a date-set for sparse date collections.
  
  Use dense-date-set when dates are clustered within +/- 1000 ticks.
  
  Example:
    (date-set [date1 date2 date3])"
  ([] (int-map/int-set))
  ([dates] (int-map/int-set dates)))

(defn dense-date-set
  "Creates a dense date-set optimized for clustered dates.
  
  Use when dates are densely clustered (within +/- 1000 ticks).
  More memory efficient than regular date-set for dense collections.
  
  Example:
    (dense-date-set consecutive-trading-days)"
  ([] (int-map/dense-int-set))
  ([dates] (int-map/dense-int-set dates)))

(defn set-union
  "Returns the union of two date-sets.
  
  Contains all dates from both sets."
  [date-set1 date-set2]
  (int-map/union date-set1 date-set2))

(defn set-intersection
  "Returns the intersection of two date-sets.
  
  Contains only dates present in both sets."
  [date-set1 date-set2]
  (int-map/intersection date-set1 date-set2))

(defn set-difference
  "Returns dates in `date-set1` but not in `date-set2`."
  [date-set1 date-set2]
  (int-map/difference date-set1 date-set2))
