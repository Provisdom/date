(ns provisdom.date.collections
  "High-performance collections optimized for date operations using integer maps.

  Provides date-map and date-set collections that leverage clojure.data.int-map
  for significantly faster operations on date collections compared to standard
  Clojure maps and sets. Includes:
  - date-map and date-set constructors and predicates
  - Set operations (union, intersection, difference)
  - Range constructors (date-range-set, date-range-map)
  - Slice operations for extracting date ranges
  - Floor/ceiling lookups for nearest-date queries
  - Filter operations for maps

  Use dense-date-set when dates are densely packed (e.g., consecutive days or intraday timestamps)."
  (:require
    [clojure.data.int-map :as int-map]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.date.tick :as tick])
  (:import (clojure.data.int_map PersistentIntMap
                                 PersistentIntSet)))

;;;PREDICATES (must be defined before specs that use them)
(defn date-map?
  "Returns true if `x` is a date-map."
  [x]
  (instance? PersistentIntMap x))

(defn date-set?
  "Returns true if `x` is a date-set."
  [x]
  (instance? PersistentIntSet x))

;;;SPECS
(defmacro date-map-of
  "Creates a spec for a date-map with values matching `vpred`.

  Example:
    (s/def ::price-map (date-map-of pos?))"
  [v-pred & opts]
  (let [s-form `(s/map-of ::tick/date ~v-pred ~@opts)
        xform `(s/and ~s-form date-map?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (int-map/int-map))
                  (s/gen ~s-form)))))

(s/def ::date-set
  (s/coll-of ::tick/date
             :into (int-map/int-set)
             :kind date-set?))

(s/def ::dense-date-set
  (s/coll-of ::tick/date
             :into (int-map/dense-int-set)
             :kind date-set?))

(s/def ::date-map
  (s/with-gen
    (s/and map? date-map?)
    #(gen/fmap (partial into (int-map/int-map))
               (s/gen (s/map-of ::tick/date any?)))))

;;;DATE MAP
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
  "Updates the value at `date` by applying function `f`. If `date` is not present, `f` is called with `nil`.

  Example:
    (map-update dm date inc)"
  ([date-map date f] (int-map/update date-map date f))
  ([date-map date f & args] (apply int-map/update date-map date f args)))

(defn map-update!
  "Transient version of [[map-update]] for performance-critical code. Mutates the transient date-map in place."
  ([date-map date f] (int-map/update! date-map date f))
  ([date-map date f & args] (apply int-map/update! date-map date f args)))

;;;DATE SET
(defn date-set
  "Creates a date-set for sparse date collections. Use [[dense-date-set]] when dates are densely packed (e.g.,
  consecutive days).

  Example:
    (date-set [date1 date2 date3])"
  ([] (int-map/int-set))
  ([dates] (int-map/int-set dates)))

(defn dense-date-set
  "Creates a dense date-set optimized for densely packed dates. More memory efficient than [[date-set]] when dates are
  close together (e.g., consecutive days, intraday timestamps, or time series data).

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

;;;RANGE CONSTRUCTORS
(defn date-range-set
  "Creates a date-set from dates in range [start-date, end-date] with `step-ticks`.

  Example:
    (date-range-set date-2020 date-2021 ticks-per-day)"
  [start-date end-date step-ticks]
  (int-map/int-set (range start-date (inc end-date) step-ticks)))

(s/def ::bounded-range-args
  (s/with-gen
    (s/and (s/cat :start-date ::tick/date
                  :end-date ::tick/date
                  :step-ticks ::tick/ticks+)
           #(<= (count (range (:start-date %) (inc (:end-date %)) (:step-ticks %))) 100))
    #(gen/fmap (fn [[start n step]]
                 [start (+ start (* n step)) step])
               (gen/tuple (s/gen ::tick/date)
                          (gen/choose 0 100)
                          (gen/choose 1 1000000000000000)))))

(s/fdef date-range-set
  :args ::bounded-range-args
  :ret ::date-set)

(defn date-range-map
  "Creates a date-map from dates in range [start-date, end-date] with `step-ticks`.

  `value-fn` is called with each date to produce the value.

  Example:
    (date-range-map date-2020 date-2021 ticks-per-day (constantly 0.0))"
  [start-date end-date step-ticks value-fn]
  (reduce (fn [m date]
            (assoc m date (value-fn date)))
          (int-map/int-map)
          (range start-date (inc end-date) step-ticks)))

(s/def ::bounded-range-map-args
  (s/with-gen
    (s/and (s/cat :start-date ::tick/date
                  :end-date ::tick/date
                  :step-ticks ::tick/ticks+
                  :value-fn ifn?)
           #(<= (count (range (:start-date %) (inc (:end-date %)) (:step-ticks %))) 100))
    #(gen/fmap (fn [[start n step]]
                 [start (+ start (* n step)) step identity])
               (gen/tuple (s/gen ::tick/date)
                          (gen/choose 0 100)
                          (gen/choose 1 1000000000000000)))))

(s/fdef date-range-map
  :args ::bounded-range-map-args
  :ret ::date-map)

;;;SLICE OPERATIONS
(defn set-slice
  "Returns dates in `date-set` between `start-date` and `end-date` (inclusive)."
  [date-set start-date end-date]
  (->> date-set
       (drop-while #(< % start-date))
       (take-while #(<= % end-date))
       int-map/int-set))

(s/fdef set-slice
  :args (s/cat :date-set ::date-set
               :start-date ::tick/date
               :end-date ::tick/date)
  :ret ::date-set)

(defn map-slice
  "Returns entries in `date-map` between `start-date` and `end-date` (inclusive)."
  [date-map start-date end-date]
  (->> date-map
       (drop-while (fn [[k _]] (< k start-date)))
       (take-while (fn [[k _]] (<= k end-date)))
       (into (int-map/int-map))))

(s/fdef map-slice
  :args (s/cat :date-map ::date-map
               :start-date ::tick/date
               :end-date ::tick/date)
  :ret ::date-map)

;;;NEAREST-DATE LOOKUPS
(defn set-floor
  "Returns the greatest date in `date-set` that is <= `date`, or `nil` if none."
  [date-set date]
  (reduce (fn [best d]
            (if (<= d date)
              d
              (reduced best)))
          nil
          date-set))

(s/fdef set-floor
  :args (s/cat :date-set ::date-set
               :date ::tick/date)
  :ret (s/nilable ::tick/date))

(defn set-ceiling
  "Returns the least date in `date-set` that is >= `date`, or `nil` if none."
  [date-set date]
  (reduce (fn [_ d]
            (when (>= d date)
              (reduced d)))
          nil
          date-set))

(s/fdef set-ceiling
  :args (s/cat :date-set ::date-set
               :date ::tick/date)
  :ret (s/nilable ::tick/date))

(defn map-floor
  "Returns the entry `[date value]` in `date-map` where date is <= `date`, or `nil`. Returns the entry with the
  greatest date that doesn't exceed `date`."
  [date-map date]
  (reduce (fn [best [k _ :as entry]]
            (if (<= k date)
              entry
              (reduced best)))
          nil
          date-map))

(s/fdef map-floor
  :args (s/cat :date-map ::date-map
               :date ::tick/date)
  :ret (s/nilable (s/tuple ::tick/date any?)))

(defn map-ceiling
  "Returns the entry `[date value]` in `date-map` where date is >= `date`, or `nil`. Returns the entry with the least
  date that is at least `date`."
  [date-map date]
  (reduce (fn [_ [k _ :as entry]]
            (when (>= k date)
              (reduced entry)))
          nil
          date-map))

(s/fdef map-ceiling
  :args (s/cat :date-map ::date-map
               :date ::tick/date)
  :ret (s/nilable (s/tuple ::tick/date any?)))

;;;FILTER OPERATIONS
(defn map-filter-vals
  "Returns date-map with only entries where (pred val) is truthy."
  [pred date-map]
  (into (int-map/int-map)
        (filter (fn [[_ v]] (pred v)))
        date-map))

(s/fdef map-filter-vals
  :args (s/cat :pred ifn?
               :date-map ::date-map)
  :ret ::date-map)

(defn map-filter-keys
  "Returns date-map with only entries where (pred date) is truthy."
  [pred date-map]
  (into (int-map/int-map)
        (filter (fn [[k _]] (pred k)))
        date-map))

(s/fdef map-filter-keys
  :args (s/cat :pred ifn?
               :date-map ::date-map)
  :ret ::date-map)
