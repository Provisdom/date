(ns provisdom.date.collections-test
  (:require
    [provisdom.date.collections :as coll]
    [provisdom.date.tick :as tick]
    [provisdom.test.core :as t]))

;;11 seconds

(set! *warn-on-reflection* true)

;;No need for spec-checking DATE SET and DATE MAP functions since they are thin wrappers

;;;DATE MAP
(t/deftest date-map?-test
  (t/with-instrument :all
    (t/is (coll/date-map? (coll/date-map)))
    (t/is (coll/date-map? (coll/date-map tick/date-2020 42)))
    (t/is-not (coll/date-map? {}))
    (t/is-not (coll/date-map? nil))
    (t/is-not (coll/date-map? []))))

(t/deftest date-map-test
  (t/with-instrument :all
    ;; empty constructor
    (let [dm (coll/date-map)]
      (t/is (coll/date-map? dm))
      (t/is= 0 (count dm)))
    ;; single key-value
    (let [dm (coll/date-map tick/date-2020 :value)]
      (t/is= 1 (count dm))
      (t/is= :value (get dm tick/date-2020)))
    ;; multiple key-values
    (let [d1 tick/date-2020
          d2 (+ d1 tick/ticks-per-day)
          d3 (+ d2 tick/ticks-per-day)
          dm (coll/date-map d1 :a d2 :b d3 :c)]
      (t/is= 3 (count dm))
      (t/is= :a (get dm d1))
      (t/is= :b (get dm d2))
      (t/is= :c (get dm d3)))))

(t/deftest map-merge-test
  (t/with-instrument :all
    ;; empty merge
    (t/is (coll/date-map? (coll/map-merge)))
    ;; two maps
    (let [d1 tick/date-2020
          d2 (+ d1 tick/ticks-per-day)
          d3 (+ d2 tick/ticks-per-day)
          dm1 (coll/date-map d1 :a d2 :b)
          dm2 (coll/date-map d2 :b-override d3 :c)
          merged (coll/map-merge dm1 dm2)]
      (t/is= 3 (count merged))
      (t/is= :a (get merged d1))
      (t/is= :b-override (get merged d2))                   ;; right takes precedence
      (t/is= :c (get merged d3)))
    ;; three maps
    (let [d1 tick/date-2020
          dm1 (coll/date-map d1 1)
          dm2 (coll/date-map d1 2)
          dm3 (coll/date-map d1 3)
          merged (coll/map-merge dm1 dm2 dm3)]
      (t/is= 3 (get merged d1)))))                          ;; rightmost wins

(t/deftest map-merge-with-test
  (t/with-instrument :all
    ;; merge with addition
    (let [d1 tick/date-2020
          d2 (+ d1 tick/ticks-per-day)
          dm1 (coll/date-map d1 10 d2 20)
          dm2 (coll/date-map d1 5 d2 10)
          merged (coll/map-merge-with + dm1 dm2)]
      (t/is= 15 (get merged d1))
      (t/is= 30 (get merged d2)))
    ;; merge with vector collection
    (let [d1 tick/date-2020
          dm1 (coll/date-map d1 [:a])
          dm2 (coll/date-map d1 [:b :c])
          merged (coll/map-merge-with into dm1 dm2)]
      (t/is= [:a :b :c] (get merged d1)))
    ;; three maps
    (let [d1 tick/date-2020
          dm1 (coll/date-map d1 1)
          dm2 (coll/date-map d1 2)
          dm3 (coll/date-map d1 3)
          merged (coll/map-merge-with + dm1 dm2 dm3)]
      (t/is= 6 (get merged d1)))))

(t/deftest map-update-test
  (t/with-instrument :all
    ;; update existing key
    (let [dm (coll/date-map tick/date-2020 10)
          updated (coll/map-update dm tick/date-2020 inc)]
      (t/is= 11 (get updated tick/date-2020)))
    ;; update non-existing key (f called with nil)
    (let [dm (coll/date-map)
          updated (coll/map-update dm tick/date-2020 (fnil inc 0))]
      (t/is= 1 (get updated tick/date-2020)))
    ;; update with extra args
    (let [dm (coll/date-map tick/date-2020 10)
          updated (coll/map-update dm tick/date-2020 + 5 3)]
      (t/is= 18 (get updated tick/date-2020)))))

(t/deftest map-update!-test
  (t/with-instrument :all
    ;; transient update
    (let [dm (coll/date-map tick/date-2020 10)
          result (-> dm
                   transient
                   (coll/map-update! tick/date-2020 inc)
                   persistent!)]
      (t/is= 11 (get result tick/date-2020)))
    ;; transient update with extra args
    (let [dm (coll/date-map tick/date-2020 10)
          result (-> dm
                   transient
                   (coll/map-update! tick/date-2020 + 5 3)
                   persistent!)]
      (t/is= 18 (get result tick/date-2020)))))

;;;DATE SET
(t/deftest date-set?-test
  (t/with-instrument :all
    (t/is (coll/date-set? (coll/date-set)))
    (t/is (coll/date-set? (coll/date-set [tick/date-2020])))
    (t/is (coll/date-set? (coll/dense-date-set)))
    (t/is-not (coll/date-set? #{}))
    (t/is-not (coll/date-set? nil))
    (t/is-not (coll/date-set? []))))

(t/deftest date-set-test
  (t/with-instrument :all
    ;; empty constructor
    (let [ds (coll/date-set)]
      (t/is (coll/date-set? ds))
      (t/is= 0 (count ds)))
    ;; with dates
    (let [d1 tick/date-2020
          d2 (+ d1 tick/ticks-per-day)
          d3 (+ d2 tick/ticks-per-day)
          ds (coll/date-set [d1 d2 d3])]
      (t/is= 3 (count ds))
      (t/is (contains? ds d1))
      (t/is (contains? ds d2))
      (t/is (contains? ds d3))
      (t/is-not (contains? ds (+ d3 tick/ticks-per-day))))))

(t/deftest dense-date-set-test
  (t/with-instrument :all
    ;; empty constructor
    (let [ds (coll/dense-date-set)]
      (t/is (coll/date-set? ds))
      (t/is= 0 (count ds)))
    ;; with consecutive dates
    (let [start tick/date-2020
          dates (map #(+ start (* % tick/ticks-per-day)) (range 100))
          ds (coll/dense-date-set dates)]
      (t/is= 100 (count ds))
      (t/is (contains? ds start))
      (t/is (contains? ds (+ start (* 50 tick/ticks-per-day)))))))

(t/deftest set-union-test
  (t/with-instrument :all
    (let [d1 tick/date-2020
          d2 (+ d1 tick/ticks-per-day)
          d3 (+ d2 tick/ticks-per-day)
          d4 (+ d3 tick/ticks-per-day)
          ds1 (coll/date-set [d1 d2])
          ds2 (coll/date-set [d3 d4])
          ds3 (coll/date-set [d2 d3])]
      ;; disjoint sets
      (t/is= 4 (count (coll/set-union ds1 ds2)))
      ;; overlapping sets
      (let [union (coll/set-union ds1 ds3)]
        (t/is= 3 (count union))
        (t/is (contains? union d1))
        (t/is (contains? union d2))
        (t/is (contains? union d3)))
      ;; union with empty
      (t/is= 2 (count (coll/set-union ds1 (coll/date-set)))))))

(t/deftest set-intersection-test
  (t/with-instrument :all
    (let [d1 tick/date-2020
          d2 (+ d1 tick/ticks-per-day)
          d3 (+ d2 tick/ticks-per-day)
          ds1 (coll/date-set [d1 d2 d3])
          ds2 (coll/date-set [d2 d3])
          ds3 (coll/date-set [(+ d3 tick/ticks-per-day)])]
      ;; overlapping
      (let [inter (coll/set-intersection ds1 ds2)]
        (t/is= 2 (count inter))
        (t/is (contains? inter d2))
        (t/is (contains? inter d3)))
      ;; disjoint sets
      (t/is= 0 (count (coll/set-intersection ds1 ds3)))
      ;; intersection with empty
      (t/is= 0 (count (coll/set-intersection ds1 (coll/date-set)))))))

(t/deftest set-difference-test
  (t/with-instrument :all
    (let [d1 tick/date-2020
          d2 (+ d1 tick/ticks-per-day)
          d3 (+ d2 tick/ticks-per-day)
          ds1 (coll/date-set [d1 d2 d3])
          ds2 (coll/date-set [d2])]
      ;; d1, d3 remain after removing d2
      (let [diff (coll/set-difference ds1 ds2)]
        (t/is= 2 (count diff))
        (t/is (contains? diff d1))
        (t/is-not (contains? diff d2))
        (t/is (contains? diff d3)))
      ;; difference from empty set is identity
      (t/is= 3 (count (coll/set-difference ds1 (coll/date-set))))
      ;; empty difference from full set
      (t/is= 0 (count (coll/set-difference (coll/date-set) ds1))))))

;;;RANGE CONSTRUCTORS
(t/deftest date-range-set-test
  (t/with-instrument `coll/date-range-set
    (t/is-spec-check coll/date-range-set))
  (t/with-instrument :all
    (let [step tick/ticks-per-day
          start tick/date-2020
          end (+ start (* 10 step))]
      ;; 11 days: day 0 through day 10
      (t/is= 11 (count (coll/date-range-set start end step)))
      (t/is= start (first (coll/date-range-set start end step)))
      (t/is= end (last (coll/date-range-set start end step)))
      ;; empty when start > end
      (t/is= 0 (count (coll/date-range-set end start step))))))

(t/deftest date-range-map-test
  (t/with-instrument `coll/date-range-map
    (t/is-spec-check coll/date-range-map))
  ;;no instrumentation; Orchestra fspec validation fails with fn args
  (let [step tick/ticks-per-day
        start tick/date-2020
        end (+ start (* 5 step))
        dm (coll/date-range-map start end step (constantly 42))]
    (t/is= 6 (count dm))
    (t/is= 42 (get dm start))
    (t/is= 42 (get dm end))
    ;; test with identity-like fn
    (let [dm2 (coll/date-range-map start end step #(- % start))]
      (t/is= 0 (get dm2 start))
      (t/is= (* 5 step) (get dm2 end)))))

;;;SLICE OPERATIONS
(t/deftest set-slice-test
  (t/with-instrument `coll/set-slice
    (t/is-spec-check coll/set-slice))
  (t/with-instrument :all
    (let [step tick/ticks-per-day
          start tick/date-2020
          ds (coll/date-range-set start (+ start (* 10 step)) step)
          slice-start (+ start (* 3 step))
          slice-end (+ start (* 7 step))]
      ;; days 3, 4, 5, 6, 7 -> 5 elements
      (t/is= 5 (count (coll/set-slice ds slice-start slice-end)))
      (t/is= slice-start (first (coll/set-slice ds slice-start slice-end)))
      (t/is= slice-end (last (coll/set-slice ds slice-start slice-end)))
      ;; slice outside range returns empty
      (t/is= 0 (count (coll/set-slice ds (+ start (* 20 step)) (+ start (* 30 step))))))))

(t/deftest map-slice-test
  (t/with-instrument `coll/map-slice
    (t/is-spec-check coll/map-slice))
  (t/with-instrument :all
    (let [step tick/ticks-per-day
          start tick/date-2020
          dm (coll/date-range-map start (+ start (* 10 step)) step (constantly :v))
          slice-start (+ start (* 2 step))
          slice-end (+ start (* 4 step))]
      ;; days 2, 3, 4 -> 3 elements
      (t/is= 3 (count (coll/map-slice dm slice-start slice-end)))
      (t/is= slice-start (ffirst (coll/map-slice dm slice-start slice-end))))))

;;;NEAREST-DATE LOOKUPS
(t/deftest set-floor-test
  (t/with-instrument `coll/set-floor
    (t/is-spec-check coll/set-floor))
  (t/with-instrument :all
    (let [step tick/ticks-per-day
          start tick/date-2020
          ds (coll/date-range-set start (+ start (* 10 step)) step)
          ;; query between day 3 and day 4
          query (+ start (long (* 3.5 step)))]
      (t/is= (+ start (* 3 step)) (coll/set-floor ds query))
      ;; exact match returns itself
      (t/is= (+ start (* 5 step)) (coll/set-floor ds (+ start (* 5 step))))
      ;; before all returns nil
      (t/is= nil (coll/set-floor ds (- start step))))))

(t/deftest set-ceiling-test
  (t/with-instrument `coll/set-ceiling
    (t/is-spec-check coll/set-ceiling))
  (t/with-instrument :all
    (let [step tick/ticks-per-day
          start tick/date-2020
          ds (coll/date-range-set start (+ start (* 10 step)) step)
          query (+ start (long (* 3.5 step)))]
      (t/is= (+ start (* 4 step)) (coll/set-ceiling ds query))
      ;; exact match returns itself
      (t/is= (+ start (* 5 step)) (coll/set-ceiling ds (+ start (* 5 step))))
      ;; after all returns nil
      (t/is= nil (coll/set-ceiling ds (+ start (* 20 step)))))))

(t/deftest map-floor-test
  (t/with-instrument `coll/map-floor
    (t/is-spec-check coll/map-floor))
  (t/with-instrument :all
    (let [step tick/ticks-per-day
          start tick/date-2020
          dm (coll/date-range-map start (+ start (* 10 step)) step #(str "d" (/ (- % start) step)))
          query (+ start (long (* 3.5 step)))]
      (t/is= [(+ start (* 3 step)) "d3"] (coll/map-floor dm query))
      (t/is= nil (coll/map-floor dm (- start step))))))

(t/deftest map-ceiling-test
  (t/with-instrument `coll/map-ceiling
    (t/is-spec-check coll/map-ceiling))
  (t/with-instrument :all
    (let [step tick/ticks-per-day
          start tick/date-2020
          dm (coll/date-range-map start (+ start (* 10 step)) step #(str "d" (/ (- % start) step)))
          query (+ start (long (* 3.5 step)))]
      (t/is= [(+ start (* 4 step)) "d4"] (coll/map-ceiling dm query))
      (t/is= nil (coll/map-ceiling dm (+ start (* 20 step)))))))

;;;FILTER OPERATIONS
(t/deftest map-filter-vals-test
  (t/with-instrument `coll/map-filter-vals
    (t/is-spec-check coll/map-filter-vals))
  ;;no instrumentation; Orchestra fspec validation fails with fn args
  (let [step tick/ticks-per-day
        start tick/date-2020
        dm (coll/date-range-map start (+ start (* 10 step)) step
             #(mod (/ (- % start) step) 3))]
    ;; values are [0 1 2 0 1 2 0 1 2 0 1]
    (t/is= 4 (count (coll/map-filter-vals zero? dm)))
    (t/is= 7 (count (coll/map-filter-vals pos? dm)))))

(t/deftest map-filter-keys-test
  (t/with-instrument `coll/map-filter-keys
    (t/is-spec-check coll/map-filter-keys))
  ;;no instrumentation; Orchestra fspec validation fails with fn args
  (let [step tick/ticks-per-day
        start tick/date-2020
        dm (coll/date-range-map start (+ start (* 10 step)) step (constantly :v))]
    ;; filter for even day indices (0, 2, 4, 6, 8, 10) -> 6 elements
    (t/is= 6 (count (coll/map-filter-keys #(even? (/ (- % start) step)) dm)))))
