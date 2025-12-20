(ns provisdom.date.tick-test
  (:require
    [clojure.test :as ct]
    ; [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.date.tick :as tick]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t])
  (:import (java.time Duration)))

;9 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;JAVA-DURATION
(ct/deftest ticks->java-duration-test
  (t/is-spec-check tick/ticks->java-duration))
  (t/is= Duration/ZERO
       (tick/ticks->java-duration 0))
  (t/is= (Duration/ofNanos tick/min-nanos)
       (tick/ticks->java-duration m/min-long))
  (t/is= (Duration/ofNanos tick/max-nanos)
       (tick/ticks->java-duration m/max-long)))

(ct/deftest java-duration->ticks-by-bounding-test
  (t/is-spec-check tick/java-duration->ticks-by-bounding))
  (t/is= 0
       (tick/java-duration->ticks-by-bounding Duration/ZERO))
  (t/is= m/min-long
       (tick/java-duration->ticks-by-bounding (Duration/ofNanos tick/min-nanos)))
  (t/is= m/max-long
       (tick/java-duration->ticks-by-bounding (Duration/ofNanos tick/max-nanos)))
  (t/is= m/max-long
       (tick/java-duration->ticks-by-bounding (Duration/ofNanos m/max-long))))

;;;INSTANT-MS
(ct/deftest date->instant-ms-test
  (t/is-spec-check tick/date->instant-ms))
  (t/is= 3155760000000
       (tick/date->instant-ms 0))
  (t/is= 0
       (tick/date->instant-ms tick/date-1970))
  (t/is= -4906628144104
       (tick/date->instant-ms m/min-long))
  (t/is= 11218148144104
       (tick/date->instant-ms m/max-long)))

(ct/deftest instant-ms->date-test
  (t/is-spec-check tick/instant-ms->date))
  (t/is= 0
       (tick/instant-ms->date 3155760000000))
  (t/is= tick/date-1970
       (tick/instant-ms->date 0))
  (t/is= m/min-long
       (tick/instant-ms->date -4906628144104))
  (t/is= m/max-long
       (tick/instant-ms->date 11218148144104)))

(ct/deftest ms->instant-ms-by-bounding-test
  (t/is-spec-check tick/ms->instant-ms-by-bounding))
  (t/is= -4906628144104 (tick/ms->instant-ms-by-bounding m/min-long))
  (t/is= 11218148144104 (tick/ms->instant-ms-by-bounding m/max-long))
  (t/is= 0 (tick/ms->instant-ms-by-bounding 0)))

;;;INSTANT
(ct/deftest date->instant-test
  (t/is-spec-check tick/date->instant))
  (t/is= #inst"2070-01-01T00:00:00.000-00:00"
       (tick/date->instant 0))
  (t/is= #inst"1970-01-01T00:00:00.000-00:00"
       (tick/date->instant tick/date-1970))
  (t/is= #inst"1814-07-08T07:44:15.896-00:00"
       (tick/date->instant m/min-long))
  (t/is= #inst"2325-06-28T16:15:44.104-00:00"
       (tick/date->instant m/max-long)))

(ct/deftest instant->date-test
  (t/is-spec-check tick/instant->date))
  (t/is= 0
       (tick/instant->date #inst"2070-01-01T00:00:00.000-00:00"))
  (t/is= tick/date-1970
       (tick/instant->date #inst"1970-01-01T00:00:00.000-00:00"))
  (t/is= m/min-long
       (tick/instant->date #inst"1814-07-08T07:44:15.896-00:00"))
  (t/is= m/max-long
       (tick/instant->date #inst"2325-06-28T16:15:44.104-00:00")))

(ct/deftest java-date->instant-by-bounding-test
  (t/is-spec-check tick/java-date->instant-by-bounding))
  (t/is= #inst "1814-07-08T07:44:15.896-00:00"
       (tick/java-date->instant-by-bounding #inst"0000-01-01T00:00:00.000-00:00"))
  (t/is= #inst "2325-06-28T16:15:44.104-00:00"
       (tick/java-date->instant-by-bounding #inst"9999-12-31T23:59:59.999-00:00"))
  (t/is= #inst "2070-01-01T00:00:00.000-00:00"
       (tick/java-date->instant-by-bounding #inst"2070-01-01T00:00:00.000-00:00")))

;;;TICKS
(ct/deftest ticks->breakdown-test
  (t/is-spec-check tick/ticks->breakdown))
  (t/is= #::tick{:weeks 1, :days 6, :hours 15, :minutes 23, :seconds 33,
               :ms    318, :us 504, :ticks 904}
       (tick/ticks->breakdown 1348333636369480))
  (t/is= {::tick/days 13, ::tick/us 55413318504, ::tick/ticks 904}
       (tick/ticks->breakdown 1348333636369480 #{::tick/days ::tick/us})))

(ct/deftest breakdown->ticks-test
  (t/is-spec-check tick/breakdown->ticks))
  (t/is= 1348333636369480
       (tick/breakdown->ticks
         #::tick{:weeks 1, :days 6, :hours 15, :minutes 23, :seconds 33,
                 :ms    318, :us 504, :ticks 904}))
  (t/is= 1348333636369480
       (tick/breakdown->ticks
         {::tick/days 13, ::tick/us 55413318504, ::tick/ticks 904})))

(ct/deftest format-ticks-test
  (t/is-spec-check tick/format-ticks))
  ;; Test detailed time format (show-average-years? false)
  (t/is= "20w01h18m17.923283s"
       (tick/format-ticks {::tick/fraction-precision 6
                           ::tick/show-average-years? false
                           ::tick/ticks 13843198424235230}))
  (t/is= "52w1d05h49m12.000000s"
    (tick/format-ticks {::tick/fraction-precision 6
                        ::tick/show-average-years? false
                        ::tick/ticks tick/ticks-per-average-year}))
  (t/is= "20w01h18m17.9233s"
       (tick/format-ticks {::tick/fraction-precision 4
                           ::tick/show-average-years? false
                           ::tick/ticks 13843198424235230}))
  (t/is= "20w01h18m17.92328254s"
       (tick/format-ticks {::tick/fraction-precision 8
                           ::tick/show-average-years? false
                           ::tick/ticks 13843198424235230}))
  (t/is= "20w01h18m17.923282543706293s"
       (tick/format-ticks {::tick/fraction-precision 15
                           ::tick/show-average-years? false
                           ::tick/ticks 13843198424235230}))
  (t/is= "481w5d09h34m51.375291s"
       (tick/format-ticks {::tick/fraction-precision 6
                           ::tick/show-average-years? false
                           ::tick/ticks 333333333333333333}))
  ;; Test average years format (default show-average-years? true)
  (t/is= "0.383456ay"
       (tick/format-ticks {::tick/fraction-precision 6
                           ::tick/ticks 13843198424235230}))
  (t/is= "1.000000ay"
       (tick/format-ticks {::tick/fraction-precision 6
                           ::tick/ticks tick/ticks-per-average-year}))
  (t/is= "0.3835ay"
       (tick/format-ticks {::tick/fraction-precision 4
                           ::tick/ticks 13843198424235230}))
  (t/is= "9.23331542ay"
       (tick/format-ticks {::tick/fraction-precision 8
                           ::tick/ticks 333333333333333333})))

(ct/deftest parse-ticks-test
  (t/is-spec-check tick/parse-ticks))
  ;; Test detailed time format
  (t/is= 13843198424235752
       (tick/parse-ticks "20w01h18m17.923283s"))
  (t/is= 5374424235752
    (tick/parse-ticks "01h18m17.923283s"))
  (t/is= 13843198424255200                                    ;precision difference
       (tick/parse-ticks "20w01h18m17.923300s"))
  (t/is= 13843198424235752                                    ;consistent with new format
       (tick/parse-ticks "20w01h18m17.923283s"))
  (t/is= 13843198424235752
       (tick/parse-ticks "20w01h18m17.923283s"))
  (t/is= 1144000000 (tick/parse-ticks "1.000000s"))           ;1 second exactly
  (t/is= 3333333333615 (tick/parse-ticks "48m33.752914s"))
  (t/is= 333333333333332903
       (tick/parse-ticks "481w5d09h34m51.375291s"))
  (t/is= 333333333333000 (tick/parse-ticks "3d08h56m15.291375s"))
  ;; Test average years format
  (t/is= tick/ticks-per-average-year (tick/parse-ticks "1.000000ay"))
  (t/is= 36101153088000000 (tick/parse-ticks "1ay"))
  (t/is= 13845766940381376 (tick/parse-ticks "0.383527ay"))
  (t/is= 333333318324726720 (tick/parse-ticks "9.233315ay"))
  ;; Test round-trip compatibility (default show-average-years? true)
  (let [original-ticks 13843198424235230
        formatted (tick/format-ticks {::tick/ticks original-ticks})
        parsed (tick/parse-ticks formatted)]
    (t/is= 13843203758512128 parsed))
  ;; Test round-trip compatibility (show-average-years? false)
  (let [original-ticks 13843198424235230
        formatted (tick/format-ticks {::tick/show-average-years? false
                                      ::tick/ticks original-ticks})
        parsed (tick/parse-ticks formatted)]
    (t/is= 13843198424235752 parsed)))

;;;MONTHS
(ct/deftest months->breakdown-test
  (t/is-spec-check tick/months->breakdown))
  (t/is= {::tick/years  43
        ::tick/months 7}
       (tick/months->breakdown 523)))

(ct/deftest breakdown->months-test
  (t/is-spec-check tick/breakdown->months))
  (t/is= 523
       (tick/breakdown->months
         {::tick/years  43
          ::tick/months 7})))

;;;DATE
#_(ct/deftest date$-test
    (t/is-spec-check tick/date$))
    (t/is= -1792793997471048000 (tick/date$)))

(ct/deftest date->breakdown-test
  (t/is-spec-check tick/date->breakdown))
  (t/is= #::tick{:year         2070
               :month        1
               :day-of-month 1}
       (tick/date->breakdown 0 #{}))
  (t/is= #::tick{:year         1814
               :month        7
               :day-of-month 8
               :ticks        31867145224192}
       (tick/date->breakdown m/min-long #{}))
  (t/is= #::tick{:year         2325
               :month        6
               :day-of-month 28
               :ticks        66974454775807}
       (tick/date->breakdown m/max-long #{}))
  (t/is= #::tick{:year         2066
               :month        3
               :day-of-month 2
               :hours        10
               :minutes      57
               :seconds      0
               :ms           767
               :us           174
               :ticks        641}
       (tick/date->breakdown -138431984242352303))
  (t/is= #::tick{:year         2066
               :month        3
               :day-of-month 2
               :ticks        45097357647697}
       (tick/date->breakdown -138431984242352303 #{}))
  (t/is= #::tick{:year         2066
               :month        3
               :day-of-month 2
               :seconds      39420
               :ms           767
               :ticks        199697}
       (tick/date->breakdown -138431984242352303 #{::tick/seconds ::tick/ms}))
  (t/is= #::tick{:year         2041
               :month        1
               :day-of-month 29}
       (tick/date->breakdown -1044162662400000000 #{}))
  (t/is= #::tick{:us    0, :month 2, :seconds 0, :day-of-month 29, :year 2024,
               :hours 0, :ticks 0, :minutes 0, :ms 0}
       (tick/date->breakdown -1654904908800000000))
  (t/is= #::tick{:us    0, :month 3, :seconds 0, :day-of-month 1, :year 2024,
               :hours 0, :ticks 0, :minutes 0, :ms 0}
       (tick/date->breakdown -1654806067200000000)))

(ct/deftest breakdown->date-test
  (t/is-spec-check tick/breakdown->date))
  (t/is= 0
       (tick/breakdown->date
         #::tick{:year 2070, :month 1, :day-of-month 1}))
  (t/is= m/min-long
       (tick/breakdown->date
         #::tick{:year 1814, :month 7, :day-of-month 8, :ticks 31867145224192}))
  (t/is= m/max-long
       (tick/breakdown->date
         #::tick{:year 2325, :month 6, :day-of-month 28, :ticks 66974454775807}))
  (t/is= -138431984242352303
       (tick/breakdown->date
         #::tick{:year    2066, :month 3, :day-of-month 2, :hours 10,
                 :minutes 57, :seconds 0, :ms 767, :us 174, :ticks 641}))
  (t/is= -138431984242352303
       (tick/breakdown->date
         #::tick{:year 2066, :month 3, :day-of-month 2, :ticks 45097357647697}))
  (t/is= -138431984242352303
       (tick/breakdown->date
         #::tick{:year 2066, :month 3, :day-of-month 2, :seconds 39420,
                 :ms   767, :ticks 199697}))
  (t/is= -1044162662400000000
       (tick/breakdown->date
         #::tick{:year 2041, :month 1, :day-of-month 29}))
  (t/is= -1654904908800000000
       (tick/breakdown->date
         #::tick{:us    0, :month 2, :seconds 0, :day-of-month 29, :year 2024,
                 :hours 0, :ticks 0, :minutes 0, :ms 0}))
  (t/is= -1654806067200000000
       (tick/breakdown->date
         #::tick{:us    0, :month 3, :seconds 0, :day-of-month 1, :year 2024,
                 :hours 0, :ticks 0, :minutes 0, :ms 0})))

(ct/deftest java-date->date-by-bounding-test
  (t/is-spec-check tick/java-date->date-by-bounding))
  (t/is= m/min-long
       (tick/java-date->date-by-bounding #inst"0000-01-01T00:00:00.000-00:00"))
  (t/is= m/max-long
       (tick/java-date->date-by-bounding #inst"9999-12-31T23:59:59.999-00:00"))
  (t/is= tick/date-2070
       (tick/java-date->date-by-bounding #inst"2070-01-01T00:00:00.000-00:00")))

(ct/deftest date-breakdown?-test
  (t/is-spec-check tick/date-breakdown?))
  (t/is (tick/date-breakdown?
        #::tick{:us    0, :month 3, :seconds 0, :day-of-month 1, :year 2024,
                :hours 0, :ticks 0, :minutes 0, :ms 0}))
  (t/is-not (tick/date-breakdown?
            #::tick{:us    0, :month 2, :seconds 0, :day-of-month 30,
                    :year 2024, :hours 0, :ticks 0, :minutes 0, :ms 0})))

(ct/deftest format-date-test
  (t/is-spec-check tick/format-date))
  (t/is= "2031-08-28T13:30:07.671.748:258"
       (tick/format-date -1384319842423520030))
  (t/is= "2031-08-28T13:30:07.6717"
       (tick/format-date -1384319842423520030 4))
  (t/is= "2031-08-28T13:30:07.67174823"
       (tick/format-date -1384319842423520030 8))
  (t/is= "2031-08-28T13:30:07.671748225524476"
       (tick/format-date -1384319842423520030 15)))

(ct/deftest parse-date-test
  (t/is-spec-check tick/parse-date))
  (t/is= -1384319842423520030
       (tick/parse-date "2031-08-28T13:30:07.671.748:258"))
  (t/is= -1384319842423575200                                 ;approx is off by 55170 ticks
       (tick/parse-date "2031-08-28T13:30:07.6717"))
  (t/is= -1384319842423520025                                 ;approx is off by 5 ticks
       (tick/parse-date "2031-08-28T13:30:07.67174823"))
  (t/is= -1384319842423520030
       (tick/parse-date "2031-08-28T13:30:07.671748225524476")))

(ct/deftest add-months-to-date-test
  (t/is-spec-check tick/add-months-to-date))
  (t/is= #::tick{:year         2024
               :month        7
               :day-of-month 1}
       (tick/date->breakdown
         (tick/add-months-to-date tick/date-2020 54)
         #{}))
  (t/is= #::tick{:year         2018
               :month        10
               :day-of-month 1}
       (tick/date->breakdown
         (tick/add-months-to-date tick/date-2020 -15)
         #{})))

(ct/deftest day-of-week-test
  (t/is-spec-check tick/day-of-week))
  (t/is= :wednesday
       (tick/day-of-week tick/date-2020))
  (t/is= :friday
       (tick/day-of-week (tick/add-months-to-date tick/date-2020 4))))

(ct/deftest start-of-year-test
  (t/is-spec-check tick/start-of-year))
  (t/is= tick/date-2020
       (tick/start-of-year tick/date-2020))
  (t/is= tick/date-2020
       (tick/start-of-year (tick/add-months-to-date tick/date-2020 4))))

(ct/deftest end-of-year-test
  (t/is-spec-check tick/end-of-year))
  (t/is= (tick/add-months-to-date tick/date-2020 12)
       (tick/end-of-year tick/date-2020))
  (t/is= (tick/add-months-to-date tick/date-2020 12)
       (tick/end-of-year (tick/add-months-to-date tick/date-2020 4))))

(ct/deftest start-of-month-test
  (t/is-spec-check tick/start-of-month))
  (t/is= tick/date-2020
       (tick/start-of-month tick/date-2020))
  (t/is= (tick/add-months-to-date tick/date-2020 4)
       (tick/start-of-month
         (+ (tick/add-months-to-date tick/date-2020 4) 2342478))))

(ct/deftest end-of-month-test
  (t/is-spec-check tick/end-of-month))
  (t/is= (tick/add-months-to-date tick/date-2020 1)
       (tick/end-of-month tick/date-2020))
  (t/is= (tick/add-months-to-date tick/date-2020 5)
       (tick/end-of-month
         (+ (tick/add-months-to-date tick/date-2020 4) 2342478))))

(ct/deftest start-of-day-test
  (t/is-spec-check tick/start-of-day))
  (t/is= tick/date-2020
       (tick/start-of-day tick/date-2020))
  (t/is= tick/date-2020
       (tick/start-of-day (+ tick/date-2020 2342478))))

(ct/deftest end-of-day-test
  (t/is-spec-check tick/end-of-day))
  (t/is= (+ tick/date-2020 tick/ticks-per-day)
       (tick/end-of-day tick/date-2020))
  (t/is= (+ tick/date-2020 tick/ticks-per-day)
       (tick/end-of-day (+ tick/date-2020 2342478))))

(ct/deftest ticks-in-month-test
  (t/is-spec-check tick/ticks-in-month))
  (t/is= 3064089600000000
       (tick/ticks-in-month tick/date-2020))
  (t/is= 2965248000000000
       (tick/ticks-in-month
         (+ tick/date-2020 (* 3 tick/ticks-per-average-month)))))

;;;DATE INTERVALS
(ct/deftest months-difference-test
  (t/is-spec-check tick/months-difference))
  (t/is= 77
       (tick/months-difference [73847 234242232323552353]))
  (t/is= 1
       (tick/months-difference [-2473847 2342423]))
  (t/is= -1
       (tick/months-difference [2342423 -2473847])))

(ct/deftest date-range->duration-test
  (t/is-spec-check tick/date-range->duration))
  (t/is= [77 2656363523478506]
       (tick/date-range->duration [73847 234242232323552353]))
  (t/is= [1 -3064089595183730]
       (tick/date-range->duration [-2473847 2342423])))

(ct/deftest date-range->months-floor-test
  (t/is-spec-check tick/date-range->months-floor))
  (t/is= [77 2656363523478506]
       (tick/date-range->months-floor [73847 234242232323552353]))
  (t/is= [0 4816270]
       (tick/date-range->months-floor [-2473847 2342423])))

(ct/deftest date-range->months-ceil-test
  (t/is-spec-check tick/date-range->months-ceil))
  (t/is= [78 -308884476521494]
       (tick/date-range->months-ceil [73847 234242232323552353]))
  (t/is= [1 -3064089595183730]
       (tick/date-range->months-ceil [-2473847 2342423])))

(ct/deftest date-range->prorated-months-test
  (t/is-spec-check tick/date-range->prorated-months))
  (t/is= 76.89583182367238
    (tick/date-range->prorated-months [73847 234242232323552353]))
  (t/is= 1.5718437215413022E-9
    (tick/date-range->prorated-months [-2473847 2342423])))

(ct/deftest format-duration-test
  (t/is-spec-check tick/format-duration))
  (t/is= "3mo20w01h18m17.923283s"
    (tick/format-duration {::tick/duration [3 13843198424235230]
                           ::tick/show-average-years? false}))
  (t/is= "1y3mo20w01h18m17.9233s"
    (tick/format-duration {::tick/duration [15 13843198424235230]
                           ::tick/fraction-precision 4
                           ::tick/show-average-years? false}))
  (t/is= "1y3mo0.3835ay"
    (tick/format-duration {::tick/duration [15 13843198424235230]
                           ::tick/fraction-precision 4
                           ::tick/show-average-years? true}))
  (t/is= "-3mo20w01h18m17.92328254s"
    (tick/format-duration {::tick/duration [-3 13843198424235230]
                           ::tick/fraction-precision 8
                           ::tick/show-average-years? false}))
  (t/is= "-1y-3mo20w01h18m17.923282543706293s"
    (tick/format-duration {::tick/duration [-15 13843198424235230]
                           ::tick/fraction-precision 15
                           ::tick/show-average-years? false}))
  (t/is= "481w5d09h34m51.375291s"
    (tick/format-duration {::tick/duration [0 333333333333333333]
                           ::tick/show-average-years? false}))
  (t/is= "9.233315ay"
    (tick/format-duration {::tick/duration [0 333333333333333333]})))

(ct/deftest parse-duration-test
  (t/is-spec-check tick/parse-duration))
  ;; Test round-trip with format-duration
  (t/is= [3 13843198424235752]
    (tick/parse-duration "3mo20w01h18m17.923283s"))
  (t/is= [15 13843198424235752]
    (tick/parse-duration "1y3mo20w01h18m17.923283s"))
  (t/is= [-3 13843198424235752]
    (tick/parse-duration "-3mo20w01h18m17.923283s"))
  (t/is= [-15 13843198424235752]
    (tick/parse-duration "-1y-3mo20w01h18m17.923283s"))
  (t/is= [0 333333333333332903]
    (tick/parse-duration "481w5d09h34m51.375291s"))
  ;; Test average years format
  (t/is= [3 36101153088000000]
    (tick/parse-duration "3mo1.000000ay"))
  (t/is= [0 72202306176000000]
    (tick/parse-duration "2.000000ay"))
  ;; Test edge cases
  (t/is= [12 0]
    (tick/parse-duration "1y"))
  (t/is= [5 0]
    (tick/parse-duration "5mo"))
  (t/is= [0 0]
    (tick/parse-duration "0.000000ay"))
  ;; Test round-trip compatibility with simple values
  (let [original-duration [15 0]
        formatted (tick/format-duration {::tick/duration original-duration})
        parsed (tick/parse-duration formatted)]
    (t/is= original-duration parsed))
  ;; Test round-trip with average years (exact values)
  (let [original-duration [3 tick/ticks-per-average-year]
        formatted (tick/format-duration {::tick/duration original-duration 
                                         ::tick/show-average-years? true
                                         ::tick/fraction-precision 6})
        parsed (tick/parse-duration formatted)]
    (t/is= original-duration parsed)))

;;;AVERAGE YEARS
(ct/deftest ticks->average-years-test
  (t/is-spec-check tick/ticks->average-years))
  (t/is= 8.16660631615668E-6
       (tick/ticks->average-years 294823904829))
  (t/is= -6.852542892382862E-11
       (tick/ticks->average-years -2473847)))

(ct/deftest date-range->average-years-test
  (t/is-spec-check tick/date-range->average-years))
  (t/is= 3.3368513762008396E-4
       (tick/date-range->average-years [294823904829 12341242141242]))
  (t/is= 6.917427246472333E-11
       (tick/date-range->average-years [-2473847 23424])))

;;;PREDICATES
(ct/deftest weekend?-test
  (t/is-spec-check tick/weekend?))
  (t/is-not (tick/weekend? tick/date-2020))
  (t/is (tick/weekend? (tick/breakdown->date {::tick/year         2020
                                            ::tick/month        1
                                            ::tick/day-of-month 4}))))

(ct/deftest weekday?-test
  (t/is-spec-check tick/weekday?))
  (t/is (tick/weekday? tick/date-2020))
  (t/is-not (tick/weekday? (tick/breakdown->date {::tick/year         2020
                                                ::tick/month        1
                                                ::tick/day-of-month 4}))))

(ct/deftest first-day-of-month?-test
  (t/is-spec-check tick/first-day-of-month?))
  (t/is (tick/first-day-of-month? tick/date-2020))
  (t/is-not (tick/first-day-of-month?
            (tick/breakdown->date {::tick/year         2020
                                   ::tick/month        12
                                   ::tick/day-of-month 3}))))

(ct/deftest last-day-of-month?-test
  (t/is-spec-check tick/last-day-of-month?))
  (t/is-not (tick/last-day-of-month? tick/date-2020))
  (t/is (tick/last-day-of-month?
        (tick/breakdown->date {::tick/year         2020
                               ::tick/month        12
                               ::tick/day-of-month 31}))))

(ct/deftest same-day?-test
  (t/is-spec-check tick/same-day?))
  (t/is-not (tick/same-day? [tick/date-2020 tick/date-2070]))
  (t/is (tick/same-day? [tick/date-2020 tick/date-2020]))
  (t/is-not (tick/same-day? [tick/date-2070 tick/date-2020])))
