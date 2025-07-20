(ns provisdom.date.tick-test
  (:require
    [clojure.test :refer :all]
    ; [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.date.tick :as tick]
    [provisdom.math.core :as m]
    [provisdom.test.core :refer :all])
  (:import (java.time Duration)))

;9 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;JAVA-DURATION
(deftest ticks->java-duration-test
  (is (spec-check tick/ticks->java-duration))
  (is= Duration/ZERO
       (tick/ticks->java-duration 0))
  (is= (Duration/ofNanos tick/min-nanos)
       (tick/ticks->java-duration m/min-long))
  (is= (Duration/ofNanos tick/max-nanos)
       (tick/ticks->java-duration m/max-long)))

(deftest java-duration->ticks-by-bounding-test
  (is (spec-check tick/java-duration->ticks-by-bounding))
  (is= 0
       (tick/java-duration->ticks-by-bounding Duration/ZERO))
  (is= m/min-long
       (tick/java-duration->ticks-by-bounding (Duration/ofNanos tick/min-nanos)))
  (is= m/max-long
       (tick/java-duration->ticks-by-bounding (Duration/ofNanos tick/max-nanos)))
  (is= m/max-long
       (tick/java-duration->ticks-by-bounding (Duration/ofNanos m/max-long))))

;;;INSTANT-MS
(deftest date->instant-ms-test
  (is (spec-check tick/date->instant-ms))
  (is= 3155760000000
       (tick/date->instant-ms 0))
  (is= 0
       (tick/date->instant-ms tick/date-1970))
  (is= -4906628144104
       (tick/date->instant-ms m/min-long))
  (is= 11218148144104
       (tick/date->instant-ms m/max-long)))

(deftest instant-ms->date-test
  (is (spec-check tick/instant-ms->date))
  (is= 0
       (tick/instant-ms->date 3155760000000))
  (is= tick/date-1970
       (tick/instant-ms->date 0))
  (is= m/min-long
       (tick/instant-ms->date -4906628144104))
  (is= m/max-long
       (tick/instant-ms->date 11218148144104)))

(deftest ms->instant-ms-by-bounding-test
  (is (spec-check tick/ms->instant-ms-by-bounding))
  (is= -4906628144104 (tick/ms->instant-ms-by-bounding m/min-long))
  (is= 11218148144104 (tick/ms->instant-ms-by-bounding m/max-long))
  (is= 0 (tick/ms->instant-ms-by-bounding 0)))

;;;INSTANT
(deftest date->instant-test
  (is (spec-check tick/date->instant))
  (is= #inst"2070-01-01T00:00:00.000-00:00"
       (tick/date->instant 0))
  (is= #inst"1970-01-01T00:00:00.000-00:00"
       (tick/date->instant tick/date-1970))
  (is= #inst"1814-07-08T07:44:15.896-00:00"
       (tick/date->instant m/min-long))
  (is= #inst"2325-06-28T16:15:44.104-00:00"
       (tick/date->instant m/max-long)))

(deftest instant->date-test
  (is (spec-check tick/instant->date))
  (is= 0
       (tick/instant->date #inst"2070-01-01T00:00:00.000-00:00"))
  (is= tick/date-1970
       (tick/instant->date #inst"1970-01-01T00:00:00.000-00:00"))
  (is= m/min-long
       (tick/instant->date #inst"1814-07-08T07:44:15.896-00:00"))
  (is= m/max-long
       (tick/instant->date #inst"2325-06-28T16:15:44.104-00:00")))

(deftest java-date->instant-by-bounding-test
  (is (spec-check tick/java-date->instant-by-bounding))
  (is= #inst "1814-07-08T07:44:15.896-00:00"
       (tick/java-date->instant-by-bounding #inst"0000-01-01T00:00:00.000-00:00"))
  (is= #inst "2325-06-28T16:15:44.104-00:00"
       (tick/java-date->instant-by-bounding #inst"9999-12-31T23:59:59.999-00:00"))
  (is= #inst "2070-01-01T00:00:00.000-00:00"
       (tick/java-date->instant-by-bounding #inst"2070-01-01T00:00:00.000-00:00")))

;;;TICKS
(deftest ticks->breakdown-test
  (is (spec-check tick/ticks->breakdown))
  (is= #::tick{:weeks 1, :days 6, :hours 15, :minutes 23, :seconds 33,
               :ms    318, :us 504, :ticks 904}
       (tick/ticks->breakdown 1348333636369480))
  (is= {::tick/days 13, ::tick/us 55413318504, ::tick/ticks 904}
       (tick/ticks->breakdown 1348333636369480 #{::tick/days ::tick/us})))

(deftest breakdown->ticks-test
  (is (spec-check tick/breakdown->ticks))
  (is= 1348333636369480
       (tick/breakdown->ticks
         #::tick{:weeks 1, :days 6, :hours 15, :minutes 23, :seconds 33,
                 :ms    318, :us 504, :ticks 904}))
  (is= 1348333636369480
       (tick/breakdown->ticks
         {::tick/days 13, ::tick/us 55413318504, ::tick/ticks 904})))

(deftest format-ticks-test
  (is (spec-check tick/format-ticks))
  (is= "20w01h18m17.923283s"
       (tick/format-ticks {::tick/ticks 13843198424235230 ::tick/fraction-precision 6}))
  (is= "52w1d05h49m12.000000s"
    (tick/format-ticks {::tick/ticks tick/ticks-per-average-year ::tick/fraction-precision 6}))
  (is= "20w01h18m17.9233s"
       (tick/format-ticks {::tick/ticks 13843198424235230 ::tick/fraction-precision 4}))
  (is= "20w01h18m17.92328254s"
       (tick/format-ticks {::tick/ticks 13843198424235230 ::tick/fraction-precision 8}))
  (is= "20w01h18m17.923282543706293s"
       (tick/format-ticks {::tick/ticks 13843198424235230 ::tick/fraction-precision 15}))
  (is= "481w5d09h34m51.375291s"
       (tick/format-ticks {::tick/ticks 333333333333333333 ::tick/fraction-precision 6})))

(deftest parse-ticks-test
  (is (spec-check tick/parse-ticks))
  (is= 13843198424235752
       (tick/parse-ticks "20w01h18m17.923283s"))
  (is= 5374424235752
    (tick/parse-ticks "01h18m17.923283s"))
  (is= 13843198424255200                                    ;precision difference
       (tick/parse-ticks "20w01h18m17.923300s"))
  (is= 13843198424235752                                    ;consistent with new format
       (tick/parse-ticks "20w01h18m17.923283s"))
  (is= 13843198424235752
       (tick/parse-ticks "20w01h18m17.923283s"))
  (is= 1144000000 (tick/parse-ticks "1.000000s"))           ;1 second exactly
  (is= 3333333333615 (tick/parse-ticks "48m33.752914s"))
  (is= 333333333333332903
       (tick/parse-ticks "481w5d09h34m51.375291s"))
  (is= 333333333333000 (tick/parse-ticks "3d08h56m15.291375s")))

;;;MONTHS
(deftest months->breakdown-test
  (is (spec-check tick/months->breakdown))
  (is= {::tick/years  43
        ::tick/months 7}
       (tick/months->breakdown 523)))

(deftest breakdown->months-test
  (is (spec-check tick/breakdown->months))
  (is= 523
       (tick/breakdown->months
         {::tick/years  43
          ::tick/months 7})))

;;;DATE
#_(deftest date$-test
    (is (spec-check tick/date$))
    (is= -1792793997471048000 (tick/date$)))

(deftest date->breakdown-test
  (is (spec-check tick/date->breakdown))
  (is= #::tick{:year         2070
               :month        1
               :day-of-month 1}
       (tick/date->breakdown 0 #{}))
  (is= #::tick{:year         1814
               :month        7
               :day-of-month 8
               :ticks        31867145224192}
       (tick/date->breakdown m/min-long #{}))
  (is= #::tick{:year         2325
               :month        6
               :day-of-month 28
               :ticks        66974454775807}
       (tick/date->breakdown m/max-long #{}))
  (is= #::tick{:year         2066
               :month        3
               :day-of-month 2
               :hours        10
               :minutes      57
               :seconds      0
               :ms           767
               :us           174
               :ticks        641}
       (tick/date->breakdown -138431984242352303))
  (is= #::tick{:year         2066
               :month        3
               :day-of-month 2
               :ticks        45097357647697}
       (tick/date->breakdown -138431984242352303 #{}))
  (is= #::tick{:year         2066
               :month        3
               :day-of-month 2
               :seconds      39420
               :ms           767
               :ticks        199697}
       (tick/date->breakdown -138431984242352303 #{::tick/seconds ::tick/ms}))
  (is= #::tick{:year         2041
               :month        1
               :day-of-month 29}
       (tick/date->breakdown -1044162662400000000 #{}))
  (is= #::tick{:us    0, :month 2, :seconds 0, :day-of-month 29, :year 2024,
               :hours 0, :ticks 0, :minutes 0, :ms 0}
       (tick/date->breakdown -1654904908800000000))
  (is= #::tick{:us    0, :month 3, :seconds 0, :day-of-month 1, :year 2024,
               :hours 0, :ticks 0, :minutes 0, :ms 0}
       (tick/date->breakdown -1654806067200000000)))

(deftest breakdown->date-test
  (is (spec-check tick/breakdown->date))
  (is= 0
       (tick/breakdown->date
         #::tick{:year 2070, :month 1, :day-of-month 1}))
  (is= m/min-long
       (tick/breakdown->date
         #::tick{:year 1814, :month 7, :day-of-month 8, :ticks 31867145224192}))
  (is= m/max-long
       (tick/breakdown->date
         #::tick{:year 2325, :month 6, :day-of-month 28, :ticks 66974454775807}))
  (is= -138431984242352303
       (tick/breakdown->date
         #::tick{:year    2066, :month 3, :day-of-month 2, :hours 10,
                 :minutes 57, :seconds 0, :ms 767, :us 174, :ticks 641}))
  (is= -138431984242352303
       (tick/breakdown->date
         #::tick{:year 2066, :month 3, :day-of-month 2, :ticks 45097357647697}))
  (is= -138431984242352303
       (tick/breakdown->date
         #::tick{:year 2066, :month 3, :day-of-month 2, :seconds 39420,
                 :ms   767, :ticks 199697}))
  (is= -1044162662400000000
       (tick/breakdown->date
         #::tick{:year 2041, :month 1, :day-of-month 29}))
  (is= -1654904908800000000
       (tick/breakdown->date
         #::tick{:us    0, :month 2, :seconds 0, :day-of-month 29, :year 2024,
                 :hours 0, :ticks 0, :minutes 0, :ms 0}))
  (is= -1654806067200000000
       (tick/breakdown->date
         #::tick{:us    0, :month 3, :seconds 0, :day-of-month 1, :year 2024,
                 :hours 0, :ticks 0, :minutes 0, :ms 0})))

(deftest java-date->date-by-bounding-test
  (is (spec-check tick/java-date->date-by-bounding))
  (is= m/min-long
       (tick/java-date->date-by-bounding #inst"0000-01-01T00:00:00.000-00:00"))
  (is= m/max-long
       (tick/java-date->date-by-bounding #inst"9999-12-31T23:59:59.999-00:00"))
  (is= tick/date-2070
       (tick/java-date->date-by-bounding #inst"2070-01-01T00:00:00.000-00:00")))

(deftest date-breakdown?-test
  (is (spec-check tick/date-breakdown?))
  (is (tick/date-breakdown?
        #::tick{:us    0, :month 3, :seconds 0, :day-of-month 1, :year 2024,
                :hours 0, :ticks 0, :minutes 0, :ms 0}))
  (is-not (tick/date-breakdown?
            #::tick{:us    0, :month 2, :seconds 0, :day-of-month 30,
                    :year 2024, :hours 0, :ticks 0, :minutes 0, :ms 0})))

(deftest format-date-test
  (is (spec-check tick/format-date))
  (is= "2031-08-28T13:30:07.671.748:258"
       (tick/format-date -1384319842423520030))
  (is= "2031-08-28T13:30:07.6717"
       (tick/format-date -1384319842423520030 4))
  (is= "2031-08-28T13:30:07.67174823"
       (tick/format-date -1384319842423520030 8))
  (is= "2031-08-28T13:30:07.671748225524476"
       (tick/format-date -1384319842423520030 15)))

(deftest parse-date-test
  (is (spec-check tick/parse-date))
  (is= -1384319842423520030
       (tick/parse-date "2031-08-28T13:30:07.671.748:258"))
  (is= -1384319842423575200                                 ;approx is off by 55170 ticks
       (tick/parse-date "2031-08-28T13:30:07.6717"))
  (is= -1384319842423520025                                 ;approx is off by 5 ticks
       (tick/parse-date "2031-08-28T13:30:07.67174823"))
  (is= -1384319842423520030
       (tick/parse-date "2031-08-28T13:30:07.671748225524476")))

(deftest add-months-to-date-test
  (is (spec-check tick/add-months-to-date))
  (is= #::tick{:year         2024
               :month        7
               :day-of-month 1}
       (tick/date->breakdown
         (tick/add-months-to-date tick/date-2020 54)
         #{}))
  (is= #::tick{:year         2018
               :month        10
               :day-of-month 1}
       (tick/date->breakdown
         (tick/add-months-to-date tick/date-2020 -15)
         #{})))

(deftest day-of-week-test
  (is (spec-check tick/day-of-week))
  (is= :wednesday
       (tick/day-of-week tick/date-2020))
  (is= :friday
       (tick/day-of-week (tick/add-months-to-date tick/date-2020 4))))

(deftest start-of-year-test
  (is (spec-check tick/start-of-year))
  (is= tick/date-2020
       (tick/start-of-year tick/date-2020))
  (is= tick/date-2020
       (tick/start-of-year (tick/add-months-to-date tick/date-2020 4))))

(deftest end-of-year-test
  (is (spec-check tick/end-of-year))
  (is= (tick/add-months-to-date tick/date-2020 12)
       (tick/end-of-year tick/date-2020))
  (is= (tick/add-months-to-date tick/date-2020 12)
       (tick/end-of-year (tick/add-months-to-date tick/date-2020 4))))

(deftest start-of-month-test
  (is (spec-check tick/start-of-month))
  (is= tick/date-2020
       (tick/start-of-month tick/date-2020))
  (is= (tick/add-months-to-date tick/date-2020 4)
       (tick/start-of-month
         (+ (tick/add-months-to-date tick/date-2020 4) 2342478))))

(deftest end-of-month-test
  (is (spec-check tick/end-of-month))
  (is= (tick/add-months-to-date tick/date-2020 1)
       (tick/end-of-month tick/date-2020))
  (is= (tick/add-months-to-date tick/date-2020 5)
       (tick/end-of-month
         (+ (tick/add-months-to-date tick/date-2020 4) 2342478))))

(deftest start-of-day-test
  (is (spec-check tick/start-of-day))
  (is= tick/date-2020
       (tick/start-of-day tick/date-2020))
  (is= tick/date-2020
       (tick/start-of-day (+ tick/date-2020 2342478))))

(deftest end-of-day-test
  (is (spec-check tick/end-of-day))
  (is= (+ tick/date-2020 tick/ticks-per-day)
       (tick/end-of-day tick/date-2020))
  (is= (+ tick/date-2020 tick/ticks-per-day)
       (tick/end-of-day (+ tick/date-2020 2342478))))

(deftest ticks-in-month-test
  (is (spec-check tick/ticks-in-month))
  (is= 3064089600000000
       (tick/ticks-in-month tick/date-2020))
  (is= 2965248000000000
       (tick/ticks-in-month
         (+ tick/date-2020 (* 3 tick/ticks-per-average-month)))))

;;;DATE INTERVALS
(deftest months-difference-test
  (is (spec-check tick/months-difference))
  (is= 77
       (tick/months-difference [73847 234242232323552353]))
  (is= 1
       (tick/months-difference [-2473847 2342423]))
  (is= -1
       (tick/months-difference [2342423 -2473847])))

(deftest date-range->duration-test
  (is (spec-check tick/date-range->duration))
  (is= [77 2656363523478506]
       (tick/date-range->duration [73847 234242232323552353]))
  (is= [1 -3064089595183730]
       (tick/date-range->duration [-2473847 2342423])))

(deftest date-range->months-floor-test
  (is (spec-check tick/date-range->months-floor))
  (is= [77 2656363523478506]
       (tick/date-range->months-floor [73847 234242232323552353]))
  (is= [0 4816270]
       (tick/date-range->months-floor [-2473847 2342423])))

(deftest date-range->months-ceil-test
  (is (spec-check tick/date-range->months-ceil))
  (is= [78 -308884476521494]
       (tick/date-range->months-ceil [73847 234242232323552353]))
  (is= [1 -3064089595183730]
       (tick/date-range->months-ceil [-2473847 2342423])))

(deftest date-range->prorated-months-test
  (is (spec-check tick/date-range->prorated-months))
  (is= 76.89583182367238
    (tick/date-range->prorated-months [73847 234242232323552353]))
  (is= 1.5718437215413022E-9
    (tick/date-range->prorated-months [-2473847 2342423])))

(deftest format-duration-test
  (is (spec-check tick/format-duration))
  (is= "3mo20w01h18m17.923283s"
    (tick/format-duration {::tick/duration [3 13843198424235230]
                           ::tick/show-average-years? false}))
  (is= "1y3mo20w01h18m17.9233s"
    (tick/format-duration {::tick/duration [15 13843198424235230]
                           ::tick/fraction-precision 4
                           ::tick/show-average-years? false}))
  (is= "1y3mo0.3835ay"
    (tick/format-duration {::tick/duration [15 13843198424235230]
                           ::tick/fraction-precision 4
                           ::tick/show-average-years? true}))
  (is= "-3mo20w01h18m17.92328254s"
    (tick/format-duration {::tick/duration [-3 13843198424235230]
                           ::tick/fraction-precision 8
                           ::tick/show-average-years? false}))
  (is= "-1y-3mo20w01h18m17.923282543706293s"
    (tick/format-duration {::tick/duration [-15 13843198424235230]
                           ::tick/fraction-precision 15
                           ::tick/show-average-years? false}))
  (is= "481w5d09h34m51.375291s"
    (tick/format-duration {::tick/duration [0 333333333333333333]
                           ::tick/show-average-years? false}))
  (is= "9.233315ay"
    (tick/format-duration {::tick/duration [0 333333333333333333]})))

(deftest parse-duration-test
  (is (spec-check tick/parse-duration))
  ;; Test round-trip with format-duration
  (is= [3 13843198424235752]
    (tick/parse-duration "3mo20w01h18m17.923283s"))
  (is= [15 13843198424235752]
    (tick/parse-duration "1y3mo20w01h18m17.923283s"))
  (is= [-3 13843198424235752]
    (tick/parse-duration "-3mo20w01h18m17.923283s"))
  (is= [-15 13843198424235752]
    (tick/parse-duration "-1y-3mo20w01h18m17.923283s"))
  (is= [0 333333333333332903]
    (tick/parse-duration "481w5d09h34m51.375291s"))
  ;; Test average years format
  (is= [3 36101153088000000]
    (tick/parse-duration "3mo1.000000ay"))
  (is= [0 72202306176000000]
    (tick/parse-duration "2.000000ay"))
  ;; Test edge cases
  (is= [12 0]
    (tick/parse-duration "1y"))
  (is= [5 0]
    (tick/parse-duration "5mo"))
  (is= [0 0]
    (tick/parse-duration "0.000000ay"))
  ;; Test round-trip compatibility with simple values
  (let [original-duration [15 0]
        formatted (tick/format-duration {::tick/duration original-duration})
        parsed (tick/parse-duration formatted)]
    (is= original-duration parsed))
  ;; Test round-trip with average years (exact values)
  (let [original-duration [3 tick/ticks-per-average-year]
        formatted (tick/format-duration {::tick/duration original-duration 
                                         ::tick/show-average-years? true
                                         ::tick/fraction-precision 6})
        parsed (tick/parse-duration formatted)]
    (is= original-duration parsed)))

;;;AVERAGE YEARS
(deftest ticks->average-years-test
  (is (spec-check tick/ticks->average-years))
  (is= 8.16660631615668E-6
       (tick/ticks->average-years 294823904829))
  (is= -6.852542892382862E-11
       (tick/ticks->average-years -2473847)))

(deftest date-range->average-years-test
  (is (spec-check tick/date-range->average-years))
  (is= 3.3368513762008396E-4
       (tick/date-range->average-years [294823904829 12341242141242]))
  (is= 6.917427246472333E-11
       (tick/date-range->average-years [-2473847 23424])))

;;;PREDICATES
(deftest weekend?-test
  (is (spec-check tick/weekend?))
  (is-not (tick/weekend? tick/date-2020))
  (is (tick/weekend? (tick/breakdown->date {::tick/year         2020
                                            ::tick/month        1
                                            ::tick/day-of-month 4}))))

(deftest weekday?-test
  (is (spec-check tick/weekday?))
  (is (tick/weekday? tick/date-2020))
  (is-not (tick/weekday? (tick/breakdown->date {::tick/year         2020
                                                ::tick/month        1
                                                ::tick/day-of-month 4}))))

(deftest first-day-of-month?-test
  (is (spec-check tick/first-day-of-month?))
  (is (tick/first-day-of-month? tick/date-2020))
  (is-not (tick/first-day-of-month?
            (tick/breakdown->date {::tick/year         2020
                                   ::tick/month        12
                                   ::tick/day-of-month 3}))))

(deftest last-day-of-month?-test
  (is (spec-check tick/last-day-of-month?))
  (is-not (tick/last-day-of-month? tick/date-2020))
  (is (tick/last-day-of-month?
        (tick/breakdown->date {::tick/year         2020
                               ::tick/month        12
                               ::tick/day-of-month 31}))))

(deftest same-day?-test
  (is (spec-check tick/same-day?))
  (is-not (tick/same-day? [tick/date-2020 tick/date-2070]))
  (is (tick/same-day? [tick/date-2020 tick/date-2020]))
  (is-not (tick/same-day? [tick/date-2070 tick/date-2020])))
