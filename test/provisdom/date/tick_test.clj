(ns provisdom.date.tick-test
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.date.tick :as tick]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t])
  (:import [java.time Duration]))

;;6 seconds

(set! *warn-on-reflection* true)

(t/deftest date-spec-test
  ;; No spec-check for macros (spec-generating function)
  (s/def ::test-date-spec (tick/date-spec {:date-max tick/date-2070 :date-min tick/date-2020}))
  (t/is (s/valid? ::test-date-spec tick/date-2045))
  (t/is-not (s/valid? ::test-date-spec tick/date-1970)))

(t/deftest ticks-spec-test
  ;; No spec-check for macros (spec-generating function)
  (s/def ::test-ticks-spec (tick/ticks-spec {:ticks-max 1000 :ticks-min -1000}))
  (t/is (s/valid? ::test-ticks-spec 500))
  (t/is-not (s/valid? ::test-ticks-spec 2000)))

(t/deftest ticks-non--spec-test
  ;; No spec-check for macros (spec-generating function)
  (s/def ::test-ticks-non--spec (tick/ticks-non--spec 1000))
  (t/is (s/valid? ::test-ticks-non--spec 0))
  (t/is (s/valid? ::test-ticks-non--spec 500))
  (t/is-not (s/valid? ::test-ticks-non--spec -1))
  (t/is-not (s/valid? ::test-ticks-non--spec 1001)))

(t/deftest ticks+-spec-test
  ;; No spec-check for macros (spec-generating function)
  (s/def ::test-ticks+-spec (tick/ticks+-spec 1000))
  (t/is (s/valid? ::test-ticks+-spec 1))
  (t/is (s/valid? ::test-ticks+-spec 500))
  (t/is-not (s/valid? ::test-ticks+-spec 0))
  (t/is-not (s/valid? ::test-ticks+-spec 1001)))

(t/deftest instant-in-range?-test
  (t/with-instrument `tick/instant-in-range?
    (t/is-spec-check `tick/instant-in-range?))
  (t/with-instrument :all
    (t/is (tick/instant-in-range? #inst"2020-01-01T00:00:00.000-00:00"))
    (t/is (tick/instant-in-range? #inst"1970-01-01T00:00:00.000-00:00"))
    (t/is-not (tick/instant-in-range? #inst"0001-01-01T00:00:00.000-00:00"))
    (t/is-not (tick/instant-in-range? #inst"9999-12-31T23:59:59.999-00:00"))))

;;;JAVA DURATION
(t/deftest ticks->java-duration-test
  (t/with-instrument `tick/ticks->java-duration
    (t/is-spec-check tick/ticks->java-duration))
  (t/with-instrument :all
    (t/is= Duration/ZERO (tick/ticks->java-duration 0))
    (t/is= (Duration/ofNanos tick/min-nanos) (tick/ticks->java-duration m/min-long))
    (t/is= (Duration/ofNanos tick/max-nanos) (tick/ticks->java-duration m/max-long))))

(t/deftest java-duration->ticks-by-bounding-test
  (t/with-instrument `tick/java-duration->ticks-by-bounding
    (t/is-spec-check tick/java-duration->ticks-by-bounding))
  (t/with-instrument :all
    (t/is= 0 (tick/java-duration->ticks-by-bounding Duration/ZERO))
    (t/is= m/min-long (tick/java-duration->ticks-by-bounding (Duration/ofNanos tick/min-nanos)))
    (t/is= m/max-long (tick/java-duration->ticks-by-bounding (Duration/ofNanos tick/max-nanos)))
    (t/is= m/max-long (tick/java-duration->ticks-by-bounding (Duration/ofNanos m/max-long)))))

;;;INSTANT-MS
(t/deftest date->instant-ms-test
  (t/with-instrument `tick/date->instant-ms
    (t/is-spec-check tick/date->instant-ms))
  (t/with-instrument :all
    (t/is= 3155760000000 (tick/date->instant-ms 0))
    (t/is= 0 (tick/date->instant-ms tick/date-1970))
    (t/is= -4906628144104 (tick/date->instant-ms m/min-long))
    (t/is= 11218148144104 (tick/date->instant-ms m/max-long))))

(t/deftest instant-ms->date-test
  (t/with-instrument `tick/instant-ms->date
    (t/is-spec-check tick/instant-ms->date))
  (t/with-instrument :all
    (t/is= 0 (tick/instant-ms->date 3155760000000))
    (t/is= tick/date-1970 (tick/instant-ms->date 0))
    (t/is= m/min-long (tick/instant-ms->date -4906628144104))
    (t/is= m/max-long (tick/instant-ms->date 11218148144104))))

(t/deftest ms->instant-ms-by-bounding-test
  (t/with-instrument `tick/ms->instant-ms-by-bounding
    (t/is-spec-check tick/ms->instant-ms-by-bounding))
  (t/with-instrument :all
    (t/is= -4906628144104 (tick/ms->instant-ms-by-bounding m/min-long))
    (t/is= 11218148144104 (tick/ms->instant-ms-by-bounding m/max-long))
    (t/is= 0 (tick/ms->instant-ms-by-bounding 0))))

;;;INSTANT
(t/deftest date->instant-test
  (t/with-instrument `tick/date->instant
    (t/is-spec-check tick/date->instant))
  (t/with-instrument :all
    (t/is= #inst"2070-01-01T00:00:00.000-00:00" (tick/date->instant 0))
    (t/is= #inst"1970-01-01T00:00:00.000-00:00" (tick/date->instant tick/date-1970))
    (t/is= #inst"1814-07-08T07:44:15.896-00:00" (tick/date->instant m/min-long))
    (t/is= #inst"2325-06-28T16:15:44.104-00:00" (tick/date->instant m/max-long))))

(t/deftest instant->date-test
  (t/with-instrument `tick/instant->date
    (t/is-spec-check tick/instant->date))
  (t/with-instrument :all
    (t/is= 0 (tick/instant->date #inst"2070-01-01T00:00:00.000-00:00"))
    (t/is= tick/date-1970 (tick/instant->date #inst"1970-01-01T00:00:00.000-00:00"))
    (t/is= m/min-long (tick/instant->date #inst"1814-07-08T07:44:15.896-00:00"))
    (t/is= m/max-long (tick/instant->date #inst"2325-06-28T16:15:44.104-00:00"))))

(t/deftest java-date->instant-by-bounding-test
  (t/with-instrument `tick/java-date->instant-by-bounding
    (t/is-spec-check tick/java-date->instant-by-bounding))
  (t/with-instrument :all
    (t/is= #inst "1814-07-08T07:44:15.896-00:00"
      (tick/java-date->instant-by-bounding #inst"0000-01-01T00:00:00.000-00:00"))
    (t/is= #inst "2325-06-28T16:15:44.104-00:00"
      (tick/java-date->instant-by-bounding #inst"9999-12-31T23:59:59.999-00:00"))
    (t/is= #inst "2070-01-01T00:00:00.000-00:00"
      (tick/java-date->instant-by-bounding #inst"2070-01-01T00:00:00.000-00:00"))))

;;;TICKS
(t/deftest ticks->breakdown-test
  (t/with-instrument `tick/ticks->breakdown
    (t/is-spec-check tick/ticks->breakdown))
  (t/with-instrument :all
    (t/is= #::tick{:days 6, :hours 15, :minutes 23, :ms 318, :seconds 33, :ticks 904, :us 504,
                   :weeks 1}
      (tick/ticks->breakdown 1348333636369480))
    (t/is= {::tick/days 13, ::tick/ticks 904, ::tick/us 55413318504}
      (tick/ticks->breakdown 1348333636369480 #{::tick/days ::tick/us}))))

(t/deftest breakdown->ticks-test
  (t/with-instrument `tick/breakdown->ticks
    (t/is-spec-check tick/breakdown->ticks))
  (t/with-instrument :all
    (t/is= 1348333636369480
      (tick/breakdown->ticks
        #::tick{:days 6, :hours 15, :minutes 23, :ms 318, :seconds 33, :ticks 904, :us 504,
                :weeks 1}))
    (t/is= 1348333636369480
      (tick/breakdown->ticks {::tick/days 13, ::tick/ticks 904, ::tick/us 55413318504}))))

(t/deftest format-ticks-test
  (t/with-instrument `tick/format-ticks
    (t/is-spec-check tick/format-ticks))
  (t/with-instrument :all
    ;; Test detailed time format (show-average-years? false)
    (t/is= "20w01h18m17.923283s"
      (tick/format-ticks {::tick/fraction-precision  6
                          ::tick/show-average-years? false
                          ::tick/ticks               13843198424235230}))
    (t/is= "52w1d05h49m12.000000s"
      (tick/format-ticks {::tick/fraction-precision  6
                          ::tick/show-average-years? false
                          ::tick/ticks               tick/ticks-per-average-year}))
    (t/is= "20w01h18m17.9233s"
      (tick/format-ticks {::tick/fraction-precision  4
                          ::tick/show-average-years? false
                          ::tick/ticks               13843198424235230}))
    (t/is= "20w01h18m17.92328254s"
      (tick/format-ticks {::tick/fraction-precision  8
                          ::tick/show-average-years? false
                          ::tick/ticks               13843198424235230}))
    (t/is= "20w01h18m17.923282543706293s"
      (tick/format-ticks {::tick/fraction-precision  15
                          ::tick/show-average-years? false
                          ::tick/ticks               13843198424235230}))
    (t/is= "481w5d09h34m51.375291s"
      (tick/format-ticks {::tick/fraction-precision  6
                          ::tick/show-average-years? false
                          ::tick/ticks               333333333333333333}))
    ;; Test average years format (default show-average-years? true)
    (t/is= "0.383456ay"
      (tick/format-ticks {::tick/fraction-precision 6
                          ::tick/ticks              13843198424235230}))
    (t/is= "1.000000ay"
      (tick/format-ticks {::tick/fraction-precision 6
                          ::tick/ticks              tick/ticks-per-average-year}))
    (t/is= "0.3835ay"
      (tick/format-ticks {::tick/fraction-precision 4
                          ::tick/ticks              13843198424235230}))
    (t/is= "9.23331542ay"
      (tick/format-ticks {::tick/fraction-precision 8
                          ::tick/ticks              333333333333333333}))))

(t/deftest parse-ticks-test
  (t/with-instrument `tick/parse-ticks
    (t/is-spec-check tick/parse-ticks))
  (t/with-instrument :all
    ;; Test detailed time format
    (t/is= 13843198424235752 (tick/parse-ticks "20w01h18m17.923283s"))
    (t/is= 5374424235752 (tick/parse-ticks "01h18m17.923283s"))
    ;;precision difference
    (t/is= 13843198424255200 (tick/parse-ticks "20w01h18m17.923300s"))
    ;;consistent with new format
    (t/is= 13843198424235752 (tick/parse-ticks "20w01h18m17.923283s"))
    (t/is= 13843198424235752 (tick/parse-ticks "20w01h18m17.923283s"))
    (t/is= 1144000000 (tick/parse-ticks "1.000000s"))       ;1 second exactly
    (t/is= 3333333333615 (tick/parse-ticks "48m33.752914s"))
    (t/is= 333333333333332903 (tick/parse-ticks "481w5d09h34m51.375291s"))
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
                                        ::tick/ticks               original-ticks})
          parsed (tick/parse-ticks formatted)]
      (t/is= 13843198424235752 parsed))))

;;;MONTHS
(t/deftest months->breakdown-test
  (t/with-instrument `tick/months->breakdown
    (t/is-spec-check tick/months->breakdown))
  (t/with-instrument :all
    (t/is= {::tick/months 7
            ::tick/years  43}
      (tick/months->breakdown 523))))

(t/deftest breakdown->months-test
  (t/with-instrument `tick/breakdown->months
    (t/is-spec-check tick/breakdown->months))
  (t/with-instrument :all
    (t/is= 523
      (tick/breakdown->months
        {::tick/months 7
         ::tick/years  43}))))

;;;DATES
(t/deftest date$-test
  (t/with-instrument `tick/date$
    (t/is-spec-check tick/date$))
  (t/with-instrument :all
    ;; date$ returns current date - just verify it's a valid date
    (let [now (tick/date$)]
      (t/is (int? now)))))

(t/deftest date->breakdown-test
  (t/with-instrument `tick/date->breakdown
    (t/is-spec-check tick/date->breakdown))
  (t/with-instrument :all
    (t/is= #::tick{:day-of-month 1
                   :month        1
                   :year         2070}
      (tick/date->breakdown 0 #{}))
    (t/is= #::tick{:day-of-month 8
                   :month        7
                   :ticks        31867145224192
                   :year         1814}
      (tick/date->breakdown m/min-long #{}))
    (t/is= #::tick{:day-of-month 28
                   :month        6
                   :ticks        66974454775807
                   :year         2325}
      (tick/date->breakdown m/max-long #{}))
    (t/is= #::tick{:day-of-month 2
                   :hours        10
                   :minutes      57
                   :month        3
                   :ms           767
                   :seconds      0
                   :ticks        641
                   :us           174
                   :year         2066}
      (tick/date->breakdown -138431984242352303))
    (t/is= #::tick{:day-of-month 2
                   :month        3
                   :ticks        45097357647697
                   :year         2066}
      (tick/date->breakdown -138431984242352303 #{}))
    (t/is= #::tick{:day-of-month 2
                   :month        3
                   :ms           767
                   :seconds      39420
                   :ticks        199697
                   :year         2066}
      (tick/date->breakdown -138431984242352303 #{::tick/seconds ::tick/ms}))
    (t/is= #::tick{:day-of-month 29
                   :month        1
                   :year         2041}
      (tick/date->breakdown -1044162662400000000 #{}))
    (t/is= #::tick{:day-of-month 29, :hours 0, :minutes 0, :month 2, :ms 0, :seconds 0, :ticks 0,
                   :us 0, :year 2024}
      (tick/date->breakdown -1654904908800000000))
    (t/is= #::tick{:day-of-month 1, :hours 0, :minutes 0, :month 3, :ms 0, :seconds 0, :ticks 0,
                   :us 0, :year 2024}
      (tick/date->breakdown -1654806067200000000))))

(t/deftest breakdown->date-test
  (t/with-instrument `tick/breakdown->date
    (t/is-spec-check tick/breakdown->date))
  (t/with-instrument :all
    (t/is= 0
      (tick/breakdown->date #::tick{:day-of-month 1, :month 1, :year 2070}))
    (t/is= m/min-long
      (tick/breakdown->date #::tick{:day-of-month 8, :month 7, :ticks 31867145224192, :year 1814}))
    (t/is= m/max-long
      (tick/breakdown->date #::tick{:day-of-month 28, :month 6, :ticks 66974454775807, :year 2325}))
    (t/is= -138431984242352303
      (tick/breakdown->date
        #::tick{:day-of-month 2, :hours 10, :minutes 57, :month 3, :ms 767, :seconds 0, :ticks 641,
                :us 174, :year 2066}))
    (t/is= -138431984242352303
      (tick/breakdown->date #::tick{:day-of-month 2, :month 3, :ticks 45097357647697, :year 2066}))
    (t/is= -138431984242352303
      (tick/breakdown->date
        #::tick{:day-of-month 2, :month 3, :ms 767, :seconds 39420, :ticks 199697, :year 2066}))
    (t/is= -1044162662400000000
      (tick/breakdown->date #::tick{:day-of-month 29, :month 1, :year 2041}))
    (t/is= -1654904908800000000
      (tick/breakdown->date
        #::tick{:day-of-month 29, :hours 0, :minutes 0, :month 2, :ms 0, :seconds 0, :ticks 0,
                :us 0, :year 2024}))
    (t/is= -1654806067200000000
      (tick/breakdown->date
        #::tick{:day-of-month 1, :hours 0, :minutes 0, :month 3, :ms 0, :seconds 0, :ticks 0, :us 0,
                :year 2024}))))

(t/deftest java-date->date-by-bounding-test
  (t/with-instrument `tick/java-date->date-by-bounding
    (t/is-spec-check tick/java-date->date-by-bounding))
  (t/with-instrument :all
    (t/is= m/min-long (tick/java-date->date-by-bounding #inst"0000-01-01T00:00:00.000-00:00"))
    (t/is= m/max-long (tick/java-date->date-by-bounding #inst"9999-12-31T23:59:59.999-00:00"))
    (t/is= tick/date-2070 (tick/java-date->date-by-bounding #inst"2070-01-01T00:00:00.000-00:00"))))

(t/deftest date-breakdown?-test
  (t/with-instrument `tick/date-breakdown?
    (t/is-spec-check tick/date-breakdown?))
  (t/with-instrument :all
    (t/is (tick/date-breakdown?
            #::tick{:day-of-month 1, :hours 0, :minutes 0, :month 3, :ms 0, :seconds 0, :ticks 0,
                    :us 0, :year 2024}))
    (t/is-not (tick/date-breakdown?
                #::tick{:day-of-month 30, :hours 0, :minutes 0, :month 2, :ms 0, :seconds 0,
                        :ticks 0, :us 0, :year 2024}))))

(t/deftest format-date-test
  (t/with-instrument `tick/format-date
    (t/is-spec-check tick/format-date))
  (t/with-instrument :all
    (t/is= "2031-08-28T13:30:07.671.748:258" (tick/format-date -1384319842423520030))
    (t/is= "2031-08-28T13:30:07.6717" (tick/format-date -1384319842423520030 4))
    (t/is= "2031-08-28T13:30:07.67174823" (tick/format-date -1384319842423520030 8))
    (t/is= "2031-08-28T13:30:07.671748225524476" (tick/format-date -1384319842423520030 15))))

(t/deftest parse-date-test
  (t/with-instrument `tick/parse-date
    (t/is-spec-check tick/parse-date))
  (t/with-instrument :all
    (t/is= -1384319842423520030 (tick/parse-date "2031-08-28T13:30:07.671.748:258"))
    ;;approx is off by 55170 ticks
    (t/is= -1384319842423575200 (tick/parse-date "2031-08-28T13:30:07.6717"))
    ;;approx is off by 5 ticks
    (t/is= -1384319842423520025 (tick/parse-date "2031-08-28T13:30:07.67174823"))
    (t/is= -1384319842423520030 (tick/parse-date "2031-08-28T13:30:07.671748225524476"))))

(t/deftest add-months-to-date-test
  (t/with-instrument `tick/add-months-to-date
    (t/is-spec-check tick/add-months-to-date))
  (t/with-instrument :all
    (t/is= #::tick{:day-of-month 1
                   :month        7
                   :year         2024}
      (tick/date->breakdown
        (tick/add-months-to-date tick/date-2020 54)
        #{}))
    (t/is= #::tick{:day-of-month 1
                   :month        10
                   :year         2018}
      (tick/date->breakdown
        (tick/add-months-to-date tick/date-2020 -15)
        #{}))))

(t/deftest day-of-week-test
  (t/with-instrument `tick/day-of-week
    (t/is-spec-check tick/day-of-week))
  (t/with-instrument :all
    (t/is= :wednesday (tick/day-of-week tick/date-2020))
    (t/is= :friday (tick/day-of-week (tick/add-months-to-date tick/date-2020 4)))))

(t/deftest start-of-year-test
  (t/with-instrument `tick/start-of-year
    (t/is-spec-check tick/start-of-year))
  (t/with-instrument :all
    (t/is= tick/date-2020 (tick/start-of-year tick/date-2020))
    (t/is= tick/date-2020 (tick/start-of-year (tick/add-months-to-date tick/date-2020 4)))))

(t/deftest end-of-year-test
  (t/with-instrument `tick/end-of-year
    (t/is-spec-check tick/end-of-year))
  (t/with-instrument :all
    (t/is= (tick/add-months-to-date tick/date-2020 12) (tick/end-of-year tick/date-2020))
    (t/is= (tick/add-months-to-date tick/date-2020 12)
      (tick/end-of-year (tick/add-months-to-date tick/date-2020 4)))))

(t/deftest start-of-month-test
  (t/with-instrument `tick/start-of-month
    (t/is-spec-check tick/start-of-month))
  (t/with-instrument :all
    (t/is= tick/date-2020 (tick/start-of-month tick/date-2020))
    (t/is= (tick/add-months-to-date tick/date-2020 4)
      (tick/start-of-month (+ (tick/add-months-to-date tick/date-2020 4) 2342478)))))

(t/deftest end-of-month-test
  (t/with-instrument `tick/end-of-month
    (t/is-spec-check tick/end-of-month))
  (t/with-instrument :all
    (t/is= (tick/add-months-to-date tick/date-2020 1) (tick/end-of-month tick/date-2020))
    (t/is= (tick/add-months-to-date tick/date-2020 5)
      (tick/end-of-month (+ (tick/add-months-to-date tick/date-2020 4) 2342478)))))

(t/deftest start-of-day-test
  (t/with-instrument `tick/start-of-day
    (t/is-spec-check tick/start-of-day))
  (t/with-instrument :all
    (t/is= tick/date-2020 (tick/start-of-day tick/date-2020))
    (t/is= tick/date-2020 (tick/start-of-day (+ tick/date-2020 2342478)))))

(t/deftest end-of-day-test
  (t/with-instrument `tick/end-of-day
    (t/is-spec-check tick/end-of-day))
  (t/with-instrument :all
    (t/is= (+ tick/date-2020 tick/ticks-per-day) (tick/end-of-day tick/date-2020))
    (t/is= (+ tick/date-2020 tick/ticks-per-day) (tick/end-of-day (+ tick/date-2020 2342478)))))

(t/deftest start-of-week-test
  (t/with-instrument `tick/start-of-week
    (t/is-spec-check tick/start-of-week))
  (t/with-instrument :all
    ;; 2020-01-01 is Wednesday, week starts on Sunday 2019-12-29
    (t/is= (tick/breakdown->date {::tick/day-of-month 29 ::tick/month 12 ::tick/year 2019})
      (tick/start-of-week tick/date-2020))
    ;; Sunday should return itself
    (let [sunday (tick/breakdown->date {::tick/day-of-month 5 ::tick/month 1 ::tick/year 2020})]
      (t/is= sunday (tick/start-of-week sunday)))
    ;; Saturday should return the previous Sunday
    (let [saturday (tick/breakdown->date {::tick/day-of-month 4 ::tick/month 1 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 29 ::tick/month 12 ::tick/year 2019})
        (tick/start-of-week saturday)))))

(t/deftest end-of-week-test
  (t/with-instrument `tick/end-of-week
    (t/is-spec-check tick/end-of-week))
  (t/with-instrument :all
    ;; 2020-01-01 is Wednesday, week ends on Sunday 2020-01-05
    (t/is= (tick/breakdown->date {::tick/day-of-month 5 ::tick/month 1 ::tick/year 2020})
      (tick/end-of-week tick/date-2020))
    ;; Sunday should return next Sunday
    (let [sunday (tick/breakdown->date {::tick/day-of-month 5 ::tick/month 1 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 12 ::tick/month 1 ::tick/year 2020})
        (tick/end-of-week sunday)))
    ;; Saturday should return next Sunday
    (let [saturday (tick/breakdown->date {::tick/day-of-month 4 ::tick/month 1 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 5 ::tick/month 1 ::tick/year 2020})
        (tick/end-of-week saturday)))))

(t/deftest start-of-quarter-test
  (t/with-instrument `tick/start-of-quarter
    (t/is-spec-check tick/start-of-quarter))
  (t/with-instrument :all
    ;; Q1: Jan 1
    (t/is= tick/date-2020 (tick/start-of-quarter tick/date-2020))
    ;; Q2: Apr 1
    (let [may-15 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 5 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 4 ::tick/year 2020})
        (tick/start-of-quarter may-15)))
    ;; Q3: Jul 1
    (let [aug-20 (tick/breakdown->date {::tick/day-of-month 20 ::tick/month 8 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 7 ::tick/year 2020})
        (tick/start-of-quarter aug-20)))
    ;; Q4: Oct 1
    (let [dec-31 (tick/breakdown->date {::tick/day-of-month 31 ::tick/month 12 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 10 ::tick/year 2020})
        (tick/start-of-quarter dec-31)))))

(t/deftest end-of-quarter-test
  (t/with-instrument `tick/end-of-quarter
    (t/is-spec-check tick/end-of-quarter))
  (t/with-instrument :all
    ;; Q1 ends Apr 1
    (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 4 ::tick/year 2020})
      (tick/end-of-quarter tick/date-2020))
    ;; Q2 ends Jul 1
    (let [may-15 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 5 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 7 ::tick/year 2020})
        (tick/end-of-quarter may-15)))
    ;; Q3 ends Oct 1
    (let [aug-20 (tick/breakdown->date {::tick/day-of-month 20 ::tick/month 8 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 10 ::tick/year 2020})
        (tick/end-of-quarter aug-20)))
    ;; Q4 ends Jan 1 of next year
    (let [dec-31 (tick/breakdown->date {::tick/day-of-month 31 ::tick/month 12 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 1 ::tick/year 2021})
        (tick/end-of-quarter dec-31)))))

(t/deftest ticks-in-month-test
  (t/with-instrument `tick/ticks-in-month
    (t/is-spec-check tick/ticks-in-month))
  (t/with-instrument :all
    (t/is= 3064089600000000 (tick/ticks-in-month tick/date-2020))
    (t/is= 2965248000000000
      (tick/ticks-in-month (+ tick/date-2020 (* 3 tick/ticks-per-average-month))))))

;;;DATE RANGE
(t/deftest months-difference-test
  (t/with-instrument `tick/months-difference
    (t/is-spec-check tick/months-difference))
  (t/with-instrument :all
    (t/is= 77 (tick/months-difference [73847 234242232323552353]))
    (t/is= 1 (tick/months-difference [-2473847 2342423]))
    (t/is= -1 (tick/months-difference [2342423 -2473847]))))

(t/deftest date-range->duration-test
  (t/with-instrument `tick/date-range->duration
    (t/is-spec-check tick/date-range->duration))
  (t/with-instrument :all
    (t/is= [77 2656363523478506] (tick/date-range->duration [73847 234242232323552353]))
    (t/is= [1 -3064089595183730] (tick/date-range->duration [-2473847 2342423]))))

(t/deftest date-range->months-floor-test
  (t/with-instrument `tick/date-range->months-floor
    (t/is-spec-check tick/date-range->months-floor))
  (t/with-instrument :all
    (t/is= [77 2656363523478506] (tick/date-range->months-floor [73847 234242232323552353]))
    (t/is= [0 4816270] (tick/date-range->months-floor [-2473847 2342423]))))

(t/deftest date-range->months-ceil-test
  (t/with-instrument `tick/date-range->months-ceil
    (t/is-spec-check tick/date-range->months-ceil))
  (t/with-instrument :all
    (t/is= [78 -308884476521494] (tick/date-range->months-ceil [73847 234242232323552353]))
    (t/is= [1 -3064089595183730] (tick/date-range->months-ceil [-2473847 2342423]))))

(t/deftest date-range->prorated-months-test
  (t/with-instrument `tick/date-range->prorated-months
    (t/is-spec-check tick/date-range->prorated-months))
  (t/with-instrument :all
    (t/is= 76.89583182367238 (tick/date-range->prorated-months [73847 234242232323552353]))
    (t/is= 1.5718437215413022E-9 (tick/date-range->prorated-months [-2473847 2342423]))))

(t/deftest format-duration-test
  (t/with-instrument `tick/format-duration
    (t/is-spec-check tick/format-duration))
  (t/with-instrument :all
    (t/is= "3mo20w01h18m17.923283s"
      (tick/format-duration {::tick/duration            [3 13843198424235230]
                             ::tick/show-average-years? false}))
    (t/is= "1y3mo20w01h18m17.9233s"
      (tick/format-duration {::tick/duration            [15 13843198424235230]
                             ::tick/fraction-precision  4
                             ::tick/show-average-years? false}))
    (t/is= "1y3mo0.3835ay"
      (tick/format-duration {::tick/duration            [15 13843198424235230]
                             ::tick/fraction-precision  4
                             ::tick/show-average-years? true}))
    (t/is= "-3mo20w01h18m17.92328254s"
      (tick/format-duration {::tick/duration            [-3 13843198424235230]
                             ::tick/fraction-precision  8
                             ::tick/show-average-years? false}))
    (t/is= "-1y-3mo20w01h18m17.923282543706293s"
      (tick/format-duration {::tick/duration            [-15 13843198424235230]
                             ::tick/fraction-precision  15
                             ::tick/show-average-years? false}))
    (t/is= "481w5d09h34m51.375291s"
      (tick/format-duration {::tick/duration            [0 333333333333333333]
                             ::tick/show-average-years? false}))
    (t/is= "9.233315ay" (tick/format-duration {::tick/duration [0 333333333333333333]}))))

(t/deftest parse-duration-test
  (t/with-instrument `tick/parse-duration
    (t/is-spec-check tick/parse-duration))
  (t/with-instrument :all
    ;; Test round-trip with format-duration
    (t/is= [3 13843198424235752] (tick/parse-duration "3mo20w01h18m17.923283s"))
    (t/is= [15 13843198424235752] (tick/parse-duration "1y3mo20w01h18m17.923283s"))
    (t/is= [-3 13843198424235752] (tick/parse-duration "-3mo20w01h18m17.923283s"))
    (t/is= [-15 13843198424235752] (tick/parse-duration "-1y-3mo20w01h18m17.923283s"))
    (t/is= [0 333333333333332903] (tick/parse-duration "481w5d09h34m51.375291s"))
    ;; Test average years format
    (t/is= [3 36101153088000000] (tick/parse-duration "3mo1.000000ay"))
    (t/is= [0 72202306176000000] (tick/parse-duration "2.000000ay"))
    ;; Test edge cases
    (t/is= [12 0] (tick/parse-duration "1y"))
    (t/is= [5 0] (tick/parse-duration "5mo"))
    (t/is= [0 0] (tick/parse-duration "0.000000ay"))
    ;; Test round-trip compatibility with simple values
    (let [original-duration [15 0]
          formatted (tick/format-duration {::tick/duration original-duration})
          parsed (tick/parse-duration formatted)]
      (t/is= original-duration parsed))
    ;; Test round-trip with average years (exact values)
    (let [original-duration [3 tick/ticks-per-average-year]
          formatted (tick/format-duration {::tick/duration            original-duration
                                           ::tick/fraction-precision  6
                                           ::tick/show-average-years? true})
          parsed (tick/parse-duration formatted)]
      (t/is= original-duration parsed))))

;;;AVERAGE YEARS
(t/deftest ticks->average-years-test
  (t/with-instrument `tick/ticks->average-years
    (t/is-spec-check tick/ticks->average-years))
  (t/with-instrument :all
    (t/is= 8.16660631615668E-6 (tick/ticks->average-years 294823904829))
    (t/is= -6.852542892382862E-11 (tick/ticks->average-years -2473847))))

(t/deftest average-years->ticks-test
  (t/with-instrument `tick/average-years->ticks
    (t/is-spec-check tick/average-years->ticks))
  (t/with-instrument :all
    (t/is= 0 (tick/average-years->ticks 0.0))
    (t/is= tick/ticks-per-average-year (tick/average-years->ticks 1.0))
    ;; round-trip test
    (t/is= 1.0 (tick/ticks->average-years (tick/average-years->ticks 1.0)))
    (t/is= 2.5 (tick/ticks->average-years (tick/average-years->ticks 2.5)))))

(t/deftest date-range->average-years-test
  (t/with-instrument `tick/date-range->average-years
    (t/is-spec-check tick/date-range->average-years))
  (t/with-instrument :all
    (t/is= 3.3368513762008396E-4 (tick/date-range->average-years [294823904829 12341242141242]))
    (t/is= 6.917427246472333E-11 (tick/date-range->average-years [-2473847 23424]))))

;;;PREDICATES
(t/deftest weekend?-test
  (t/with-instrument `tick/weekend?
    (t/is-spec-check tick/weekend?))
  (t/with-instrument :all
    (t/is-not (tick/weekend? tick/date-2020))
    (t/is (tick/weekend? (tick/breakdown->date {::tick/day-of-month 4
                                                ::tick/month        1
                                                ::tick/year         2020})))))

(t/deftest weekday?-test
  (t/with-instrument `tick/weekday?
    (t/is-spec-check tick/weekday?))
  (t/with-instrument :all
    (t/is (tick/weekday? tick/date-2020))
    (t/is-not (tick/weekday? (tick/breakdown->date {::tick/day-of-month 4
                                                    ::tick/month        1
                                                    ::tick/year         2020})))))

(t/deftest first-day-of-month?-test
  (t/with-instrument `tick/first-day-of-month?
    (t/is-spec-check tick/first-day-of-month?))
  (t/with-instrument :all
    (t/is (tick/first-day-of-month? tick/date-2020))
    (t/is-not (tick/first-day-of-month? (tick/breakdown->date {::tick/day-of-month 3
                                                               ::tick/month        12
                                                               ::tick/year         2020})))))

(t/deftest last-day-of-month?-test
  (t/with-instrument `tick/last-day-of-month?
    (t/is-spec-check tick/last-day-of-month?))
  (t/with-instrument :all
    (t/is-not (tick/last-day-of-month? tick/date-2020))
    (t/is (tick/last-day-of-month? (tick/breakdown->date {::tick/day-of-month 31
                                                          ::tick/month        12
                                                          ::tick/year         2020})))))

(t/deftest same-day?-test
  (t/with-instrument `tick/same-day?
    (t/is-spec-check tick/same-day?))
  (t/with-instrument :all
    (t/is-not (tick/same-day? [tick/date-2020 tick/date-2070]))
    (t/is (tick/same-day? [tick/date-2020 tick/date-2020]))
    (t/is-not (tick/same-day? [tick/date-2070 tick/date-2020]))))

;;;HOLIDAY CALENDAR & BUSINESS DAYS
(t/deftest business-day?-test
  (t/with-instrument `tick/business-day?
    (t/is-spec-check tick/business-day?))
  (t/with-instrument :all
    ;; Wednesday is a business day
    (t/is (tick/business-day? tick/date-2020))
    ;; Saturday is not
    (t/is-not (tick/business-day? (tick/breakdown->date {::tick/day-of-month 4
                                                         ::tick/month        1
                                                         ::tick/year         2020})))
    ;; Sunday is not
    (t/is-not (tick/business-day? (tick/breakdown->date {::tick/day-of-month 5
                                                         ::tick/month        1
                                                         ::tick/year         2020})))
    ;; Monday with holiday set
    (let [monday (tick/breakdown->date {::tick/day-of-month 6 ::tick/month 1 ::tick/year 2020})
          holidays #{monday}]
      (t/is (tick/business-day? monday))
      (t/is-not (tick/business-day? monday holidays)))))

(t/deftest add-business-days-test
  (t/with-instrument `tick/add-business-days
    (t/is-spec-check tick/add-business-days))
  (t/with-instrument :all
    ;; 2020-01-01 (Wed) + 5 business days = 2020-01-08 (Wed)
    (t/is= (tick/breakdown->date {::tick/day-of-month 8 ::tick/month 1 ::tick/year 2020})
      (tick/add-business-days tick/date-2020 5))
    ;; 2020-01-01 (Wed) - 3 business days = 2019-12-27 (Fri)
    (t/is= (tick/breakdown->date {::tick/day-of-month 27 ::tick/month 12 ::tick/year 2019})
      (tick/add-business-days tick/date-2020 -3))
    ;; 0 business days returns same date
    (t/is= tick/date-2020 (tick/add-business-days tick/date-2020 0))
    ;; With holidays
    (let [jan-2 (tick/breakdown->date {::tick/day-of-month 2 ::tick/month 1 ::tick/year 2020})
          holidays #{jan-2}]
      ;; 2020-01-01 (Wed) + 1 = 2020-01-03 (Fri), skipping Jan 2
      (t/is= (tick/breakdown->date {::tick/day-of-month 3 ::tick/month 1 ::tick/year 2020})
        (tick/add-business-days tick/date-2020 1 holidays)))))

(t/deftest business-days-between-test
  (t/with-instrument `tick/business-days-between
    (t/is-spec-check tick/business-days-between))
  (t/with-instrument :all
    ;; Wed Jan 1 to Wed Jan 8 = 5 business days
    (let [jan-8 (tick/breakdown->date {::tick/day-of-month 8 ::tick/month 1 ::tick/year 2020})]
      (t/is= 5 (tick/business-days-between [tick/date-2020 jan-8])))
    ;; Reverse direction
    (let [jan-8 (tick/breakdown->date {::tick/day-of-month 8 ::tick/month 1 ::tick/year 2020})]
      (t/is= -5 (tick/business-days-between [jan-8 tick/date-2020])))
    ;; Same day
    (t/is= 0 (tick/business-days-between [tick/date-2020 tick/date-2020]))
    ;; One week = 5 business days
    (let [jan-8 (tick/breakdown->date {::tick/day-of-month 8 ::tick/month 1 ::tick/year 2020})]
      (t/is= 5 (tick/business-days-between [tick/date-2020 jan-8])))
    ;; With holidays
    (let [jan-2 (tick/breakdown->date {::tick/day-of-month 2 ::tick/month 1 ::tick/year 2020})
          jan-8 (tick/breakdown->date {::tick/day-of-month 8 ::tick/month 1 ::tick/year 2020})
          holidays #{jan-2}]
      (t/is= 4 (tick/business-days-between [tick/date-2020 jan-8] holidays)))))

;;;FISCAL YEAR
(t/deftest fiscal-year-test
  (t/with-instrument `tick/fiscal-year
    (t/is-spec-check tick/fiscal-year))
  (t/with-instrument :all
    ;; Calendar year (default)
    (t/is= 2020 (tick/fiscal-year tick/date-2020))
    (t/is= 2020 (tick/fiscal-year tick/date-2020 1))
    ;; October fiscal year: Nov 2020 = FY 2021
    (let [nov-2020 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 11 ::tick/year 2020})]
      (t/is= 2021 (tick/fiscal-year nov-2020 10)))
    ;; October fiscal year: Sep 2020 = FY 2020
    (let [sep-2020 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 9 ::tick/year 2020})]
      (t/is= 2020 (tick/fiscal-year sep-2020 10)))
    ;; July fiscal year
    (let [aug-2020 (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 8 ::tick/year 2020})]
      (t/is= 2021 (tick/fiscal-year aug-2020 7)))))

(t/deftest start-of-fiscal-year-test
  (t/with-instrument `tick/start-of-fiscal-year
    (t/is-spec-check tick/start-of-fiscal-year))
  (t/with-instrument :all
    ;; Calendar year
    (t/is= tick/date-2020 (tick/start-of-fiscal-year tick/date-2020))
    ;; October fiscal year in Nov = Oct 1 of same calendar year
    (let [nov-2020 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 11 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 10 ::tick/year 2020})
        (tick/start-of-fiscal-year nov-2020 10)))
    ;; October fiscal year in Sep = Oct 1 of previous calendar year
    (let [sep-2020 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 9 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 10 ::tick/year 2019})
        (tick/start-of-fiscal-year sep-2020 10)))))

(t/deftest end-of-fiscal-year-test
  (t/with-instrument `tick/end-of-fiscal-year
    (t/is-spec-check tick/end-of-fiscal-year))
  (t/with-instrument :all
    ;; Calendar year
    (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 1 ::tick/year 2021})
      (tick/end-of-fiscal-year tick/date-2020))
    ;; October fiscal year in Nov = Oct 1 of next calendar year
    (let [nov-2020 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 11 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 10 ::tick/year 2021})
        (tick/end-of-fiscal-year nov-2020 10)))
    ;; October fiscal year in Sep = Oct 1 of same calendar year
    (let [sep-2020 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 9 ::tick/year 2020})]
      (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 10 ::tick/year 2020})
        (tick/end-of-fiscal-year sep-2020 10)))))

;;;DATE SEQUENCES
(t/deftest date-seq-test
  (t/with-instrument `tick/date-seq
    (t/is-spec-check tick/date-seq))
  (t/with-instrument :all
    ;; Daily sequence
    (let [dates (take 3 (tick/date-seq tick/date-2020))]
      (t/is= [tick/date-2020
              (+ tick/date-2020 tick/ticks-per-day)
              (+ tick/date-2020 (* 2 tick/ticks-per-day))]
        (vec dates)))
    ;; Weekly sequence
    (let [dates (take 3 (tick/date-seq tick/date-2020 {:step-unit :week}))]
      (t/is= [tick/date-2020
              (+ tick/date-2020 tick/ticks-per-week)
              (+ tick/date-2020 (* 2 tick/ticks-per-week))]
        (vec dates)))
    ;; Monthly sequence
    (let [dates (take 3 (tick/date-seq tick/date-2020 {:step-unit :month}))]
      (t/is= [tick/date-2020
              (tick/add-months-to-date tick/date-2020 1)
              (tick/add-months-to-date tick/date-2020 2)]
        (vec dates)))
    ;; Yearly sequence
    (let [dates (take 3 (tick/date-seq tick/date-2020 {:step-unit :year}))]
      (t/is= [tick/date-2020
              (tick/add-months-to-date tick/date-2020 12)
              (tick/add-months-to-date tick/date-2020 24)]
        (vec dates)))
    ;; With end-date
    (let [end (+ tick/date-2020 (* 5 tick/ticks-per-day))
          dates (tick/date-seq tick/date-2020 {:end-date end})]
      (t/is= 5 (count dates)))
    ;; Step amount
    (let [dates (take 3 (tick/date-seq tick/date-2020 {:step-amount 2, :step-unit :day}))]
      (t/is= [tick/date-2020
              (+ tick/date-2020 (* 2 tick/ticks-per-day))
              (+ tick/date-2020 (* 4 tick/ticks-per-day))]
        (vec dates)))))

;;;RANGE PREDICATES
(t/deftest date-in-range?-test
  (t/with-instrument `tick/date-in-range?
    (t/is-spec-check tick/date-in-range?))
  (t/with-instrument :all
    (let [start tick/date-2020
          end (+ tick/date-2020 (* 10 tick/ticks-per-day))
          mid (+ tick/date-2020 (* 5 tick/ticks-per-day))]
      ;; In range
      (t/is (tick/date-in-range? mid [start end]))
      ;; Start is inclusive
      (t/is (tick/date-in-range? start [start end]))
      ;; End is exclusive
      (t/is-not (tick/date-in-range? end [start end]))
      ;; Before range
      (t/is-not (tick/date-in-range? (- start tick/ticks-per-day) [start end]))
      ;; After range
      (t/is-not (tick/date-in-range? (+ end tick/ticks-per-day) [start end])))))

(t/deftest ranges-overlap?-test
  (t/with-instrument `tick/ranges-overlap?
    (t/is-spec-check tick/ranges-overlap?))
  (t/with-instrument :all
    (let [a1 tick/date-2020
          a2 (+ tick/date-2020 (* 10 tick/ticks-per-day))
          b1 (+ tick/date-2020 (* 5 tick/ticks-per-day))
          b2 (+ tick/date-2020 (* 15 tick/ticks-per-day))]
      ;; Overlapping ranges
      (t/is (tick/ranges-overlap? [a1 a2] [b1 b2]))
      ;; No overlap (disjoint)
      (t/is-not (tick/ranges-overlap? [a1 a2] [a2 b2]))
      ;; Same range
      (t/is (tick/ranges-overlap? [a1 a2] [a1 a2]))
      ;; One inside another
      (t/is (tick/ranges-overlap? [a1 b2] [b1 a2])))))

(t/deftest range-intersection-test
  (t/with-instrument `tick/range-intersection
    (t/is-spec-check tick/range-intersection))
  (t/with-instrument :all
    (let [a1 tick/date-2020
          a2 (+ tick/date-2020 (* 10 tick/ticks-per-day))
          b1 (+ tick/date-2020 (* 5 tick/ticks-per-day))
          b2 (+ tick/date-2020 (* 15 tick/ticks-per-day))]
      ;; Overlapping ranges
      (t/is= [b1 a2] (tick/range-intersection [a1 a2] [b1 b2]))
      ;; No overlap
      (t/is= nil (tick/range-intersection [a1 a2] [a2 b2]))
      ;; Same range
      (t/is= [a1 a2] (tick/range-intersection [a1 a2] [a1 a2])))))

(t/deftest range-contains?-test
  (t/with-instrument `tick/range-contains?
    (t/is-spec-check tick/range-contains?))
  (t/with-instrument :all
    (let [outer-start tick/date-2020
          outer-end (+ tick/date-2020 (* 20 tick/ticks-per-day))
          inner-start (+ tick/date-2020 (* 5 tick/ticks-per-day))
          inner-end (+ tick/date-2020 (* 15 tick/ticks-per-day))]
      ;; Outer contains inner
      (t/is (tick/range-contains? [outer-start outer-end] [inner-start inner-end]))
      ;; Inner does not contain outer
      (t/is-not (tick/range-contains? [inner-start inner-end] [outer-start outer-end]))
      ;; Same range contains itself
      (t/is (tick/range-contains? [outer-start outer-end] [outer-start outer-end])))))

;;;NAMED PERIODS
(t/deftest period->date-range-test
  (t/with-instrument `tick/period->date-range
    (t/is-spec-check tick/period->date-range))
  (t/with-instrument :all
    (let [ref (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 6 ::tick/year 2020})]
      ;; YTD
      (t/is= [(tick/breakdown->date {::tick/day-of-month 1 ::tick/month 1 ::tick/year 2020}) ref]
        (tick/period->date-range :ytd ref))
      ;; MTD
      (t/is= [(tick/breakdown->date {::tick/day-of-month 1 ::tick/month 6 ::tick/year 2020}) ref]
        (tick/period->date-range :mtd ref))
      ;; QTD (Q2 starts Apr 1)
      (t/is= [(tick/breakdown->date {::tick/day-of-month 1 ::tick/month 4 ::tick/year 2020}) ref]
        (tick/period->date-range :qtd ref))
      ;; Last 7 days
      (t/is= [(- ref (* 7 tick/ticks-per-day)) ref]
        (tick/period->date-range :last-7-days ref))
      ;; Last month
      (let [[start end] (tick/period->date-range :last-month ref)]
        (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 5 ::tick/year 2020}) start)
        (t/is= (tick/breakdown->date {::tick/day-of-month 1 ::tick/month 6 ::tick/year 2020}) end))
      ;; Trailing 12 months
      (t/is= [(tick/add-months-to-date ref -12) ref]
        (tick/period->date-range :trailing-12-months ref)))))

(t/deftest same-period-previous-year-test
  (t/with-instrument `tick/same-period-previous-year
    (t/is-spec-check tick/same-period-previous-year))
  (t/with-instrument :all
    (let [ref-2020 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 6 ::tick/year 2020})
          ref-2019 (tick/breakdown->date {::tick/day-of-month 15 ::tick/month 6 ::tick/year 2019})]
      ;; YTD in 2019
      (t/is= [(tick/breakdown->date {::tick/day-of-month 1 ::tick/month 1 ::tick/year 2019})
              ref-2019]
        (tick/same-period-previous-year :ytd ref-2020)))))

;;;ISO WEEK DATES & ORDINAL DATES
(t/deftest ordinal-day-test
  (t/with-instrument `tick/ordinal-day
    (t/is-spec-check tick/ordinal-day))
  (t/with-instrument :all
    ;; Jan 1 = day 1
    (t/is= 1 (tick/ordinal-day tick/date-2020))
    ;; Feb 14 = day 45
    (t/is= 45 (tick/ordinal-day (tick/breakdown->date {::tick/day-of-month 14
                                                       ::tick/month        2
                                                       ::tick/year         2020})))
    ;; Dec 31 in leap year = 366
    (t/is= 366 (tick/ordinal-day (tick/breakdown->date {::tick/day-of-month 31
                                                        ::tick/month        12
                                                        ::tick/year         2020})))
    ;; Dec 31 in non-leap year = 365
    (t/is= 365 (tick/ordinal-day (tick/breakdown->date {::tick/day-of-month 31
                                                        ::tick/month        12
                                                        ::tick/year         2019})))))

(t/deftest iso-week-year-test
  (t/with-instrument `tick/iso-week-year
    (t/is-spec-check tick/iso-week-year))
  (t/with-instrument :all
    ;; 2020-01-01 is in ISO week year 2020
    (t/is= 2020 (tick/iso-week-year tick/date-2020))
    ;; 2019-12-31 is in ISO week year 2020 (week 1 of 2020)
    (t/is= 2020 (tick/iso-week-year (tick/breakdown->date {::tick/day-of-month 31
                                                           ::tick/month        12
                                                           ::tick/year         2019})))
    ;; 2021-01-01 is in ISO week year 2020 (still week 53 of 2020)
    (t/is= 2020 (tick/iso-week-year (tick/breakdown->date {::tick/day-of-month 1
                                                           ::tick/month        1
                                                           ::tick/year         2021})))))

(t/deftest iso-week-number-test
  (t/with-instrument `tick/iso-week-number
    (t/is-spec-check tick/iso-week-number))
  (t/with-instrument :all
    ;; 2020-01-01 is week 1
    (t/is= 1 (tick/iso-week-number tick/date-2020))
    ;; 2019-12-31 is week 1 of 2020
    (t/is= 1 (tick/iso-week-number (tick/breakdown->date {::tick/day-of-month 31
                                                          ::tick/month        12
                                                          ::tick/year         2019})))
    ;; 2020-12-31 is week 53
    (t/is= 53 (tick/iso-week-number (tick/breakdown->date {::tick/day-of-month 31
                                                           ::tick/month        12
                                                           ::tick/year         2020})))))

(t/deftest format-iso-week-date-test
  (t/with-instrument `tick/format-iso-week-date
    (t/is-spec-check tick/format-iso-week-date))
  (t/with-instrument :all
    ;; 2020-01-01 is Wednesday of week 1
    (t/is= "2020-W01-3" (tick/format-iso-week-date tick/date-2020))
    ;; Sunday formatting (ISO day 7)
    (t/is= "2020-W01-7"
      (tick/format-iso-week-date (tick/breakdown->date {::tick/day-of-month 5
                                                        ::tick/month        1
                                                        ::tick/year         2020})))))

(t/deftest format-ordinal-date-test
  (t/with-instrument `tick/format-ordinal-date
    (t/is-spec-check tick/format-ordinal-date))
  (t/with-instrument :all
    (t/is= "2020-001" (tick/format-ordinal-date tick/date-2020))
    (t/is= "2020-045"
      (tick/format-ordinal-date (tick/breakdown->date {::tick/day-of-month 14
                                                       ::tick/month        2
                                                       ::tick/year         2020})))))

;;;RELATIVE FORMATTING
(t/deftest format-relative-test
  (t/with-instrument `tick/format-relative
    (t/is-spec-check tick/format-relative))
  (t/with-instrument :all
    (let [ref tick/date-2020]
      ;; Just now
      (t/is= "just now" (tick/format-relative ref ref))
      ;; Minutes
      (t/is= "5 minutes ago" (tick/format-relative (- ref (* 5 tick/ticks-per-minute)) ref))
      (t/is= "1 minute ago" (tick/format-relative (- ref tick/ticks-per-minute) ref))
      ;; Hours
      (t/is= "3 hours ago" (tick/format-relative (- ref (* 3 tick/ticks-per-hour)) ref))
      (t/is= "1 hour ago" (tick/format-relative (- ref tick/ticks-per-hour) ref))
      ;; Yesterday/Tomorrow
      (t/is= "yesterday" (tick/format-relative (- ref tick/ticks-per-day) ref))
      (t/is= "tomorrow" (tick/format-relative (+ ref tick/ticks-per-day) ref))
      ;; Days
      (t/is= "3 days ago" (tick/format-relative (- ref (* 3 tick/ticks-per-day)) ref))
      (t/is= "in 3 days" (tick/format-relative (+ ref (* 3 tick/ticks-per-day)) ref))
      ;; Weeks
      (t/is= "2 weeks ago" (tick/format-relative (- ref (* 14 tick/ticks-per-day)) ref))
      (t/is= "in 2 weeks" (tick/format-relative (+ ref (* 14 tick/ticks-per-day)) ref))
      ;; Months
      (t/is= "2 months ago" (tick/format-relative (tick/add-months-to-date ref -2) ref))
      (t/is= "in 2 months" (tick/format-relative (tick/add-months-to-date ref 2) ref))
      ;; Years
      (t/is= "2 years ago" (tick/format-relative (tick/add-months-to-date ref -24) ref))
      (t/is= "in 2 years" (tick/format-relative (tick/add-months-to-date ref 24) ref)))))
