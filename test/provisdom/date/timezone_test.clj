(ns provisdom.date.timezone-test
  (:require
    [provisdom.date.tick :as tick]
    [provisdom.date.timezone :as tz]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;2 seconds

(set! *warn-on-reflection* true)

;;;ZONE UTILITIES
(t/deftest valid-zone-id?-test
  (t/with-instrument `tz/valid-zone-id?
    (t/is-spec-check tz/valid-zone-id?))
  (t/with-instrument :all
    (t/is (tz/valid-zone-id? "America/New_York"))
    (t/is (tz/valid-zone-id? "UTC"))
    (t/is (tz/valid-zone-id? "Europe/London"))
    (t/is-not (tz/valid-zone-id? "Invalid/Zone"))
    (t/is-not (tz/valid-zone-id? ""))
    (t/is-not (tz/valid-zone-id? nil))
    (t/is-not (tz/valid-zone-id? 123))))

(t/deftest system-zone-id$-test
  (t/with-instrument `tz/system-zone-id$
    (t/is-spec-check tz/system-zone-id$))
  (t/with-instrument :all
    (let [zone-id (tz/system-zone-id$)]
      (t/is (string? zone-id))
      (t/is (tz/valid-zone-id? zone-id)))))

(t/deftest available-zone-ids-test
  (t/with-instrument `tz/available-zone-ids
    (t/is-spec-check tz/available-zone-ids))
  (t/with-instrument :all
    (let [zones (tz/available-zone-ids)]
      (t/is (vector? zones))
      (t/is (> (count zones) 0))
      (t/is (some #{"America/New_York"} zones))
      (t/is (some #{"UTC"} zones))
      ;; Sorted
      (t/is= zones (vec (sort zones))))))

(t/deftest zone-offset-at-date-test
  (t/with-instrument `tz/zone-offset-at-date
    (t/is-spec-check tz/zone-offset-at-date))
  (t/with-instrument :all
    ;; UTC has zero offset
    (t/is= {::tz/offset-hours   0
            ::tz/offset-minutes 0
            ::tz/offset-seconds 0
            ::tz/offset-ticks   0}
      (tz/zone-offset-at-date "UTC" tick/date-2020))
    ;; New York in January (EST = UTC-5)
    (let [offset (tz/zone-offset-at-date "America/New_York" tick/date-2020)]
      (t/is= -5 (::tz/offset-hours offset))
      (t/is= 0 (::tz/offset-minutes offset)))
    ;; New York in July (EDT = UTC-4)
    (let [july-2020 (tick/breakdown->date {::tick/year         2020
                                           ::tick/month        7
                                           ::tick/day-of-month 15})
          offset (tz/zone-offset-at-date "America/New_York" july-2020)]
      (t/is= -4 (::tz/offset-hours offset))
      (t/is= 0 (::tz/offset-minutes offset)))
    ;; India has +5:30 offset
    (let [offset (tz/zone-offset-at-date "Asia/Kolkata" tick/date-2020)]
      (t/is= 5 (::tz/offset-hours offset))
      (t/is= 30 (::tz/offset-minutes offset)))))

(t/deftest format-offset-test
  (t/with-instrument `tz/format-offset
    (t/is-spec-check tz/format-offset))
  (t/with-instrument :all
    (t/is= "+00:00"
      (tz/format-offset {::tz/offset-hours 0 ::tz/offset-minutes 0 ::tz/offset-ticks 0}))
    (t/is= "-05:00"
      (tz/format-offset {::tz/offset-hours -5 ::tz/offset-minutes 0 ::tz/offset-ticks 0}))
    (t/is= "+05:30"
      (tz/format-offset {::tz/offset-hours 5 ::tz/offset-minutes 30 ::tz/offset-ticks 0}))
    (t/is= "-09:30"
      (tz/format-offset {::tz/offset-hours -9 ::tz/offset-minutes -30 ::tz/offset-ticks 0}))))

;;;DST DETECTION
(t/deftest dst?-test
  (t/with-instrument `tz/dst?
    (t/is-spec-check tz/dst?))
  (t/with-instrument :all
    ;; New York in January is NOT in DST
    (t/is-not (tz/dst? "America/New_York" tick/date-2020))
    ;; New York in July IS in DST
    (let [july-2020 (tick/breakdown->date {::tick/year         2020
                                           ::tick/month        7
                                           ::tick/day-of-month 15})]
      (t/is (tz/dst? "America/New_York" july-2020)))
    ;; UTC never has DST
    (t/is-not (tz/dst? "UTC" tick/date-2020))
    (let [july-2020 (tick/breakdown->date {::tick/year         2020
                                           ::tick/month        7
                                           ::tick/day-of-month 15})]
      (t/is-not (tz/dst? "UTC" july-2020)))))

(t/deftest ambiguous-local-time?-test
  (t/with-instrument `tz/ambiguous-local-time?
    (t/is-spec-check tz/ambiguous-local-time?))
  (t/with-instrument :all
    ;; Nov 3, 2024 at 1:30 AM is ambiguous in New York (fall-back)
    (t/is
      (tz/ambiguous-local-time? "America/New_York"
        {::tick/year 2024 ::tick/month 11 ::tick/day-of-month 3 ::tick/hours 1 ::tick/minutes 30}))
    ;; Nov 3, 2024 at 3:00 AM is NOT ambiguous
    (t/is-not
      (tz/ambiguous-local-time? "America/New_York"
        {::tick/year 2024 ::tick/month 11 ::tick/day-of-month 3 ::tick/hours 3 ::tick/minutes 0}))
    ;; Regular time is not ambiguous
    (t/is-not
      (tz/ambiguous-local-time? "America/New_York"
        {::tick/year 2024 ::tick/month 7 ::tick/day-of-month 4 ::tick/hours 12 ::tick/minutes 0}))))

(t/deftest invalid-local-time?-test
  (t/with-instrument `tz/invalid-local-time?
    (t/is-spec-check tz/invalid-local-time?))
  (t/with-instrument :all
    ;; Mar 10, 2024 at 2:30 AM is invalid in New York (spring-forward)
    (t/is
      (tz/invalid-local-time? "America/New_York"
        {::tick/year 2024 ::tick/month 3 ::tick/day-of-month 10 ::tick/hours 2 ::tick/minutes 30}))
    ;; Mar 10, 2024 at 3:00 AM is valid (after the gap)
    (t/is-not
      (tz/invalid-local-time? "America/New_York"
        {::tick/year 2024 ::tick/month 3 ::tick/day-of-month 10 ::tick/hours 3 ::tick/minutes 0}))
    ;; Mar 10, 2024 at 1:59 AM is valid (before the gap)
    (t/is-not
      (tz/invalid-local-time? "America/New_York"
        {::tick/year 2024 ::tick/month 3 ::tick/day-of-month 10 ::tick/hours 1 ::tick/minutes 59}))
    ;; Regular time is not invalid
    (t/is-not
      (tz/invalid-local-time? "America/New_York"
        {::tick/year 2024 ::tick/month 7 ::tick/day-of-month 4 ::tick/hours 12 ::tick/minutes 0}))))

(t/deftest next-dst-transition-test
  (t/with-instrument `tz/next-dst-transition
    (t/is-spec-check tz/next-dst-transition))
  (t/with-instrument :all
    ;; From Jan 2024, next transition is spring-forward in March
    (let [jan-2024 (tick/breakdown->date {::tick/year         2024
                                          ::tick/month        1
                                          ::tick/day-of-month 15})
          transition (tz/next-dst-transition "America/New_York" jan-2024)]
      (t/is (some? transition))
      (t/is= :gap (::tz/transition-type transition))
      ;; Offset changes from -5 to -4
      (t/is= -5 (::tz/offset-hours (::tz/offset-before transition)))
      (t/is= -4 (::tz/offset-hours (::tz/offset-after transition))))
    ;; From July 2024, next transition is fall-back in November
    (let [july-2024 (tick/breakdown->date {::tick/year         2024
                                           ::tick/month        7
                                           ::tick/day-of-month 15})
          transition (tz/next-dst-transition "America/New_York" july-2024)]
      (t/is (some? transition))
      (t/is= :overlap (::tz/transition-type transition))
      ;; Offset changes from -4 to -5
      (t/is= -4 (::tz/offset-hours (::tz/offset-before transition)))
      (t/is= -5 (::tz/offset-hours (::tz/offset-after transition))))
    ;; UTC has no DST transitions
    (t/is= nil (tz/next-dst-transition "UTC" tick/date-2020))))

;;;CORE CONVERSIONS
(t/deftest date->local-breakdown-test
  (t/with-instrument `tz/date->local-breakdown
    (t/is-spec-check tz/date->local-breakdown))
  (t/with-instrument :all
    ;; UTC midnight 2020-01-01 -> Local time in New York is 2019-12-31 19:00 (EST)
    (let [breakdown (tz/date->local-breakdown "America/New_York" tick/date-2020 #{})]
      (t/is= 2019 (::tick/year breakdown))
      (t/is= 12 (::tick/month breakdown))
      (t/is= 31 (::tick/day-of-month breakdown)))
    ;; With time components
    (let [breakdown (tz/date->local-breakdown "America/New_York" tick/date-2020)]
      (t/is= 2019 (::tick/year breakdown))
      (t/is= 12 (::tick/month breakdown))
      (t/is= 31 (::tick/day-of-month breakdown))
      (t/is= 19 (::tick/hours breakdown))
      (t/is= 0 (::tick/minutes breakdown)))
    ;; UTC stays the same
    (let [breakdown (tz/date->local-breakdown "UTC" tick/date-2020 #{})]
      (t/is= 2020 (::tick/year breakdown))
      (t/is= 1 (::tick/month breakdown))
      (t/is= 1 (::tick/day-of-month breakdown)))))

(t/deftest local-breakdown->date-test
  (t/with-instrument `tz/local-breakdown->date
    (t/is-spec-check tz/local-breakdown->date))
  (t/with-instrument :all
    ;; Simple conversion
    (let [breakdown {::tick/year         2020
                     ::tick/month        1
                     ::tick/day-of-month 1
                     ::tick/hours        0
                     ::tick/minutes      0
                     ::tick/seconds      0}
          date (tz/local-breakdown->date "UTC" breakdown)]
      (t/is= tick/date-2020 date))
    ;; New York local time to UTC
    (let [breakdown {::tick/year         2019
                     ::tick/month        12
                     ::tick/day-of-month 31
                     ::tick/hours        19
                     ::tick/minutes      0
                     ::tick/seconds      0}
          date (tz/local-breakdown->date "America/New_York" breakdown)]
      (t/is= tick/date-2020 date))
    ;; Ambiguous time - earlier (default)
    (let [breakdown {::tick/year         2024
                     ::tick/month        11
                     ::tick/day-of-month 3
                     ::tick/hours        1
                     ::tick/minutes      30
                     ::tick/seconds      0}
          date-earlier (tz/local-breakdown->date "America/New_York" breakdown)
          date-later (tz/local-breakdown->date "America/New_York"
                       breakdown {::tz/dst-resolution :later})]
      ;; Earlier should be before later (by 1 hour)
      (t/is (< date-earlier date-later))
      (t/is= tick/ticks-per-hour (- date-later date-earlier)))
    ;; Ambiguous time - error
    (let [breakdown {::tick/year         2024
                     ::tick/month        11
                     ::tick/day-of-month 3
                     ::tick/hours        1
                     ::tick/minutes      30
                     ::tick/seconds      0}
          result (tz/local-breakdown->date "America/New_York"
                   breakdown {::tz/dst-resolution :error})]
      (t/is (anomalies/anomaly? result)))
    ;; Invalid time - post-gap (default)
    (let [breakdown {::tick/year         2024
                     ::tick/month        3
                     ::tick/day-of-month 10
                     ::tick/hours        2
                     ::tick/minutes      30
                     ::tick/seconds      0}
          date (tz/local-breakdown->date "America/New_York" breakdown)
          ;; Should resolve to 3:00 AM EDT (the moment after the gap)
          result-breakdown (tz/date->local-breakdown "America/New_York" date)]
      (t/is= 3 (::tick/hours result-breakdown))
      (t/is= 0 (::tick/minutes result-breakdown)))
    ;; Invalid time - pre-gap
    (let [breakdown {::tick/year         2024
                     ::tick/month        3
                     ::tick/day-of-month 10
                     ::tick/hours        2
                     ::tick/minutes      30
                     ::tick/seconds      0}
          date (tz/local-breakdown->date "America/New_York"
                 breakdown {::tz/gap-resolution :pre-gap})
          ;; Should resolve to just before 2:00 AM EST
          result-breakdown (tz/date->local-breakdown "America/New_York" date)]
      (t/is= 1 (::tick/hours result-breakdown))
      (t/is= 59 (::tick/minutes result-breakdown)))
    ;; Invalid time - error
    (let [breakdown {::tick/year         2024
                     ::tick/month        3
                     ::tick/day-of-month 10
                     ::tick/hours        2
                     ::tick/minutes      30
                     ::tick/seconds      0}
          result (tz/local-breakdown->date "America/New_York"
                   breakdown {::tz/gap-resolution :error})]
      (t/is (anomalies/anomaly? result)))))

;;;STRING FORMATTING/PARSING
(t/deftest date->local-string-test
  (t/with-instrument `tz/date->local-string
    (t/is-spec-check tz/date->local-string))
  (t/with-instrument :all
    ;; UTC
    (t/is= "2020-01-01T00:00:00.000000" (tz/date->local-string "UTC" tick/date-2020))
    ;; New York (2019-12-31 19:00:00 EST)
    (t/is= "2019-12-31T19:00:00.000000" (tz/date->local-string "America/New_York" tick/date-2020))
    ;; With precision
    (t/is= "2020-01-01T00:00:00.0000" (tz/date->local-string "UTC" tick/date-2020 4))))

(t/deftest local-string->date-test
  (t/with-instrument `tz/local-string->date
    (t/is-spec-check tz/local-string->date))
  (t/with-instrument :all
    ;; Round-trip UTC
    (let [date tick/date-2020
          str (tz/date->local-string "UTC" date)
          parsed (tz/local-string->date "UTC" str)]
      (t/is= date parsed))
    ;; Round-trip New York
    (let [date tick/date-2020
          str (tz/date->local-string "America/New_York" date)
          parsed (tz/local-string->date "America/New_York" str)]
      (t/is= date parsed))
    ;; Parse with date only
    (let [parsed (tz/local-string->date "UTC" "2020-01-01")]
      (t/is= tick/date-2020 parsed))
    ;; Invalid string returns anomaly
    (let [result (tz/local-string->date "UTC" "invalid")]
      (t/is (anomalies/anomaly? result)))))

(t/deftest format-with-zone-test
  (t/with-instrument `tz/format-with-zone
    (t/is-spec-check tz/format-with-zone))
  (t/with-instrument :all
    (t/is= "2020-01-01T00:00:00.000000 UTC" (tz/format-with-zone "UTC" tick/date-2020))
    (t/is= "2019-12-31T19:00:00.000000 America/New_York"
      (tz/format-with-zone "America/New_York" tick/date-2020))))

;;;ZONE-AWARE CALENDAR OPS
(t/deftest start-of-day-in-zone-test
  (t/with-instrument `tz/start-of-day-in-zone
    (t/is-spec-check tz/start-of-day-in-zone))
  (t/with-instrument :all
    ;; UTC
    (t/is= tick/date-2020 (tz/start-of-day-in-zone "UTC" tick/date-2020))
    ;; New York - start of day is 5 hours after UTC midnight
    (let [noon-utc (+ tick/date-2020 (* 12 tick/ticks-per-hour))
          start (tz/start-of-day-in-zone "UTC" noon-utc)]
      (t/is= tick/date-2020 start))
    ;; New York at noon UTC (7 AM EST) -> start of day is 5 AM UTC
    (let [noon-utc (+ tick/date-2020 (* 12 tick/ticks-per-hour))
          start (tz/start-of-day-in-zone "America/New_York" noon-utc)]
      ;; Local midnight in New York on Jan 1 2020 is 5 AM UTC
      (t/is= (+ tick/date-2020 (* 5 tick/ticks-per-hour)) start))))

(t/deftest end-of-day-in-zone-test
  (t/with-instrument `tz/end-of-day-in-zone
    (t/is-spec-check tz/end-of-day-in-zone))
  (t/with-instrument :all
    ;; UTC
    (t/is= (+ tick/date-2020 tick/ticks-per-day) (tz/end-of-day-in-zone "UTC" tick/date-2020))
    ;; New York at noon UTC -> end of day is 5 AM UTC next day
    (let [noon-utc (+ tick/date-2020 (* 12 tick/ticks-per-hour))
          end (tz/end-of-day-in-zone "America/New_York" noon-utc)]
      ;; Local midnight on Jan 2 2020 in New York is 5 AM UTC Jan 2
      (t/is= (+ tick/date-2020 tick/ticks-per-day (* 5 tick/ticks-per-hour)) end))))

(t/deftest local-day-of-week-test
  (t/with-instrument `tz/local-day-of-week
    (t/is-spec-check tz/local-day-of-week))
  (t/with-instrument :all
    ;; 2020-01-01 is Wednesday in UTC
    (t/is= :wednesday (tz/local-day-of-week "UTC" tick/date-2020))
    ;; 2020-01-01 00:00 UTC is 2019-12-31 19:00 in New York, which is Tuesday
    (t/is= :tuesday (tz/local-day-of-week "America/New_York" tick/date-2020))
    ;; At noon UTC on 2020-01-01, it's still Jan 1 in New York (Wednesday)
    (let [noon-utc (+ tick/date-2020 (* 12 tick/ticks-per-hour))]
      (t/is= :wednesday (tz/local-day-of-week "America/New_York" noon-utc)))))
