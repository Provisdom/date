(ns provisdom.date.instant-test
  (:require
    [provisdom.date.instant :as instant]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t]))

;;2 seconds

(set! *warn-on-reflection* true)

;;;LEAP YEARS
(t/deftest leap-year?-test
  (t/with-instrument `instant/leap-year?
    (t/is-spec-check instant/leap-year?))
  (t/with-instrument :all
    (t/is (instant/leap-year? 2000))
    (t/is-not (instant/leap-year? 2001))
    (t/is (instant/leap-year? 2004))
    ;; century years not leap unless divisible by 400
    (t/is-not (instant/leap-year? 1900))
    (t/is-not (instant/leap-year? 2100))
    (t/is (instant/leap-year? 1600))))

(t/deftest days-in-month-test
  (t/with-instrument `instant/days-in-month
    (t/is-spec-check instant/days-in-month))
  (t/with-instrument :all
    (t/is= 31 (instant/days-in-month [2014 3]))
    (t/is= 28 (instant/days-in-month [2014 2]))
    (t/is= 31 (instant/days-in-month [2014 1]))
    (t/is= 29 (instant/days-in-month [2004 2]))))

(t/deftest days-until-month-test
  (t/with-instrument `instant/days-until-month
    (t/is-spec-check instant/days-until-month))
  (t/with-instrument :all
    (t/is= 59 (instant/days-until-month [2014 3]))
    (t/is= 31 (instant/days-until-month [2014 2]))
    (t/is (zero? (instant/days-until-month [2014 1])))
    (t/is= 31 (instant/days-until-month [2004 2]))))

(t/deftest passed-leap-days-test
  (t/with-instrument `instant/passed-leap-days
    (t/is-spec-check instant/passed-leap-days))
  (t/with-instrument :all
    (t/is= 2425 (instant/passed-leap-days [0 1] [9999 12]))
    (t/is= -2425 (instant/passed-leap-days [9999 12] [0 1]))
    (t/is= 124 (instant/passed-leap-days [1814 1] [2325 12]))
    (t/is= -124 (instant/passed-leap-days [2325 12] [1814 1]))
    (t/is= -13 (instant/passed-leap-days [2070 1] [2016 3]))
    (t/is= -14 (instant/passed-leap-days [2070 1] [2014 2]))
    (t/is= 2 (instant/passed-leap-days [2014 2] [2020 9]))
    (t/is (zero? (instant/passed-leap-days [2014 2] [2014 5])))
    (t/is= 1 (instant/passed-leap-days [2000 1] [2000 3]))
    (t/is= 1 (instant/passed-leap-days [2000 1] [2004 1]))
    (t/is= 2 (instant/passed-leap-days [2000 1] [2004 3]))
    (t/is= 2 (instant/passed-leap-days [2000 1] [2005 1]))
    (t/is= 25 (instant/passed-leap-days [2000 1] [2100 1]))
    (t/is= 25 (instant/passed-leap-days [2000 1] [2100 3]))
    (t/is (zero? (instant/passed-leap-days [2000 1] [1996 3])))
    (t/is= -1 (instant/passed-leap-days [2000 1] [1996 1]))
    (t/is= -24 (instant/passed-leap-days [2000 1] [1900 3]))
    (t/is= -24 (instant/passed-leap-days [2000 1] [1900 1]))))

;;;INST
;; inst$ and in-ms$ test current time - verify they return valid results
(t/deftest inst$-test
  (t/with-instrument `instant/inst$
    (t/is-spec-check instant/inst$ {:num-tests 3}))
  (t/with-instrument :all
    ;; just verify it returns a valid instant in range
    (let [now (instant/inst$)]
      (t/is (inst? now))
      (t/is (<= instant/min-in-ms (instant/inst->in-ms now) instant/max-in-ms)))))

(t/deftest inst->in-ms-test
  (t/with-instrument `instant/inst->in-ms
    (t/is-spec-check instant/inst->in-ms))
  (t/with-instrument :all
    (t/is= 3155760000000 (instant/inst->in-ms #inst"2070-01-01T00:00:00.000-00:00"))
    (t/is= 0 (instant/inst->in-ms #inst"1970-01-01T00:00:00.000-00:00"))
    (t/is= -62135769600000 (instant/inst->in-ms #inst"0001-01-01T00:00:00.000-00:00"))
    (t/is= 253402300799999 (instant/inst->in-ms #inst"9999-12-31T23:59:59.999-00:00"))))

(t/deftest java-date->inst-by-bounding-test
  (t/with-instrument `instant/java-date->inst-by-bounding
    (t/is-spec-check instant/java-date->inst-by-bounding))
  (t/with-instrument :all
    (t/is= #inst"0001-01-01T00:00:00.000-00:00"
      (instant/java-date->inst-by-bounding #inst"0000-01-01T00:00:00.000-00:00"))
    (t/is= #inst"9999-12-31T23:59:59.999-00:00"
      (instant/java-date->inst-by-bounding #inst"9999-12-31T23:59:59.999-00:00"))
    (t/is= #inst"2070-01-01T00:00:00.000-00:00"
      (instant/java-date->inst-by-bounding #inst"2070-01-01T00:00:00.000-00:00"))))

;;;IN-MS
(t/deftest in-ms$-test
  (t/with-instrument `instant/in-ms$
    (t/is-spec-check instant/in-ms$ {:num-tests 3}))
  (t/with-instrument :all
    ;; verify it returns a valid in-ms value in range
    (let [now-ms (instant/in-ms$)]
      (t/is (int? now-ms))
      (t/is (<= instant/min-in-ms now-ms instant/max-in-ms)))))

(t/deftest in-ms->inst-test
  (t/with-instrument `instant/in-ms->inst
    (t/is-spec-check instant/in-ms->inst))
  (t/with-instrument :all
    (t/is= #inst"2070-01-01T00:00:00.000-00:00" (instant/in-ms->inst 3155760000000))
    (t/is= #inst"1970-01-01T00:00:00.000-00:00" (instant/in-ms->inst 0))
    (t/is= #inst"0001-01-01T00:00:00.000-00:00" (instant/in-ms->inst -62135769600000))
    (t/is= #inst"9999-12-31T23:59:59.999-00:00" (instant/in-ms->inst 253402300799999))))

(t/deftest ms->in-ms-by-bounding-test
  (t/with-instrument `instant/ms->in-ms-by-bounding
    (t/is-spec-check instant/ms->in-ms-by-bounding))
  (t/with-instrument :all
    (t/is= -62135769600000 (instant/ms->in-ms-by-bounding m/min-long))
    (t/is= 253402300799999 (instant/ms->in-ms-by-bounding m/max-long))
    (t/is= 0 (instant/ms->in-ms-by-bounding 0))))

;;;AVERAGE YEARS
(t/deftest in-ms->average-years-test
  (t/with-instrument `instant/in-ms->average-years
    (t/is-spec-check instant/in-ms->average-years))
  (t/with-instrument :all
    (t/is= 9.342597625683242 (instant/in-ms->average-years 294823904829))
    (t/is= -7.839309068885994E-5 (instant/in-ms->average-years -2473847))))

(t/deftest average-years->in-ms-test
  (t/with-instrument `instant/average-years->in-ms
    (t/is-spec-check instant/average-years->in-ms {:num-tests 15}))
  (t/with-instrument :all
    (t/is= 31556952000 (instant/average-years->in-ms 1.0))
    (t/is= 0 (instant/average-years->in-ms 0.0))
    (t/is= -31556952000 (instant/average-years->in-ms -1.0))
    ;; round-trip test
    (t/is= 294823904829 (instant/average-years->in-ms 9.342597625683242))))

(t/deftest inst-interval->average-years-test
  (t/with-instrument `instant/inst-interval->average-years
    (t/is-spec-check instant/inst-interval->average-years))
  (t/with-instrument :all
    (t/is= 3.901584981971643E-4
      (instant/inst-interval->average-years
        [(instant/in-ms->inst 29029) (instant/in-ms->inst 12341242)]))
    (t/is= 3.919982829773927E-4
      (instant/inst-interval->average-years
        [(instant/in-ms->inst -29029) (instant/in-ms->inst 12341242)]))))

;;;DATE BREAKDOWN
(t/deftest inst->year-test
  (t/with-instrument `instant/inst->year
    (t/is-spec-check instant/inst->year))
  (t/with-instrument :all
    (t/is= 2024 (instant/inst->year #inst"2024-03-15T14:30:00.000-00:00"))
    (t/is= 1 (instant/inst->year #inst"0001-01-01T00:00:00.000-00:00"))
    (t/is= 9999 (instant/inst->year #inst"9999-12-31T23:59:59.999-00:00"))))

(t/deftest inst->month-test
  (t/with-instrument `instant/inst->month
    (t/is-spec-check instant/inst->month))
  (t/with-instrument :all
    (t/is= 3 (instant/inst->month #inst"2024-03-15T14:30:00.000-00:00"))
    (t/is= 1 (instant/inst->month #inst"2024-01-01T00:00:00.000-00:00"))
    (t/is= 12 (instant/inst->month #inst"2024-12-31T23:59:59.999-00:00"))))

(t/deftest inst->day-of-month-test
  (t/with-instrument `instant/inst->day-of-month
    (t/is-spec-check instant/inst->day-of-month))
  (t/with-instrument :all
    (t/is= 15 (instant/inst->day-of-month #inst"2024-03-15T14:30:00.000-00:00"))
    (t/is= 1 (instant/inst->day-of-month #inst"2024-01-01T00:00:00.000-00:00"))
    (t/is= 31 (instant/inst->day-of-month #inst"2024-12-31T23:59:59.999-00:00"))))

(t/deftest inst->hour-test
  (t/with-instrument `instant/inst->hour
    (t/is-spec-check instant/inst->hour))
  (t/with-instrument :all
    (t/is= 14 (instant/inst->hour #inst"2024-03-15T14:30:00.000-00:00"))
    (t/is= 0 (instant/inst->hour #inst"2024-01-01T00:00:00.000-00:00"))
    (t/is= 23 (instant/inst->hour #inst"2024-12-31T23:59:59.999-00:00"))))

(t/deftest inst->minute-test
  (t/with-instrument `instant/inst->minute
    (t/is-spec-check instant/inst->minute))
  (t/with-instrument :all
    (t/is= 30 (instant/inst->minute #inst"2024-03-15T14:30:00.000-00:00"))
    (t/is= 0 (instant/inst->minute #inst"2024-01-01T00:00:00.000-00:00"))
    (t/is= 59 (instant/inst->minute #inst"2024-12-31T23:59:59.999-00:00"))))

(t/deftest inst->second-test
  (t/with-instrument `instant/inst->second
    (t/is-spec-check instant/inst->second))
  (t/with-instrument :all
    (t/is= 45 (instant/inst->second #inst"2024-03-15T14:30:45.000-00:00"))
    (t/is= 0 (instant/inst->second #inst"2024-01-01T00:00:00.000-00:00"))
    (t/is= 59 (instant/inst->second #inst"2024-12-31T23:59:59.999-00:00"))))

(t/deftest inst->millisecond-test
  (t/with-instrument `instant/inst->millisecond
    (t/is-spec-check instant/inst->millisecond))
  (t/with-instrument :all
    (t/is= 123 (instant/inst->millisecond #inst"2024-03-15T14:30:45.123-00:00"))
    (t/is= 0 (instant/inst->millisecond #inst"2024-01-01T00:00:00.000-00:00"))
    (t/is= 999 (instant/inst->millisecond #inst"2024-12-31T23:59:59.999-00:00"))))

;;;DAY OF WEEK
(t/deftest inst->day-of-week-test
  (t/with-instrument `instant/inst->day-of-week
    (t/is-spec-check instant/inst->day-of-week))
  (t/with-instrument :all
    ;; 2024-03-15 is a Friday
    (t/is= :friday (instant/inst->day-of-week #inst"2024-03-15T14:30:00.000-00:00"))
    ;; 2024-03-16 is a Saturday
    (t/is= :saturday (instant/inst->day-of-week #inst"2024-03-16T00:00:00.000-00:00"))
    ;; 2024-03-17 is a Sunday
    (t/is= :sunday (instant/inst->day-of-week #inst"2024-03-17T00:00:00.000-00:00"))
    ;; 2024-03-18 is a Monday
    (t/is= :monday (instant/inst->day-of-week #inst"2024-03-18T00:00:00.000-00:00"))))

;;;PREDICATES
(t/deftest weekend?-test
  (t/with-instrument `instant/weekend?
    (t/is-spec-check instant/weekend?))
  (t/with-instrument :all
    (t/is (instant/weekend? #inst"2024-03-16T00:00:00.000-00:00"))   ;; Saturday
    (t/is (instant/weekend? #inst"2024-03-17T00:00:00.000-00:00"))   ;; Sunday
    (t/is-not (instant/weekend? #inst"2024-03-15T00:00:00.000-00:00")) ;; Friday
    (t/is-not (instant/weekend? #inst"2024-03-18T00:00:00.000-00:00")))) ;; Monday

(t/deftest weekday?-test
  (t/with-instrument `instant/weekday?
    (t/is-spec-check instant/weekday?))
  (t/with-instrument :all
    (t/is (instant/weekday? #inst"2024-03-15T00:00:00.000-00:00"))   ;; Friday
    (t/is (instant/weekday? #inst"2024-03-18T00:00:00.000-00:00"))   ;; Monday
    (t/is-not (instant/weekday? #inst"2024-03-16T00:00:00.000-00:00")) ;; Saturday
    (t/is-not (instant/weekday? #inst"2024-03-17T00:00:00.000-00:00")))) ;; Sunday

(t/deftest same-day?-test
  (t/with-instrument `instant/same-day?
    (t/is-spec-check instant/same-day?))
  (t/with-instrument :all
    (t/is (instant/same-day? #inst"2024-03-15T00:00:00.000-00:00"
            #inst"2024-03-15T23:59:59.999-00:00"))
    (t/is-not (instant/same-day? #inst"2024-03-15T23:59:59.999-00:00"
                #inst"2024-03-16T00:00:00.000-00:00"))))

(t/deftest same-month?-test
  (t/with-instrument `instant/same-month?
    (t/is-spec-check instant/same-month?))
  (t/with-instrument :all
    (t/is (instant/same-month? #inst"2024-03-01T00:00:00.000-00:00"
            #inst"2024-03-31T23:59:59.999-00:00"))
    (t/is-not (instant/same-month? #inst"2024-03-31T23:59:59.999-00:00"
                #inst"2024-04-01T00:00:00.000-00:00"))
    (t/is-not (instant/same-month? #inst"2023-03-15T00:00:00.000-00:00"
                #inst"2024-03-15T00:00:00.000-00:00"))))

(t/deftest same-year?-test
  (t/with-instrument `instant/same-year?
    (t/is-spec-check instant/same-year?))
  (t/with-instrument :all
    (t/is (instant/same-year? #inst"2024-01-01T00:00:00.000-00:00"
            #inst"2024-12-31T23:59:59.999-00:00"))
    (t/is-not (instant/same-year? #inst"2023-12-31T23:59:59.999-00:00"
                #inst"2024-01-01T00:00:00.000-00:00"))))

(t/deftest first-day-of-month?-test
  (t/with-instrument `instant/first-day-of-month?
    (t/is-spec-check instant/first-day-of-month?))
  (t/with-instrument :all
    (t/is (instant/first-day-of-month? #inst"2024-03-01T12:00:00.000-00:00"))
    (t/is-not (instant/first-day-of-month? #inst"2024-03-02T00:00:00.000-00:00"))))

(t/deftest last-day-of-month?-test
  (t/with-instrument `instant/last-day-of-month?
    (t/is-spec-check instant/last-day-of-month?))
  (t/with-instrument :all
    (t/is (instant/last-day-of-month? #inst"2024-03-31T12:00:00.000-00:00"))
    (t/is (instant/last-day-of-month? #inst"2024-02-29T00:00:00.000-00:00")) ;; leap year
    (t/is (instant/last-day-of-month? #inst"2023-02-28T00:00:00.000-00:00")) ;; non-leap
    (t/is-not (instant/last-day-of-month? #inst"2024-03-30T00:00:00.000-00:00"))))

;;;DURATION ARITHMETIC
(t/deftest add-ms-test
  (t/with-instrument `instant/add-ms
    (t/is-spec-check instant/add-ms {:num-tests 15}))
  (t/with-instrument :all
    (t/is= #inst"2024-03-15T00:00:01.000-00:00"
      (instant/add-ms #inst"2024-03-15T00:00:00.000-00:00" 1000))
    (t/is= #inst"2024-03-14T23:59:59.000-00:00"
      (instant/add-ms #inst"2024-03-15T00:00:00.000-00:00" -1000))
    ;; bounds check
    (t/is= instant/max-inst
      (instant/add-ms instant/max-inst 1000000))))

(t/deftest add-seconds-test
  (t/with-instrument `instant/add-seconds
    (t/is-spec-check instant/add-seconds {:num-tests 15}))
  (t/with-instrument :all
    (t/is= #inst"2024-03-15T00:01:00.000-00:00"
      (instant/add-seconds #inst"2024-03-15T00:00:00.000-00:00" 60))))

(t/deftest add-minutes-test
  (t/with-instrument `instant/add-minutes
    (t/is-spec-check instant/add-minutes {:num-tests 15}))
  (t/with-instrument :all
    (t/is= #inst"2024-03-15T01:00:00.000-00:00"
      (instant/add-minutes #inst"2024-03-15T00:00:00.000-00:00" 60))))

(t/deftest add-hours-test
  (t/with-instrument `instant/add-hours
    (t/is-spec-check instant/add-hours {:num-tests 15}))
  (t/with-instrument :all
    (t/is= #inst"2024-03-16T00:00:00.000-00:00"
      (instant/add-hours #inst"2024-03-15T00:00:00.000-00:00" 24))))

(t/deftest add-days-test
  (t/with-instrument `instant/add-days
    (t/is-spec-check instant/add-days {:num-tests 15}))
  (t/with-instrument :all
    (t/is= #inst"2024-03-22T00:00:00.000-00:00"
      (instant/add-days #inst"2024-03-15T00:00:00.000-00:00" 7))
    (t/is= #inst"2024-03-08T00:00:00.000-00:00"
      (instant/add-days #inst"2024-03-15T00:00:00.000-00:00" -7))))

(t/deftest add-weeks-test
  (t/with-instrument `instant/add-weeks
    (t/is-spec-check instant/add-weeks {:num-tests 15}))
  (t/with-instrument :all
    (t/is= #inst"2024-03-29T00:00:00.000-00:00"
      (instant/add-weeks #inst"2024-03-15T00:00:00.000-00:00" 2))))

;;;CALENDAR NAVIGATION
(t/deftest start-of-day-test
  (t/with-instrument `instant/start-of-day
    (t/is-spec-check instant/start-of-day))
  (t/with-instrument :all
    (t/is= #inst"2024-03-15T00:00:00.000-00:00"
      (instant/start-of-day #inst"2024-03-15T14:30:45.123-00:00"))))

(t/deftest end-of-day-test
  (t/with-instrument `instant/end-of-day
    (t/is-spec-check instant/end-of-day))
  (t/with-instrument :all
    (t/is= #inst"2024-03-15T23:59:59.999-00:00"
      (instant/end-of-day #inst"2024-03-15T14:30:45.123-00:00"))))

(t/deftest start-of-month-test
  (t/with-instrument `instant/start-of-month
    (t/is-spec-check instant/start-of-month))
  (t/with-instrument :all
    (t/is= #inst"2024-03-01T00:00:00.000-00:00"
      (instant/start-of-month #inst"2024-03-15T14:30:45.123-00:00"))))

(t/deftest end-of-month-test
  (t/with-instrument `instant/end-of-month
    (t/is-spec-check instant/end-of-month))
  (t/with-instrument :all
    (t/is= #inst"2024-03-31T23:59:59.999-00:00"
      (instant/end-of-month #inst"2024-03-15T14:30:45.123-00:00"))
    ;; February in leap year
    (t/is= #inst"2024-02-29T23:59:59.999-00:00"
      (instant/end-of-month #inst"2024-02-15T00:00:00.000-00:00"))
    ;; February in non-leap year
    (t/is= #inst"2023-02-28T23:59:59.999-00:00"
      (instant/end-of-month #inst"2023-02-15T00:00:00.000-00:00"))))

(t/deftest start-of-year-test
  (t/with-instrument `instant/start-of-year
    (t/is-spec-check instant/start-of-year))
  (t/with-instrument :all
    (t/is= #inst"2024-01-01T00:00:00.000-00:00"
      (instant/start-of-year #inst"2024-03-15T14:30:45.123-00:00"))))

(t/deftest end-of-year-test
  (t/with-instrument `instant/end-of-year
    (t/is-spec-check instant/end-of-year))
  (t/with-instrument :all
    (t/is= #inst"2024-12-31T23:59:59.999-00:00"
      (instant/end-of-year #inst"2024-03-15T14:30:45.123-00:00"))))

(t/deftest start-of-week-test
  (t/with-instrument `instant/start-of-week
    (t/is-spec-check instant/start-of-week))
  (t/with-instrument :all
    ;; 2024-03-15 (Friday) -> Sunday 2024-03-10
    (t/is= #inst"2024-03-10T00:00:00.000-00:00"
      (instant/start-of-week #inst"2024-03-15T14:30:45.123-00:00"))))

(t/deftest end-of-week-test
  (t/with-instrument `instant/end-of-week
    (t/is-spec-check instant/end-of-week))
  (t/with-instrument :all
    ;; 2024-03-15 (Friday) -> Saturday 2024-03-16
    (t/is= #inst"2024-03-16T23:59:59.999-00:00"
      (instant/end-of-week #inst"2024-03-15T14:30:45.123-00:00"))))

(t/deftest start-of-quarter-test
  (t/with-instrument `instant/start-of-quarter
    (t/is-spec-check instant/start-of-quarter))
  (t/with-instrument :all
    ;; Q1: Jan-Mar
    (t/is= #inst"2024-01-01T00:00:00.000-00:00"
      (instant/start-of-quarter #inst"2024-03-15T14:30:45.123-00:00"))
    ;; Q2: Apr-Jun
    (t/is= #inst"2024-04-01T00:00:00.000-00:00"
      (instant/start-of-quarter #inst"2024-05-15T00:00:00.000-00:00"))
    ;; Q3: Jul-Sep
    (t/is= #inst"2024-07-01T00:00:00.000-00:00"
      (instant/start-of-quarter #inst"2024-08-15T00:00:00.000-00:00"))
    ;; Q4: Oct-Dec
    (t/is= #inst"2024-10-01T00:00:00.000-00:00"
      (instant/start-of-quarter #inst"2024-11-15T00:00:00.000-00:00"))))

(t/deftest end-of-quarter-test
  (t/with-instrument `instant/end-of-quarter
    (t/is-spec-check instant/end-of-quarter))
  (t/with-instrument :all
    ;; Q1: Jan-Mar
    (t/is= #inst"2024-03-31T23:59:59.999-00:00"
      (instant/end-of-quarter #inst"2024-02-15T14:30:45.123-00:00"))
    ;; Q2: Apr-Jun
    (t/is= #inst"2024-06-30T23:59:59.999-00:00"
      (instant/end-of-quarter #inst"2024-05-15T00:00:00.000-00:00"))))

;;;INTERVAL OPERATIONS
(t/deftest interval-duration-ms-test
  (t/with-instrument `instant/interval-duration-ms
    (t/is-spec-check instant/interval-duration-ms))
  (t/with-instrument :all
    (t/is= 86400000  ;; 1 day in ms
      (instant/interval-duration-ms
        [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-16T00:00:00.000-00:00"]))
    (t/is= 0
      (instant/interval-duration-ms
        [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-15T00:00:00.000-00:00"]))))

(t/deftest interval-contains?-test
  (t/with-instrument `instant/interval-contains?
    (t/is-spec-check instant/interval-contains?))
  (t/with-instrument :all
    (let [interval [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-20T00:00:00.000-00:00"]]
      (t/is (instant/interval-contains? interval #inst"2024-03-17T12:00:00.000-00:00"))
      (t/is (instant/interval-contains? interval #inst"2024-03-15T00:00:00.000-00:00")) ;; start
      (t/is (instant/interval-contains? interval #inst"2024-03-20T00:00:00.000-00:00")) ;; end
      (t/is-not (instant/interval-contains? interval #inst"2024-03-14T23:59:59.999-00:00"))
      (t/is-not (instant/interval-contains? interval #inst"2024-03-20T00:00:00.001-00:00")))))

(t/deftest interval-overlaps?-test
  (t/with-instrument `instant/interval-overlaps?
    (t/is-spec-check instant/interval-overlaps?))
  (t/with-instrument :all
    (let [a [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-20T00:00:00.000-00:00"]
          b [#inst"2024-03-18T00:00:00.000-00:00" #inst"2024-03-25T00:00:00.000-00:00"]
          c [#inst"2024-03-21T00:00:00.000-00:00" #inst"2024-03-25T00:00:00.000-00:00"]]
      (t/is (instant/interval-overlaps? a b))
      (t/is-not (instant/interval-overlaps? a c))
      ;; touching at edge
      (let [d [#inst"2024-03-20T00:00:00.000-00:00" #inst"2024-03-25T00:00:00.000-00:00"]]
        (t/is (instant/interval-overlaps? a d))))))

(t/deftest interval-intersection-test
  (t/with-instrument `instant/interval-intersection
    (t/is-spec-check instant/interval-intersection {:num-tests 15}))
  (t/with-instrument :all
    (let [a [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-20T00:00:00.000-00:00"]
          b [#inst"2024-03-18T00:00:00.000-00:00" #inst"2024-03-25T00:00:00.000-00:00"]]
      (t/is= [#inst"2024-03-18T00:00:00.000-00:00" #inst"2024-03-20T00:00:00.000-00:00"]
        (instant/interval-intersection a b)))
    ;; non-overlapping returns nil
    (let [a [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-16T00:00:00.000-00:00"]
          b [#inst"2024-03-20T00:00:00.000-00:00" #inst"2024-03-25T00:00:00.000-00:00"]]
      (t/is (nil? (instant/interval-intersection a b))))))

(t/deftest interval-union-test
  (t/with-instrument `instant/interval-union
    (t/is-spec-check instant/interval-union {:num-tests 15}))
  (t/with-instrument :all
    (let [a [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-20T00:00:00.000-00:00"]
          b [#inst"2024-03-18T00:00:00.000-00:00" #inst"2024-03-25T00:00:00.000-00:00"]]
      (t/is= [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-25T00:00:00.000-00:00"]
        (instant/interval-union a b)))
    ;; adjacent (touching) intervals can union
    (let [a [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-16T00:00:00.000-00:00"]
          b [#inst"2024-03-16T00:00:00.001-00:00" #inst"2024-03-20T00:00:00.000-00:00"]]
      (t/is= [#inst"2024-03-15T00:00:00.000-00:00" #inst"2024-03-20T00:00:00.000-00:00"]
        (instant/interval-union a b)))))

;;;FORMATTING
(t/deftest format-inst-test
  (t/with-instrument `instant/format-inst
    (t/is-spec-check instant/format-inst))
  (t/with-instrument :all
    (t/is= "2024-03-15T14:30:45.123Z"
      (instant/format-inst #inst"2024-03-15T14:30:45.123-00:00"))
    (t/is= "0001-01-01T00:00:00.000Z"
      (instant/format-inst #inst"0001-01-01T00:00:00.000-00:00"))
    (t/is= "9999-12-31T23:59:59.999Z"
      (instant/format-inst #inst"9999-12-31T23:59:59.999-00:00"))))

(t/deftest format-date-test
  (t/with-instrument `instant/format-date
    (t/is-spec-check instant/format-date))
  (t/with-instrument :all
    (t/is= "2024-03-15"
      (instant/format-date #inst"2024-03-15T14:30:45.123-00:00"))
    (t/is= "0001-01-01"
      (instant/format-date #inst"0001-01-01T00:00:00.000-00:00"))))

;;;PARSING
(t/deftest parse-inst-test
  (t/with-instrument `instant/parse-inst
    (t/is-spec-check instant/parse-inst {:num-tests 10}))
  (t/with-instrument :all
    ;; full format
    (t/is= #inst"2024-03-15T14:30:45.123-00:00"
      (instant/parse-inst "2024-03-15T14:30:45.123Z"))
    ;; without Z
    (t/is= #inst"2024-03-15T14:30:45.123-00:00"
      (instant/parse-inst "2024-03-15T14:30:45.123"))
    ;; without milliseconds
    (t/is= #inst"2024-03-15T14:30:45.000-00:00"
      (instant/parse-inst "2024-03-15T14:30:45Z"))
    ;; date only
    (t/is= #inst"2024-03-15T00:00:00.000-00:00"
      (instant/parse-inst "2024-03-15"))
    ;; invalid returns nil
    (t/is (nil? (instant/parse-inst "not-a-date")))
    ;; out of range returns nil
    (t/is (nil? (instant/parse-inst "0000-01-01T00:00:00.000Z")))))

(t/deftest parse-date-test
  (t/with-instrument `instant/parse-date
    (t/is-spec-check instant/parse-date {:num-tests 10}))
  (t/with-instrument :all
    (t/is= #inst"2024-03-15T00:00:00.000-00:00"
      (instant/parse-date "2024-03-15"))
    (t/is (nil? (instant/parse-date "invalid")))))

;; round-trip tests
(t/deftest round-trip-test
  (t/with-instrument :all
    ;; format -> parse
    (let [inst #inst"2024-03-15T14:30:45.123-00:00"]
      (t/is= inst (instant/parse-inst (instant/format-inst inst))))
    ;; parse -> format
    (let [s "2024-03-15T14:30:45.123Z"]
      (t/is= s (instant/format-inst (instant/parse-inst s))))))
