(ns provisdom.date.instant-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.date.instant :as instant]))

;1 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;LEAP YEARS
(deftest leap-year?-test
  (is (spec-check instant/leap-year?))
  (is (instant/leap-year? 2000))
  (is-not (instant/leap-year? 2001))
  (is (instant/leap-year? 2004)))

(deftest days-in-month-test
  (is (spec-check instant/days-in-month))
  (is= 31 (instant/days-in-month [2014 3]))
  (is= 28 (instant/days-in-month [2014 2]))
  (is= 31 (instant/days-in-month [2014 1]))
  (is= 29 (instant/days-in-month [2004 2])))

(deftest days-until-month-test
  (is (spec-check instant/days-until-month))
  (is= 59 (instant/days-until-month [2014 3]))
  (is= 31 (instant/days-until-month [2014 2]))
  (is (zero? (instant/days-until-month [2014 1])))
  (is= 31 (instant/days-until-month [2004 2])))

(deftest passed-leap-days-test
  (is (spec-check instant/passed-leap-days))
  (is= 2425 (instant/passed-leap-days [0 1] [9999 12]))
  (is= -2425 (instant/passed-leap-days [9999 12] [0 1]))
  (is= 124 (instant/passed-leap-days [1814 1] [2325 12]))
  (is= -124 (instant/passed-leap-days [2325 12] [1814 1]))
  (is= -13 (instant/passed-leap-days [2070 1] [2016 3]))
  (is= -14 (instant/passed-leap-days [2070 1] [2014 2]))
  (is= 2 (instant/passed-leap-days [2014 2] [2020 9]))
  (is (zero? (instant/passed-leap-days [2014 2] [2014 5])))
  (is= 1 (instant/passed-leap-days [2000 1] [2000 3]))
  (is= 1 (instant/passed-leap-days [2000 1] [2004 1]))
  (is= 2 (instant/passed-leap-days [2000 1] [2004 3]))
  (is= 2 (instant/passed-leap-days [2000 1] [2005 1]))
  (is= 25 (instant/passed-leap-days [2000 1] [2100 1]))
  (is= 25 (instant/passed-leap-days [2000 1] [2100 3]))
  (is (zero? (instant/passed-leap-days [2000 1] [1996 3])))
  (is= -1 (instant/passed-leap-days [2000 1] [1996 1]))
  (is= -24 (instant/passed-leap-days [2000 1] [1900 3]))
  (is= -24 (instant/passed-leap-days [2000 1] [1900 1])))

;;;INSTANT
#_(deftest instant$-test
  (is (spec-check instant/instant$))
  (is= #inst "2020-05-05T20:57:50.661-00:00"
         (instant/instant$)))

(deftest instant->instant-ms-test
  (is (spec-check instant/instant->instant-ms))
  (is= 3155760000000
       (instant/instant->instant-ms #inst"2070-01-01T00:00:00.000-00:00"))
  (is= 0
       (instant/instant->instant-ms #inst"1970-01-01T00:00:00.000-00:00"))
  (is= -62135769600000
       (instant/instant->instant-ms #inst"0001-01-01T00:00:00.000-00:00"))
  (is= 253402300799999
       (instant/instant->instant-ms #inst"9999-12-31T23:59:59.999-00:00")))

;;;INSTANT-MS
#_(deftest instant-ms$-test
  (is (spec-check instant/instant-ms$))
  (is= 1588705104037 (instant/instant-ms$)))

(deftest instant-ms->instant-test
  (is (spec-check instant/instant-ms->instant))
  (is= #inst"2070-01-01T00:00:00.000-00:00"
       (instant/instant-ms->instant 3155760000000))
  (is= #inst"1970-01-01T00:00:00.000-00:00"
       (instant/instant-ms->instant 0))
  (is= #inst"0001-01-01T00:00:00.000-00:00"
       (instant/instant-ms->instant -62135769600000))
  (is= #inst "9999-12-31T23:59:59.999-00:00"
       (instant/instant-ms->instant 253402300799999)))

;;;PERIODS
(deftest instant-ms->period-test
  (is (spec-check instant/instant-ms->period))
  (is= 9.342597625683242
       (instant/instant-ms->period 294823904829))
  (is= -7.839309068885994E-5
       (instant/instant-ms->period -2473847)))

(deftest instant-interval->period-test
  (is (spec-check instant/instant-interval->period))
  (is= 3.901584981971643E-4
       (instant/instant-interval->period
         [(instant/instant-ms->instant 29029)
          (instant/instant-ms->instant 12341242)]))
  (is= 3.919982829773927E-4
       (instant/instant-interval->period
         [(instant/instant-ms->instant -29029)
          (instant/instant-ms->instant 12341242)])))