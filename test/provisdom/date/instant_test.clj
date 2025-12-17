(ns provisdom.date.instant-test
  (:require
    ;[clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [orchestra.spec.test :as ost]
    [provisdom.date.instant :as instant]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t]))

;2 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;LEAP YEARS
(deftest leap-year?-test
  (t/is-spec-check instant/leap-year?))
  (is (instant/leap-year? 2000))
  (t/is-not (instant/leap-year? 2001))
  (is (instant/leap-year? 2004)))

(deftest days-in-month-test
  (t/is-spec-check instant/days-in-month))
  (t/is= 31 (instant/days-in-month [2014 3]))
  (t/is= 28 (instant/days-in-month [2014 2]))
  (t/is= 31 (instant/days-in-month [2014 1]))
  (t/is= 29 (instant/days-in-month [2004 2])))

(deftest days-until-month-test
  (t/is-spec-check instant/days-until-month))
  (t/is= 59 (instant/days-until-month [2014 3]))
  (t/is= 31 (instant/days-until-month [2014 2]))
  (is (zero? (instant/days-until-month [2014 1])))
  (t/is= 31 (instant/days-until-month [2004 2])))

(deftest passed-leap-days-test
  (t/is-spec-check instant/passed-leap-days))
  (t/is= 2425 (instant/passed-leap-days [0 1] [9999 12]))
  (t/is= -2425 (instant/passed-leap-days [9999 12] [0 1]))
  (t/is= 124 (instant/passed-leap-days [1814 1] [2325 12]))
  (t/is= -124 (instant/passed-leap-days [2325 12] [1814 1]))
  (t/is= -13 (instant/passed-leap-days [2070 1] [2016 3]))
  (t/is= -14 (instant/passed-leap-days [2070 1] [2014 2]))
  (t/is= 2 (instant/passed-leap-days [2014 2] [2020 9]))
  (is (zero? (instant/passed-leap-days [2014 2] [2014 5])))
  (t/is= 1 (instant/passed-leap-days [2000 1] [2000 3]))
  (t/is= 1 (instant/passed-leap-days [2000 1] [2004 1]))
  (t/is= 2 (instant/passed-leap-days [2000 1] [2004 3]))
  (t/is= 2 (instant/passed-leap-days [2000 1] [2005 1]))
  (t/is= 25 (instant/passed-leap-days [2000 1] [2100 1]))
  (t/is= 25 (instant/passed-leap-days [2000 1] [2100 3]))
  (is (zero? (instant/passed-leap-days [2000 1] [1996 3])))
  (t/is= -1 (instant/passed-leap-days [2000 1] [1996 1]))
  (t/is= -24 (instant/passed-leap-days [2000 1] [1900 3]))
  (t/is= -24 (instant/passed-leap-days [2000 1] [1900 1])))

;;;INST
#_(deftest inst$-test
    (t/is-spec-check instant/inst$))
    (t/is= #inst "2020-05-05T20:57:50.661-00:00"
         (instant/inst$)))

(deftest inst->in-ms-test
  (t/is-spec-check instant/inst->in-ms))
  (t/is= 3155760000000
       (instant/inst->in-ms #inst"2070-01-01T00:00:00.000-00:00"))
  (t/is= 0
       (instant/inst->in-ms #inst"1970-01-01T00:00:00.000-00:00"))
  (t/is= -62135769600000
       (instant/inst->in-ms #inst"0001-01-01T00:00:00.000-00:00"))
  (t/is= 253402300799999
       (instant/inst->in-ms #inst"9999-12-31T23:59:59.999-00:00")))

(deftest java-date->inst-by-bounding-test
  (t/is-spec-check instant/java-date->inst-by-bounding))
  (t/is= #inst"0000-01-01T00:00:00.000-00:00"
       (instant/java-date->inst-by-bounding #inst"0000-01-01T00:00:00.000-00:00"))
  (t/is= #inst"9999-12-31T23:59:59.999-00:00"
       (instant/java-date->inst-by-bounding #inst"9999-12-31T23:59:59.999-00:00"))
  (t/is= #inst"2070-01-01T00:00:00.000-00:00"
       (instant/java-date->inst-by-bounding #inst"2070-01-01T00:00:00.000-00:00")))

;;;IN-MS
#_(deftest in-ms$-test
  (t/is-spec-check instant/in-ms$))
  (t/is= 1588705104037 (instant/in-ms$)))

(deftest in-ms->inst-test
  (t/is-spec-check instant/in-ms->inst))
  (t/is= #inst"2070-01-01T00:00:00.000-00:00"
       (instant/in-ms->inst 3155760000000))
  (t/is= #inst"1970-01-01T00:00:00.000-00:00"
       (instant/in-ms->inst 0))
  (t/is= #inst"0001-01-01T00:00:00.000-00:00"
       (instant/in-ms->inst -62135769600000))
  (t/is= #inst"9999-12-31T23:59:59.999-00:00"
       (instant/in-ms->inst 253402300799999)))

(deftest ms->in-ms-by-bounding-test
  (t/is-spec-check instant/ms->in-ms-by-bounding))
  (t/is= -62135769600000 (instant/ms->in-ms-by-bounding m/min-long))
  (t/is= 253402300799999 (instant/ms->in-ms-by-bounding m/max-long))
  (t/is= 0 (instant/ms->in-ms-by-bounding 0)))

;;;AVERAGE YEARS
(deftest in-ms->average-years-test
  (t/is-spec-check instant/in-ms->average-years))
  (t/is= 9.342597625683242
       (instant/in-ms->average-years 294823904829))
  (t/is= -7.839309068885994E-5
       (instant/in-ms->average-years -2473847)))

(deftest inst-interval->average-years-test
  (t/is-spec-check instant/inst-interval->average-years))
  (t/is= 3.901584981971643E-4
       (instant/inst-interval->average-years
         [(instant/in-ms->inst 29029)
          (instant/in-ms->inst 12341242)]))
  (t/is= 3.919982829773927E-4
       (instant/inst-interval->average-years
         [(instant/in-ms->inst -29029)
          (instant/in-ms->inst 12341242)])))
