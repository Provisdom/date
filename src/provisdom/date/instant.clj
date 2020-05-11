(ns provisdom.date.instant
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [java-time.format :as jt-format]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals])
  (:import (java.util Date)))

;;;;This namespace has basic helpers for inst (java Date constrained to 0 to
;;;; <10K years -- this avoids errors and inconsistencies),
;;;; in-ms (inst-ms java-date), leap years, periods (average years),
;;;; and common conversions.
;;;;
;;;; TODO: add formatting and other helpers with java-time as needed

(declare inst->in-ms in-ms->inst)

(def ^:const unix-epoch "1970" 1970)
(def ^:const average-weeks-per-year "52.1775" 52.1775)
(def ^:const average-days-per-year "365.2425" 365.2425)
(def ^:const average-weeks-per-month "4.348125" 4.348125)
(def ^:const average-days-per-month "30.436875" 30.436875)
(def ^:const ms-per-average-year "31556952000" 31556952000)
(def ^:const ms-per-average-month "2629746000" 2629746000)
(def ^:const ms-per-week "604800000" 604800000)
(def ^:const ms-per-day "86400000" 86400000)
(def ^:const ms-per-hour "3600000" 3600000)
(def ^:const ms-per-minute "60000" 60000)
(def ^:const ms-per-second "1000" 1000)
(def ^:const minutes-per-day 1440)
(def ^:const min-in-ms -62135769600000)
(def ^:const max-in-ms 253402300799999)
(def ^:const min-inst #inst"0001-01-01T00:00:00.000-00:00")
(def ^:const max-inst #inst"9999-12-31T23:59:59.999-00:00")

(def ^:const ^:private non-leap-year-days-per-month
  [31 28 31 30 31 30 31 31 30 31 30 31])

(def ^:const ^:private non-leap-year-days-until-month
  [0 31 59 90 120 151 181 212 243 273 304 334])

(def ^:const day-of-week->name
  {:monday    "Monday"
   :tuesday   "Tuesday"
   :wednesday "Wednesday"
   :thursday  "Thursday"
   :friday    "Friday"
   :saturday  "Saturday"
   :sunday    "Sunday"})

(def ^:const day-of-week->abbreviation
  {:monday    "Mon"
   :tuesday   "Tue"
   :wednesday "Wed"
   :thursday  "Thu"
   :friday    "Fri"
   :saturday  "Sat"
   :sunday    "Sun"})

(def ^:const months->name
  ["January", "February", "March", "April", "May", "June", "July", "August",
   "September", "October", "November", "December"])

(def ^:const months->abbreviation
  ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
   "Nov", "Dec"])

(s/def ::year (s/int-in 0 10000))
(s/def ::month (s/int-in 1 13))
(s/def ::year-and-month (s/tuple ::year ::month))
(s/def ::days-per-month (s/int-in 28 32))

;;for compatibility with ticks, use ::tick/instant-ms
;;::in-ms keeps the inst in the range 0 to <10K years
(s/def ::in-ms (s/int-in min-in-ms (inc max-in-ms)))

(defn- inst-in-range?
  [inst]
  (intervals/in-interval? [min-in-ms max-in-ms] (inst->in-ms inst)))

(s/def ::java-date inst?)

;;for compatibility with ticks, use ::tick/instant
(s/def ::inst
  (s/with-gen (s/and ::java-date inst-in-range?)
              #(gen/fmap in-ms->inst (s/gen ::in-ms))))

(s/def ::duration-ms ::m/long)
(s/def ::period ::m/number)                                 ;;average-years

(defn first-inst-not-after-second?
  [[inst1 inst2]]
  (not (.after ^Date inst1 ^Date inst2)))

;;for compatibility with ticks, use ::tick/instant-interval
(s/def ::inst-interval
  (s/and (s/tuple ::inst ::inst)
         first-inst-not-after-second?))

;;;LEAP YEARS
(defn leap-year?
  "Returns whether a supplied `year` is a leap year."
  [year]
  (or (zero? (mod year 400))
      (and (zero? (mod year 4))
           (not (zero? (mod year 100))))))

(s/fdef leap-year?
  :args (s/cat :year ::year)
  :ret boolean?)

(defn days-in-month
  "Returns the number of days in a particular `year` of a supplied `month`."
  [[year month]]
  (if (and (== month 2) (leap-year? year))
    29
    (nth non-leap-year-days-per-month (dec month))))

(s/fdef days-in-month
  :args (s/cat :year-and-month ::year-and-month)
  :ret ::days-per-month)

(defn days-until-month
  "Returns the number of days in a particular `year` until a supplied `month`
  starts."
  [[year month]]
  (let [leap (if (and (>= month 3) (leap-year? year)) 1 0)]
    (+ leap (non-leap-year-days-until-month (dec month)))))

(s/fdef days-until-month
  :args (s/cat :year-and-month ::year-and-month)
  :ret (s/int-in 0 336))

(defn- passed-leap-days-since-2000
  [year month]
  (let [y (- year 2000)
        [a1 y1] (m/quot-and-mod' y 400)
        [a2 y2] (m/quot-and-mod' y1 100)
        a3 (m/quot' y2 4)
        d (+ (* 97 a1) (* 24 a2) a3 1)
        extra (if (and (leap-year? year) (<= month 2)) -1 0)]
    (+ d extra)))

(defn passed-leap-days
  "Returns the number of passed leap days between two year/month pairs."
  [[year1 month1] [year2 month2]]
  (- (passed-leap-days-since-2000 year2 month2)
     (passed-leap-days-since-2000 year1 month1)))

(s/fdef passed-leap-days
  :args (s/cat :year-and-month1 ::year-and-month
               :year-and-month2 ::year-and-month)
  :ret (s/int-in -2425 2426))

;;;INST
(defn inst$
  "Returns an ::inst of now. Loses precision below millisecond."
  []
  (Date.))

(s/fdef inst$
  :args (s/cat)
  :ret ::inst)

(defn inst->in-ms
  "Converts `inst` to ::in-ms."
  [inst]
  (inst-ms inst))

(s/fdef inst->in-ms
  :args (s/cat :inst ::inst)
  :ret ::in-ms)

(defn bound-java-date->inst
  "Bound `java-date` to ::inst range (#inst\"0001-01-01T00:00:00.000-00:00\"
  to #inst\"9999-12-31T23:59:59.999-00:00\"."
  [java-date]
  (cond (.before ^Date java-date min-inst) min-inst
        (.after ^Date java-date max-inst) max-inst
        :else java-date))

(s/fdef bound-java-date->inst
  :args (s/cat :java-date ::java-date)
  :ret ::inst)

;;;IN-MS
(defn in-ms$
  "Returns an ::in-ms of now. Loses precision below millisecond."
  []
  (.getTime ^Date (inst$)))

(s/fdef in-ms$
  :args (s/cat)
  :ret ::in-ms)

(defn in-ms->inst
  "Converts `in-ms` to ::inst."
  [in-ms]
  (Date. ^long in-ms))

(s/fdef in-ms->inst
  :args (s/cat :in-ms ::in-ms)
  :ret ::inst)

(defn bound-ms->in-ms
  "Bound `ms` to ::in-ms range."
  [ms]
  (intervals/bound-by-interval [min-in-ms max-in-ms] ms))

(s/fdef bound-ms->in-ms
  :args (s/cat :ms ::m/long)
  :ret ::in-ms)

;;;PERIODS
(defn in-ms->period
  "Returns period (in average-years) from `in-ms`."
  [in-ms]
  (/ in-ms (double ms-per-average-year)))

(s/fdef in-ms->period
  :args (s/cat :in-ms ::in-ms)
  :ret ::period)

(defn inst-interval->period
  "Returns period from `inst-interval`."
  [instant-interval]
  (/ (- (inst->in-ms (second instant-interval))
        (double (inst->in-ms (first instant-interval))))
     ms-per-average-year))

(s/fdef inst-interval->period
  :args (s/cat :inst-interval ::inst-interval)
  :ret (s/and ::period m/non-?))

