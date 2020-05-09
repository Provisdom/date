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

;;;;This namespace has basic helpers for instant (java Date),
;;;; instant-ms (inst-ms java-date), leap years, periods (average years),
;;;; and common conversions.
;;;; TODO: add formatting and other helpers with java-time as needed

(declare instant->instant-ms instant-ms->instant)

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

;;incompatible with ticks, use spec from ticks ns if using ticks/date
(s/def ::instant-ms (s/int-in -62135769600000 253402300800000))

(defn- instant-in-range?
  [instant]
  (intervals/in-interval? [-62135769600000 253402300799999]
                          (instant->instant-ms instant)))

;;incompatible with ticks, use spec from ticks ns if using ticks/date
(s/def ::instant
  (s/with-gen (s/and inst? instant-in-range?)
              #(gen/fmap instant-ms->instant (s/gen ::instant-ms))))

(s/def ::duration-ms ::m/long)
(s/def ::period ::m/number)                                 ;;average-years

(defn first-instant-not-after-second?
  [[instant1 instant2]]
  (not (.after ^Date instant1 ^Date instant2)))

;;incompatible with ticks, use spec from ticks ns if using ticks/date
(s/def ::instant-interval
  (s/and (s/tuple ::instant ::instant)
         first-instant-not-after-second?))

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

;;;INSTANT
(defn instant$
  "Returns an instant literal of now. Loses precision below millisecond."
  []
  (Date.))

(s/fdef instant$
  :args (s/cat)
  :ret ::instant)

(defn instant->instant-ms
  "Converts `instant` to ::instant-ms."
  [instant]
  (inst-ms instant))

(s/fdef instant->instant-ms
  :args (s/cat :instant ::instant)
  :ret ::instant-ms)

;;;INSTANT-MS
(defn instant-ms$
  "Returns an ::instant-ms of now. Loses precision below millisecond."
  []
  (.getTime ^Date (instant$)))

(s/fdef instant-ms$
  :args (s/cat)
  :ret ::instant-ms)

(defn instant-ms->instant
  "Converts `instant-ms` to ::instant."
  [instant-ms]
  (Date. ^long instant-ms))

(s/fdef instant-ms->instant
  :args (s/cat :instant-ms ::instant-ms)
  :ret ::instant)

;;;PERIODS
(defn instant-ms->period
  "Returns period (in average-years) from `instant-ms`."
  [instant-ms]
  (/ instant-ms (double ms-per-average-year)))

(s/fdef instant-ms->period
  :args (s/cat :instant-ms ::instant-ms)
  :ret ::period)

(defn instant-interval->period
  "Returns period from `instant-interval`."
  [instant-interval]
  (/ (- (instant->instant-ms (second instant-interval))
        (double (instant->instant-ms (first instant-interval))))
     ms-per-average-year))

(s/fdef instant-interval->period
  :args (s/cat :instant-interval ::instant-interval)
  :ret (s/and ::period m/non-?))

