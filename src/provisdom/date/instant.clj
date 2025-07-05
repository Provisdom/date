(ns provisdom.date.instant
  "Java Date utilities with bounded precision and leap year calculations.

  Provides instant and in-ms types constrained to 0-10K years (0001-9999 CE)
  to avoid errors and inconsistencies. Includes leap year logic, period
  calculations, and common time unit conversions."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals])
  (:import (java.util Date)))


(declare inst->in-ms in-ms->inst)

(def ^:const unix-epoch
  "Unix epoch year (1970)."
  1970)
(def ^:const average-weeks-per-year
  "Average weeks per year in the Gregorian calendar (52.1775)."
  52.1775)
(def ^:const average-days-per-year
  "Average days per year accounting for leap years (365.2425)."
  365.2425)
(def ^:const average-weeks-per-month
  "Average weeks per month (4.348125)."
  4.348125)
(def ^:const average-days-per-month
  "Average days per month (30.436875)."
  30.436875)
(def ^:const ms-per-average-year
  "Milliseconds per average year (31,556,952,000)."
  31556952000)
(def ^:const ms-per-average-month
  "Milliseconds per average month (2,629,746,000)."
  2629746000)
(def ^:const ms-per-week
  "Milliseconds per week (604,800,000)."
  604800000)
(def ^:const ms-per-day
  "Milliseconds per day (86,400,000)."
  86400000)
(def ^:const ms-per-hour
  "Milliseconds per hour (3,600,000)."
  3600000)
(def ^:const ms-per-minute
  "Milliseconds per minute (60,000)."
  60000)
(def ^:const ms-per-second
  "Milliseconds per second (1,000)."
  1000)
(def ^:const minutes-per-day
  "Minutes per day (1,440)."
  1440)
(def ^:const min-in-ms
  "Minimum instant in milliseconds: 0001-01-01 (-62,135,769,600,000)."
  -62135769600000)
(def ^:const max-in-ms
  "Maximum instant in milliseconds: 9999-12-31 23:59:59.999 (253,402,300,799,999)."
  253402300799999)
(def ^:const min-inst
  "Minimum supported instant (0001-01-01T00:00:00.000-00:00)."
  #inst"0001-01-01T00:00:00.000-00:00")
(def ^:const max-inst
  "Maximum supported instant (9999-12-31T23:59:59.999-00:00)."
  #inst"9999-12-31T23:59:59.999-00:00")

(def ^:const ^:private non-leap-year-days-per-month
  [31 28 31 30 31 30 31 31 30 31 30 31])

(def ^:const ^:private non-leap-year-days-until-month
  [0 31 59 90 120 151 181 212 243 273 304 334])

(def ^:const day-of-week->name
  "Maps day-of-week keywords to full names."
  {:monday    "Monday"
   :tuesday   "Tuesday"
   :wednesday "Wednesday"
   :thursday  "Thursday"
   :friday    "Friday"
   :saturday  "Saturday"
   :sunday    "Sunday"})

(def ^:const day-of-week->abbreviation
  "Maps day-of-week keywords to abbreviations."
  {:monday    "Mon"
   :tuesday   "Tue"
   :wednesday "Wed"
   :thursday  "Thu"
   :friday    "Fri"
   :saturday  "Sat"
   :sunday    "Sun"})

(def ^:const months->name
  "Month names indexed by month number (0-based)."
  ["January", "February", "March", "April", "May", "June", "July", "August",
   "September", "October", "November", "December"])

(def ^:const months->abbreviation
  "Month abbreviations indexed by month number (0-based)."
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
(s/def ::yearly-periods ::m/number)                                 ;;average-years

(defn first-inst-not-after-second?
  [[inst1 inst2]]
  (not (.after ^Date inst1 ^Date inst2)))

;;for compatibility with ticks, use ::tick/instant-interval
(s/def ::inst-interval
  (s/and (s/tuple ::inst ::inst)
    first-inst-not-after-second?))

;;;LEAP YEARS
(defn leap-year?
  "Returns true if `year` is a leap year.
  
  Uses Gregorian calendar rules: divisible by 4, except years
  divisible by 100 unless also divisible by 400."
  [year]
  (or (zero? (mod year 400))
    (and (zero? (mod year 4))
      (not (zero? (mod year 100))))))

(s/fdef leap-year?
  :args (s/cat :year ::year)
  :ret boolean?)

(defn days-in-month
  "Returns the number of days in `month` for given `year`.
  
  Accounts for leap years when month is February.
  
  Example:
    (days-in-month [2020 2]) ; => 29 (leap year)
    (days-in-month [2021 2]) ; => 28"
  [[year month]]
  (if (and (== month 2) (leap-year? year))
    29
    (nth non-leap-year-days-per-month (dec month))))

(s/fdef days-in-month
  :args (s/cat :year-and-month ::year-and-month)
  :ret ::days-per-month)

(defn days-until-month
  "Returns the number of days from `year` start until `month` begins.
  
  Accounts for leap day if month is March or later in a leap year.
  
  Example:
    (days-until-month [2020 3]) ; => 60 (leap year)
    (days-until-month [2021 3]) ; => 59"
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
  "Returns the number of leap days between two year/month pairs.
  
  Calculates the difference in accumulated leap days from the first
  year/month to the second.
  
  Example:
    (passed-leap-days [2000 1] [2004 1]) ; => 1"
  [[year1 month1] [year2 month2]]
  (- (passed-leap-days-since-2000 year2 month2)
    (passed-leap-days-since-2000 year1 month1)))

(s/fdef passed-leap-days
  :args (s/cat :year-and-month1 ::year-and-month
          :year-and-month2 ::year-and-month)
  :ret (s/int-in -2425 2426))

;;;INST
(defn inst$
  "Returns the current instant.
  
  Precision is limited to milliseconds."
  []
  (Date.))

(s/fdef inst$
  :args (s/cat)
  :ret ::inst)

(defn inst->in-ms
  "Converts `inst` to milliseconds since Unix epoch."
  [inst]
  (inst-ms inst))

(s/fdef inst->in-ms
  :args (s/cat :inst ::inst)
  :ret ::in-ms)

(defn java-date->inst-by-bounding
  "Bounds a Java Date to the supported instant range.
  
  Clamps `java-date` outside the range 0001-9999 CE to the boundaries.
  
  Returns:
    Date within the supported range"
  [java-date]
  (cond (.before ^Date java-date min-inst) min-inst
    (.after ^Date java-date max-inst) max-inst
    :else java-date))

(s/fdef java-date->inst-by-bounding
  :args (s/cat :java-date ::java-date)
  :ret ::inst)

;;;IN-MS
(defn in-ms$
  "Returns the current time in milliseconds since Unix epoch.
  
  Precision is limited to milliseconds."
  []
  (.getTime ^Date (inst$)))

(s/fdef in-ms$
  :args (s/cat)
  :ret ::in-ms)

(defn in-ms->inst
  "Converts `in-ms` since Unix epoch to an instant."
  [in-ms]
  (Date. ^long in-ms))

(s/fdef in-ms->inst
  :args (s/cat :in-ms ::in-ms)
  :ret ::inst)

(defn ms->in-ms-by-bounding
  "Bounds `ms` to the supported range.
  
  Clamps values outside the supported instant range."
  [ms]
  (intervals/bound-by-interval [min-in-ms max-in-ms] ms))

(s/fdef ms->in-ms-by-bounding
  :args (s/cat :ms ::m/long)
  :ret ::in-ms)

;;;YEARLY PERIODS
(defn in-ms->yearly-periods
  "Converts `in-ms` to period in average years.
  
  Uses average year length of 365.2425 days."
  [in-ms]
  (/ in-ms (double ms-per-average-year)))

(s/fdef in-ms->yearly-periods
  :args (s/cat :in-ms ::in-ms)
  :ret ::yearly-periods)

(defn inst-interval->yearly-periods
  "Converts an `instant-interval` to period in average years.
  
  Takes a tuple of [start-instant end-instant] and returns
  the duration as a fractional number of average years."
  [instant-interval]
  (/ (- (inst->in-ms (second instant-interval))
       (double (inst->in-ms (first instant-interval))))
    ms-per-average-year))

(s/fdef inst-interval->yearly-periods
  :args (s/cat :inst-interval ::inst-interval)
  :ret (s/and ::yearly-periods m/non-?))

