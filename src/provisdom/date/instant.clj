(ns provisdom.date.instant
  "Java Date utilities with bounded precision and leap year calculations.

  Provides instant and in-ms types constrained to 0-10K years (0001-9999 CE)
  to avoid errors and inconsistencies. Includes:
  - Leap year logic and calendar calculations
  - Date component extraction (year, month, day, hour, etc.)
  - Day-of-week computation and predicates (weekend?, weekday?)
  - Duration arithmetic (add-ms, add-days, add-weeks, etc.)
  - Calendar navigation (start-of-day, start-of-month, etc.)
  - Interval operations (contains?, overlaps?, intersection, union)
  - ISO-8601 parsing and formatting"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals])
  (:import (java.util Calendar Date TimeZone)))


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
  "Month names indexed by month number (0-based).
  Use `(nth months->name (dec month))` for 1-based month numbers."
  ["January", "February", "March", "April", "May", "June", "July", "August",
   "September", "October", "November", "December"])

(def ^:const months->abbreviation
  "Month abbreviations indexed by month number (0-based).
  Use `(nth months->abbreviation (dec month))` for 1-based month numbers."
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
(s/def ::average-years ::m/number)

(s/def ::day-of-month (s/int-in 1 32))
(s/def ::hour (s/int-in 0 24))
(s/def ::minute (s/int-in 0 60))
(s/def ::second (s/int-in 0 60))
(s/def ::millisecond (s/int-in 0 1000))
(s/def ::day-of-week #{:monday :tuesday :wednesday :thursday :friday :saturday :sunday})

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

;;;AVERAGE YEARS
(defn in-ms->average-years
  "Converts `in-ms` to average years.
  
  Uses average year length of 365.2425 days."
  [in-ms]
  (/ in-ms (double ms-per-average-year)))

(s/fdef in-ms->average-years
  :args (s/cat :in-ms ::in-ms)
  :ret ::average-years)

(defn average-years->in-ms
  "Converts `average-years` to milliseconds.

  Uses average year length of 365.2425 days.
  Returns a long (truncated)."
  [average-years]
  (long (* average-years ms-per-average-year)))

(s/fdef average-years->in-ms
  :args (s/cat :average-years ::average-years)
  :ret ::m/long)

(defn inst-interval->average-years
  "Converts an `instant-interval` to average years. Takes a tuple of `[start-instant end-instant]` and returns the
  duration as a fractional number of average years."
  [instant-interval]
  (/ (- (inst->in-ms (second instant-interval))
       (double (inst->in-ms (first instant-interval))))
    ms-per-average-year))

(s/fdef inst-interval->average-years
  :args (s/cat :inst-interval ::inst-interval)
  :ret (s/and ::average-years m/non-?))

;;;DATE BREAKDOWN
(defn- inst->calendar
  "Returns a Calendar for the given instant in UTC."
  ^Calendar [inst]
  (doto (Calendar/getInstance (TimeZone/getTimeZone "UTC"))
    (.setTime ^Date inst)))

(defn inst->year
  "Returns the year (0-9999) of `inst`."
  [inst]
  (.get (inst->calendar inst) Calendar/YEAR))

(s/fdef inst->year
  :args (s/cat :inst ::inst)
  :ret ::year)

(defn inst->month
  "Returns the month (1-12) of `inst`."
  [inst]
  (inc (.get (inst->calendar inst) Calendar/MONTH)))

(s/fdef inst->month
  :args (s/cat :inst ::inst)
  :ret ::month)

(defn inst->day-of-month
  "Returns the day of month (1-31) of `inst`."
  [inst]
  (.get (inst->calendar inst) Calendar/DAY_OF_MONTH))

(s/fdef inst->day-of-month
  :args (s/cat :inst ::inst)
  :ret ::day-of-month)

(defn inst->hour
  "Returns the hour (0-23) of `inst` in UTC."
  [inst]
  (.get (inst->calendar inst) Calendar/HOUR_OF_DAY))

(s/fdef inst->hour
  :args (s/cat :inst ::inst)
  :ret ::hour)

(defn inst->minute
  "Returns the minute (0-59) of `inst`."
  [inst]
  (.get (inst->calendar inst) Calendar/MINUTE))

(s/fdef inst->minute
  :args (s/cat :inst ::inst)
  :ret ::minute)

(defn inst->second
  "Returns the second (0-59) of `inst`."
  [inst]
  (.get (inst->calendar inst) Calendar/SECOND))

(s/fdef inst->second
  :args (s/cat :inst ::inst)
  :ret ::second)

(defn inst->millisecond
  "Returns the millisecond (0-999) of `inst`."
  [inst]
  (.get (inst->calendar inst) Calendar/MILLISECOND))

(s/fdef inst->millisecond
  :args (s/cat :inst ::inst)
  :ret ::millisecond)

;;;DAY OF WEEK
(def ^:private calendar-day->day-of-week
  {Calendar/SUNDAY    :sunday
   Calendar/MONDAY    :monday
   Calendar/TUESDAY   :tuesday
   Calendar/WEDNESDAY :wednesday
   Calendar/THURSDAY  :thursday
   Calendar/FRIDAY    :friday
   Calendar/SATURDAY  :saturday})

(defn inst->day-of-week
  "Returns the day of week keyword for `inst`.

  Returns one of: `:monday`, `:tuesday`, `:wednesday`, `:thursday`, `:friday`, `:saturday`, `:sunday`."
  [inst]
  (calendar-day->day-of-week (.get (inst->calendar inst) Calendar/DAY_OF_WEEK)))

(s/fdef inst->day-of-week
  :args (s/cat :inst ::inst)
  :ret ::day-of-week)

;;;PREDICATES
(defn weekend?
  "Returns true if `inst` falls on a Saturday or Sunday."
  [inst]
  (boolean (#{:saturday :sunday} (inst->day-of-week inst))))

(s/fdef weekend?
  :args (s/cat :inst ::inst)
  :ret boolean?)

(defn weekday?
  "Returns true if `inst` falls on a weekday (Monday-Friday)."
  [inst]
  (not (weekend? inst)))

(s/fdef weekday?
  :args (s/cat :inst ::inst)
  :ret boolean?)

(defn same-day?
  "Returns true if two instants fall on the same calendar day (UTC)."
  [inst1 inst2]
  (let [c1 (inst->calendar inst1)
        c2 (inst->calendar inst2)]
    (and (== (.get c1 Calendar/YEAR)
           (.get c2 Calendar/YEAR))
      (== (.get c1 Calendar/DAY_OF_YEAR)
        (.get c2 Calendar/DAY_OF_YEAR)))))

(s/fdef same-day?
  :args (s/cat :inst1 ::inst :inst2 ::inst)
  :ret boolean?)

(defn same-month?
  "Returns true if two instants fall in the same month and year (UTC)."
  [inst1 inst2]
  (and (== (inst->year inst1) (inst->year inst2))
    (== (inst->month inst1) (inst->month inst2))))

(s/fdef same-month?
  :args (s/cat :inst1 ::inst :inst2 ::inst)
  :ret boolean?)

(defn same-year?
  "Returns true if two instants fall in the same year (UTC)."
  [inst1 inst2]
  (== (inst->year inst1) (inst->year inst2)))

(s/fdef same-year?
  :args (s/cat :inst1 ::inst :inst2 ::inst)
  :ret boolean?)

(defn first-day-of-month?
  "Returns true if `inst` is the first day of the month."
  [inst]
  (== 1 (inst->day-of-month inst)))

(s/fdef first-day-of-month?
  :args (s/cat :inst ::inst)
  :ret boolean?)

(defn last-day-of-month?
  "Returns true if `inst` is the last day of the month."
  [inst]
  (== (inst->day-of-month inst)
    (days-in-month [(inst->year inst) (inst->month inst)])))

(s/fdef last-day-of-month?
  :args (s/cat :inst ::inst)
  :ret boolean?)

;;;DURATION ARITHMETIC
(defn add-ms
  "Adds `duration-ms` to `inst`, returning a new instant. Duration can be negative to subtract time. Result is bounded
  to valid instant range."
  [inst duration-ms]
  (in-ms->inst (ms->in-ms-by-bounding (+ (inst->in-ms inst) duration-ms))))

(s/fdef add-ms
  :args (s/cat :inst ::inst :duration-ms ::duration-ms)
  :ret ::inst)

(defn add-seconds
  "Adds `seconds` to `inst`, returning a new instant."
  [inst seconds]
  (add-ms inst (* seconds ms-per-second)))

(s/fdef add-seconds
  :args (s/cat :inst ::inst :seconds ::m/long)
  :ret ::inst)

(defn add-minutes
  "Adds `minutes` to `inst`, returning a new instant."
  [inst minutes]
  (add-ms inst (* minutes ms-per-minute)))

(s/fdef add-minutes
  :args (s/cat :inst ::inst :minutes ::m/long)
  :ret ::inst)

(defn add-hours
  "Adds `hours` to `inst`, returning a new instant."
  [inst hours]
  (add-ms inst (* hours ms-per-hour)))

(s/fdef add-hours
  :args (s/cat :inst ::inst :hours ::m/long)
  :ret ::inst)

(defn add-days
  "Adds `days` to `inst`, returning a new instant."
  [inst days]
  (add-ms inst (* days ms-per-day)))

(s/fdef add-days
  :args (s/cat :inst ::inst :days ::m/long)
  :ret ::inst)

(defn add-weeks
  "Adds `weeks` to `inst`, returning a new instant."
  [inst weeks]
  (add-ms inst (* weeks ms-per-week)))

(s/fdef add-weeks
  :args (s/cat :inst ::inst :weeks ::m/long)
  :ret ::inst)

;;;CALENDAR NAVIGATION
(defn start-of-day
  "Returns the start of the day (00:00:00.000 UTC) for `inst`."
  [inst]
  (let [cal (inst->calendar inst)]
    (.set cal Calendar/HOUR_OF_DAY 0)
    (.set cal Calendar/MINUTE 0)
    (.set cal Calendar/SECOND 0)
    (.set cal Calendar/MILLISECOND 0)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef start-of-day
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn end-of-day
  "Returns the end of the day (23:59:59.999 UTC) for `inst`."
  [inst]
  (let [cal (inst->calendar inst)]
    (.set cal Calendar/HOUR_OF_DAY 23)
    (.set cal Calendar/MINUTE 59)
    (.set cal Calendar/SECOND 59)
    (.set cal Calendar/MILLISECOND 999)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef end-of-day
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn start-of-month
  "Returns the start of the month (first day, 00:00:00.000 UTC) for `inst`."
  [inst]
  (let [cal (inst->calendar inst)]
    (.set cal Calendar/DAY_OF_MONTH 1)
    (.set cal Calendar/HOUR_OF_DAY 0)
    (.set cal Calendar/MINUTE 0)
    (.set cal Calendar/SECOND 0)
    (.set cal Calendar/MILLISECOND 0)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef start-of-month
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn end-of-month
  "Returns the end of the month (last day, 23:59:59.999 UTC) for `inst`."
  [inst]
  (let [cal (inst->calendar inst)
        year (inst->year inst)
        month (inst->month inst)]
    (.set cal Calendar/DAY_OF_MONTH (days-in-month [year month]))
    (.set cal Calendar/HOUR_OF_DAY 23)
    (.set cal Calendar/MINUTE 59)
    (.set cal Calendar/SECOND 59)
    (.set cal Calendar/MILLISECOND 999)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef end-of-month
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn start-of-year
  "Returns the start of the year (Jan 1, 00:00:00.000 UTC) for `inst`."
  [inst]
  (let [cal (inst->calendar inst)]
    (.set cal Calendar/MONTH 0)
    (.set cal Calendar/DAY_OF_MONTH 1)
    (.set cal Calendar/HOUR_OF_DAY 0)
    (.set cal Calendar/MINUTE 0)
    (.set cal Calendar/SECOND 0)
    (.set cal Calendar/MILLISECOND 0)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef start-of-year
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn end-of-year
  "Returns the end of the year (Dec 31, 23:59:59.999 UTC) for `inst`."
  [inst]
  (let [cal (inst->calendar inst)]
    (.set cal Calendar/MONTH 11)
    (.set cal Calendar/DAY_OF_MONTH 31)
    (.set cal Calendar/HOUR_OF_DAY 23)
    (.set cal Calendar/MINUTE 59)
    (.set cal Calendar/SECOND 59)
    (.set cal Calendar/MILLISECOND 999)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef end-of-year
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn start-of-week
  "Returns the start of the week (Sunday 00:00:00.000 UTC) for `inst`."
  [inst]
  (let [cal (inst->calendar inst)]
    (.set cal Calendar/DAY_OF_WEEK Calendar/SUNDAY)
    (.set cal Calendar/HOUR_OF_DAY 0)
    (.set cal Calendar/MINUTE 0)
    (.set cal Calendar/SECOND 0)
    (.set cal Calendar/MILLISECOND 0)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef start-of-week
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn end-of-week
  "Returns the end of the week (Saturday 23:59:59.999 UTC) for `inst`."
  [inst]
  (let [cal (inst->calendar inst)]
    (.set cal Calendar/DAY_OF_WEEK Calendar/SATURDAY)
    (.set cal Calendar/HOUR_OF_DAY 23)
    (.set cal Calendar/MINUTE 59)
    (.set cal Calendar/SECOND 59)
    (.set cal Calendar/MILLISECOND 999)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef end-of-week
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn start-of-quarter
  "Returns the start of the quarter (00:00:00.000 UTC) for `inst`.

  Quarters: Q1 (Jan-Mar), Q2 (Apr-Jun), Q3 (Jul-Sep), Q4 (Oct-Dec)"
  [inst]
  (let [cal (inst->calendar inst)
        month (.get cal Calendar/MONTH)
        quarter-start-month (* 3 (quot month 3))]
    (.set cal Calendar/MONTH quarter-start-month)
    (.set cal Calendar/DAY_OF_MONTH 1)
    (.set cal Calendar/HOUR_OF_DAY 0)
    (.set cal Calendar/MINUTE 0)
    (.set cal Calendar/SECOND 0)
    (.set cal Calendar/MILLISECOND 0)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef start-of-quarter
  :args (s/cat :inst ::inst)
  :ret ::inst)

(defn end-of-quarter
  "Returns the end of the quarter (23:59:59.999 UTC) for `inst`.

  Quarters: Q1 (Jan-Mar), Q2 (Apr-Jun), Q3 (Jul-Sep), Q4 (Oct-Dec)"
  [inst]
  (let [cal (inst->calendar inst)
        month (.get cal Calendar/MONTH)
        quarter-end-month (+ 2 (* 3 (quot month 3)))
        year (.get cal Calendar/YEAR)
        days (days-in-month [year (inc quarter-end-month)])]
    (.set cal Calendar/MONTH quarter-end-month)
    (.set cal Calendar/DAY_OF_MONTH days)
    (.set cal Calendar/HOUR_OF_DAY 23)
    (.set cal Calendar/MINUTE 59)
    (.set cal Calendar/SECOND 59)
    (.set cal Calendar/MILLISECOND 999)
    (java-date->inst-by-bounding (.getTime cal))))

(s/fdef end-of-quarter
  :args (s/cat :inst ::inst)
  :ret ::inst)

;;;INTERVAL OPERATIONS
(defn interval-duration-ms
  "Returns the duration in milliseconds of an instant interval."
  [[inst1 inst2]]
  (- (inst->in-ms inst2) (inst->in-ms inst1)))

(s/fdef interval-duration-ms
  :args (s/cat :inst-interval ::inst-interval)
  :ret (s/and ::duration-ms m/non-?))

(defn interval-contains?
  "Returns true if `inst` falls within `inst-interval` (inclusive)."
  [[inst1 inst2] inst]
  (let [ms (inst->in-ms inst)]
    (and (>= ms (inst->in-ms inst1))
      (<= ms (inst->in-ms inst2)))))

(s/fdef interval-contains?
  :args (s/cat :inst-interval ::inst-interval :inst ::inst)
  :ret boolean?)

(defn interval-overlaps?
  "Returns true if two instant intervals overlap."
  [[a1 a2] [b1 b2]]
  (and (not (.after ^Date a1 ^Date b2))
    (not (.after ^Date b1 ^Date a2))))

(s/fdef interval-overlaps?
  :args (s/cat :interval1 ::inst-interval :interval2 ::inst-interval)
  :ret boolean?)

(defn interval-intersection
  "Returns the intersection of two instant intervals, or `nil` if they don't overlap."
  [[a1 a2] [b1 b2]]
  (when (interval-overlaps? [a1 a2] [b1 b2])
    [(if (.after ^Date a1 ^Date b1) a1 b1)
     (if (.before ^Date a2 ^Date b2) a2 b2)]))

(s/fdef interval-intersection
  :args (s/cat :interval1 ::inst-interval :interval2 ::inst-interval)
  :ret (s/nilable ::inst-interval))

(defn interval-union
  "Returns the union of two instant intervals, or `nil` if they don't overlap or touch."
  [[a1 a2] [b1 b2]]
  (let [a1-ms (inst->in-ms a1)
        a2-ms (inst->in-ms a2)
        b1-ms (inst->in-ms b1)
        b2-ms (inst->in-ms b2)]
    (when (and (<= a1-ms (inc b2-ms))
            (<= b1-ms (inc a2-ms)))
      [(in-ms->inst (min a1-ms b1-ms))
       (in-ms->inst (max a2-ms b2-ms))])))

(s/fdef interval-union
  :args (s/cat :interval1 ::inst-interval :interval2 ::inst-interval)
  :ret (s/nilable ::inst-interval))

;;;FORMATTING
(defn format-inst
  "Formats `inst` as an ISO-8601 string.

  Format: YYYY-MM-DDTHH:MM:SS.sssZ

  Example: \"2024-03-15T14:30:45.123Z\""
  [inst]
  (let [cal (inst->calendar inst)
        year (.get cal Calendar/YEAR)
        month (inc (.get cal Calendar/MONTH))
        day (.get cal Calendar/DAY_OF_MONTH)
        hour (.get cal Calendar/HOUR_OF_DAY)
        minute (.get cal Calendar/MINUTE)
        second (.get cal Calendar/SECOND)
        ms (.get cal Calendar/MILLISECOND)]
    (format "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
      year month day hour minute second ms)))

(s/fdef format-inst
  :args (s/cat :inst ::inst)
  :ret string?)

(defn format-date
  "Formats `inst` as a date-only string.

  Format: YYYY-MM-DD

  Example: \"2024-03-15\""
  [inst]
  (let [cal (inst->calendar inst)]
    (format "%04d-%02d-%02d"
      (.get cal Calendar/YEAR)
      (inc (.get cal Calendar/MONTH))
      (.get cal Calendar/DAY_OF_MONTH))))

(s/fdef format-date
  :args (s/cat :inst ::inst)
  :ret string?)

;;;PARSING
(defn parse-inst
  "Parses an ISO-8601 date-time string to an instant.

  Accepts formats: `YYYY-MM-DDTHH:MM:SS.sssZ`, `YYYY-MM-DDTHH:MM:SSZ`, `YYYY-MM-DDTHH:MMZ`, `YYYY-MM-DD`.

  Returns `nil` if parsing fails or date is out of range."
  [s]
  (try
    (let [cal (Calendar/getInstance (TimeZone/getTimeZone "UTC"))
          [date-part time-part] (str/split (str/replace s #"Z$" "") #"T")
          [year month day] (map #(Long/parseLong %) (str/split date-part #"-"))]
      (.set cal Calendar/YEAR (int year))
      (.set cal Calendar/MONTH (dec (int month)))
      (.set cal Calendar/DAY_OF_MONTH (int day))
      (if time-part
        (let [time-parts (str/split time-part #"[:.]")
              hour (Long/parseLong (nth time-parts 0))
              minute (Long/parseLong (nth time-parts 1 "0"))
              second (Long/parseLong (nth time-parts 2 "0"))
              ms (Long/parseLong (nth time-parts 3 "0"))]
          (.set cal Calendar/HOUR_OF_DAY (int hour))
          (.set cal Calendar/MINUTE (int minute))
          (.set cal Calendar/SECOND (int second))
          (.set cal Calendar/MILLISECOND (int ms)))
        (do
          (.set cal Calendar/HOUR_OF_DAY 0)
          (.set cal Calendar/MINUTE 0)
          (.set cal Calendar/SECOND 0)
          (.set cal Calendar/MILLISECOND 0)))
      (let [inst (.getTime cal)]
        (when (inst-in-range? inst)
          inst)))
    (catch Exception _e
      nil)))

(s/fdef parse-inst
  :args (s/cat :s string?)
  :ret (s/nilable ::inst))

(defn parse-date
  "Parses a date-only string (`YYYY-MM-DD`) to an instant at midnight UTC. Returns `nil` if parsing fails or date is
  out of range."
  [s]
  (parse-inst s))

(s/fdef parse-date
  :args (s/cat :s string?)
  :ret (s/nilable ::inst))

