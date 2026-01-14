(ns provisdom.date.tick
  "High-precision date and duration handling with tick-based arithmetic.

  Core Concepts:
  - 'ticks' are the fundamental time unit, providing sub-microsecond precision
  - 'date' represents ticks elapsed since epoch 2070, chosen to center the
    practical range (1814-2325) 100 years beyond the Unix epoch (1970)
  - Dates and ticks combine through simple arithmetic to create new dates
  - 'months' form a separate unit since month lengths vary
  - All units decompose into structured maps for flexible manipulation

  Quick Start Guide:
  Choose your approach based on precision and performance needs:

  Calendar Approach:
  - Use for: exact date arithmetic, user-facing dates, regulatory compliance
  - Benefits: exact date landing, required when model uses real quarterly/monthly data
  - Trade-offs: more complex calculations, variable time steps

  Average Years Approach:
  - Use for: financial calculations, approximate durations, statistical analysis
  - Benefits: better performance (pre-calculations possible), equal time steps enable model
      optimization
  - Trade-offs: may land on approximate dates (off by hours/days)

  Two Types of Date Representations:
  1. Calendar Dates - Exact calendar-based dates using precise calendar arithmetic:
     - Handle actual calendar complexities like leap years, varying month lengths
     - Use date->breakdown and breakdown->date functions for conversion
     - Format: YYYY-MM-DDTHH:MM:SS.mmm.uuu:ttt (e.g., '2020-01-01T10:30:00.000.000:0')
     - Support exact date arithmetic for specific calendar operations

  2. Average Years - Simplified time calculations using statistical averages:
     - Based on average Gregorian year of 365.2425 days (ticks-per-average-year)
     - Used for approximate durations, financial calculations, and time estimates
     - Format: <number>ay (e.g., '0.383527ay' for average years)
     - Converted using ticks->average-years, date-range->average-years, and parsing/formatting
         functions

  Key Features:
  - Date arithmetic: add/subtract ticks, months, business days
  - Calendar boundaries: start/end of day, week, month, quarter, year, fiscal year
  - Date sequences: lazy sequences with configurable step (day, week, month, year)
  - Range operations: overlap detection, intersection, containment
  - Named periods: :ytd, :mtd, :qtd, :last-month, :trailing-12-months, etc.
  - Business days: add-business-days, business-days-between with holiday support
  - ISO week dates: iso-week-number, iso-week-year, format-iso-week-date
  - Relative formatting: format-relative for human-readable durations

  Common Patterns:

  Date Arithmetic:
    - Create new dates with simple arithmetic:
        (+ date-2020 (* 5 ticks-per-day))           ; 5 days after 2020-01-01
        (+ date-2020 (* 6 ticks-per-hour))          ; 6 hours after 2020-01-01
        (- date-2020 ticks-per-week)                ; 1 week before 2020-01-01
        (+ date-2020 (* 2 ticks-per-day) (* 3 ticks-per-hour))  ; 2 days + 3 hours after

    - Calculate time differences:
        ;; minutes from month start
        (/ (- date (start-of-month date)) ticks-per-minute)

        ;; minutes until month end
        (/ (- (add-months-to-date (start-of-month date) 1) date) ticks-per-minute)

  System Conversions:
    - Calendar dates to/from components:
        (date->breakdown date-2020)
        ; => {::year 2020 ::month 1 ::day-of-month 1}
        (breakdown->date {::year 2020 ::month 6 ::day-of-month 15})
        ; => -18252672000000000  ; tick value for June 15, 2020

    - Average years conversions:
        (ticks->average-years (* 365 ticks-per-day))  ; => 0.9993  ; ~1 year
        (* 1.5 ticks-per-average-year)                ; => 54151729632000000  ; 1.5 years

  Technical Design:
  Tick size ensures temporal models maintain accuracy during partitioning. The tick
  value makes 400 years divisible by microseconds and by 2^12 and all integers 1-16.
  Following Gregorian leap year rules (every 4 years, except centuries unless
  divisible by 400):
    400 years = 480 months = 20,871 weeks = 146,097 days
  Therefore:
    146097 × 24 × 60 × 60 × 1,000,000 × 11 × 13 × 8 = ticks in 400 years"
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]
    [provisdom.date.instant :as instant]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.strings :as strings])
  (:import
    (java.time Duration)
    (java.util Date)))

(declare breakdown->months
  breakdown->ticks
  parse-duration
  ticks->average-years
  ticks->java-duration)

(def ^:const date-1970
  "Unix epoch as a tick date (-3610189440000000000)."
  -3610189440000000000)
(def ^:const date-2020
  "Date constant for 2020-01-01 (-1805144140800000000)."
  -1805144140800000000)
(def ^:const date-2045
  "Date constant for 2045-01-01 (-902522649600000000)."
  -902522649600000000)
(def ^:const date-2070
  "Tick epoch date (2070-01-01) - zero point of the tick system."
  0)
(def ^:const epoch
  "Epoch year for tick system (2070)."
  2070)
;;365.2425 days
(def ^:const ticks-per-average-year
  "Ticks in an average Gregorian year (36,101,153,088,000,000)."
  36101153088000000)
;;30.436875 days
(def ^:const ticks-per-average-month
  "Ticks in an average month (3,008,429,424,000,000)."
  3008429424000000)
(def ^:const ticks-per-week
  "Ticks in a week (691,891,200,000,000)."
  691891200000000)
(def ^:const ticks-per-day
  "Ticks in a day (98,841,600,000,000)."
  98841600000000)
(def ^:const ticks-per-hour
  "Ticks in an hour (4,118,400,000,000)."
  4118400000000)
(def ^:const ticks-per-minute
  "Ticks in a minute (68,640,000,000)."
  68640000000)
(def ^:const ticks-per-second
  "Ticks in a second (1,144,000,000)."
  1144000000)
(def ^:const ticks-per-ms
  "Ticks in a millisecond (1,144,000)."
  1144000)
(def ^:const ticks-per-us
  "Ticks in a microsecond (1,144)."
  1144)
(def ^:const min-instant-ms
  "Minimum instant in milliseconds (1814-07-08) (-4,906,628,144,104)."
  -4906628144104)
(def ^:const max-instant-ms
  "Maximum instant in milliseconds (2325-06-28) (11,218,148,144,104)."
  11218148144104)
(def ^:const min-instant
  "Minimum supported instant (1814-07-08T07:44:15.896-00:00)."
  #inst"1814-07-08T07:44:15.896-00:00")
(def ^:const max-instant
  "Maximum supported instant (2325-06-28T16:15:44.104-00:00)."
  #inst"2325-06-28T16:15:44.104-00:00")
(def ^:const max-nanos
  "Maximum nanoseconds for Java Duration conversion (8,062,388,144,103,825,408)."
  8062388144103825408)
(def ^:const min-nanos
  "Minimum nanoseconds for Java Duration conversion (-8,062,388,144,103,825,408)."
  -8062388144103825408)

;;1/1144 of a microsecond
(s/def ::ticks ::m/long)
(s/def ::ticks+ ::m/long+)
(s/def ::ticks-non- ::m/long-non-)
(s/def ::date ::m/long)
;;ticks from epoch
(s/def ::date-as-double
  (s/double-in {:infinite? false :NaN? false :min m/min-long :max m/max-long}))

(s/def ::year (s/int-in 1814 2326))
(s/def ::month (s/int-in 1 13))
(s/def ::day-of-month (s/int-in 1 32))
(s/def ::years ::m/long)
(s/def ::years+ ::m/long+)
(s/def ::years-non- ::m/long-non-)
(s/def ::months ::m/long)
(s/def ::months+ ::m/long+)
(s/def ::months-non- ::m/long-non-)
(s/def ::weeks ::m/long)
(s/def ::weeks+ ::m/long+)
(s/def ::days ::m/long)
(s/def ::days+ ::m/long+)
(s/def ::hours ::m/long)
(s/def ::hours+ ::m/long+)
(s/def ::minutes ::m/long)
(s/def ::minutes+ ::m/long+)
(s/def ::seconds ::m/long)
(s/def ::seconds+ ::m/long+)
(s/def ::ms ::m/long)
(s/def ::us ::m/long)
(s/def ::duration (s/tuple ::months ::ticks))
(s/def ::date-range (s/tuple ::date ::date))
(s/def ::date-interval ::intervals/long-interval)
(s/def ::strict-date-interval (intervals/strict-interval-spec ::date))
;;formatting fractions of a second or average year
(s/def ::fraction-precision (s/int-in 0 16))
(s/def ::show-average-years? boolean?)
(s/def ::show-zeros? boolean?)

(defn date-spec
  "Creates a spec for dates within a specific range.
  
  Example:
    (date-spec {:date-min date-2020 :date-max date-2070})"
  [{:keys [date-min date-max]}]
  (m/long-spec {:min date-min :max date-max}))

(defn ticks-spec
  "Creates a spec for ticks within a specific range."
  [{:keys [ticks-min ticks-max]}]
  (m/long-spec {:min ticks-min :max ticks-max}))

(defn ticks-non--spec
  "Creates a spec for non-negative ticks up to `ticks-max`."
  [ticks-max]
  (m/long-non--spec ticks-max))

(defn ticks+-spec
  "Creates a spec for positive ticks up to `ticks-max`."
  [ticks-max]
  (m/long+-spec ticks-max))

(s/def ::java-duration
  (s/with-gen (partial instance? Duration)
    #(gen/fmap ticks->java-duration (s/gen ::ticks))))

(s/def ::ticks-in-month
  #{(* 28 ticks-per-day)
    (* 29 ticks-per-day)
    (* 30 ticks-per-day)
    (* 31 ticks-per-day)})

;;instant-ms stays in date range 1814-2325
(s/def ::instant-ms (s/int-in min-instant-ms (inc max-instant-ms)))

(defn instant-in-range?
  "Returns true if instant is within supported date range."
  [instant]
  (intervals/in-interval? [min-instant-ms max-instant-ms]
    (instant/inst->in-ms instant)))

;;instant stays in date range 1814-2325
(s/def ::instant
  (s/with-gen (s/and ::instant/java-date instant-in-range?)
    #(gen/fmap instant/in-ms->inst (s/gen ::instant-ms))))

(def time-breakdown-all
  [::hours ::minutes ::seconds ::ms ::us ::ticks])

(s/def ::time-breakdown
  (s/keys :opt [::hours ::minutes ::seconds ::ms ::us ::ticks]))

(def ticks-breakdown-all
  (vec (concat [::weeks ::days] time-breakdown-all)))

(s/def ::ticks-form
  (s/coll-of (set ticks-breakdown-all) :kind set? :into #{}))

(s/def ::ticks-breakdown
  (s/keys :opt [::weeks ::days ::hours ::minutes ::seconds ::ms ::us ::ticks]))

(s/def ::months-breakdown
  (s/keys :opt [::years ::months]))

(def date-breakdown-all
  (vec (concat [::year ::month ::day-of-month] time-breakdown-all)))

(s/def ::date-form
  (s/coll-of (set date-breakdown-all) :kind set? :into #{}))

(s/def ::core-date-breakdown
  (s/keys :req [::year ::month ::day-of-month]
    :opt [::hours ::minutes ::seconds ::ms ::us ::ticks]))

(defn- date-breakdown-day-in-month?
  [{::keys [year month day-of-month]}]
  (<= day-of-month (instant/days-in-month [year month])))

(defn- date-breakdown-in-range?
  [x]
  (let [{::keys [year month day-of-month]} x]
    (let [t (breakdown->ticks (dissoc x ::weeks ::days))]
      (and (not (anomalies/anomaly? t))
        (intervals/in-interval? [0 (dec ticks-per-day)] t)
        (cond (= year 1814)
          (or (> month 7)
            (and (= month 7)
              (or (> day-of-month 8)
                (and (= day-of-month 8) (>= t 31867145224192)))))

          (= year 2325)
          (or (< month 6)
            (and (= month 6)
              (or (< day-of-month 28)
                (and (= day-of-month 28) (<= t 66974454775807)))))

          :else
          true)))))

(s/def ::date-breakdown
  (s/and ::core-date-breakdown
    date-breakdown-day-in-month?
    date-breakdown-in-range?))

(def days-of-week
  [:sunday :monday :tuesday :wednesday :thursday :friday :saturday])

(def day-of-week->index
  "Map from day-of-week keyword to its index (Sunday=0, Saturday=6)."
  (zipmap days-of-week (range)))

(s/def ::day-of-week (set days-of-week))

(s/def ::holiday-set (s/coll-of ::date :kind set?))

(s/def ::step-unit #{:day :week :month :year})
(s/def ::step-amount ::m/long+)
(s/def ::end-date ::date)

;;;JAVA DURATION
(defn ticks->java-duration
  "Converts `ticks` to Java Duration, rounded to nearest nanosecond.
  
  Note: 1.144 ticks = 1 nanosecond."
  [ticks]
  (Duration/ofNanos (m/round' (* 1000 (/ ticks ticks-per-us)) :up)))

(s/fdef ticks->java-duration
  :args (s/cat :ticks ::ticks)
  :ret ::java-duration)

(defn java-duration->ticks-by-bounding
  "Converts Java Duration to `ticks`, bounded to long range.
  
  Rounds to nearest tick and clamps to valid range.
  Note: 1.144 ticks = 1 nanosecond."
  [java-duration]
  (let [nanos (.getNano ^Duration java-duration)
        seconds (.getSeconds ^Duration java-duration)
        ticks (+' (*' seconds ticks-per-second) (* (/ nanos 1000) ticks-per-us))]
    (m/round' (intervals/bound-by-interval [m/min-long m/max-long] ticks) :up)))

(s/fdef java-duration->ticks-by-bounding
  :args (s/cat :java-duration ::java-duration)
  :ret ::ticks)

;;;INSTANT-MS
(defn date->instant-ms
  "Converts tick `date` to milliseconds since Unix epoch.

  Precision is limited to milliseconds."
  [date]
  (m/round' (- (/ date ticks-per-ms) (/ date-1970 ticks-per-ms)) :up))

(s/fdef date->instant-ms
  :args (s/cat :date ::date)
  :ret ::instant-ms)

(defn instant-ms->date
  "Converts `instant-ms` since Unix epoch to tick date."
  [instant-ms]
  (condp = instant-ms
    min-instant-ms m/min-long
    max-instant-ms m/max-long
    (* ticks-per-ms (+ (/ date-1970 ticks-per-ms) instant-ms))))

(s/fdef instant-ms->date
  :args (s/cat :instant-ms ::instant-ms)
  :ret ::date)

(defn ms->instant-ms-by-bounding
  "Bounds `ms` to supported instant range."
  [ms]
  (intervals/bound-by-interval [min-instant-ms max-instant-ms] ms))

(s/fdef ms->instant-ms-by-bounding
  :args (s/cat :ms ::m/long)
  :ret ::instant-ms)

;;;INSTANT
(defn date->instant
  "Converts tick `date` to Java instant.
  
  Precision is limited to milliseconds."
  [date]
  (Date. ^long (date->instant-ms date)))

(s/fdef date->instant
  :args (s/cat :date ::date)
  :ret ::instant)

(defn instant->date
  "Converts Java `instant` to tick date."
  [instant]
  (instant-ms->date (instant/inst->in-ms instant)))

(s/fdef instant->date
  :args (s/cat :instant ::instant)
  :ret ::date)

(defn java-date->instant-by-bounding
  "Bounds Java Date to the supported instant range (1814-2325).
  
  Clamps dates outside the range to the boundaries."
  [java-date]
  (cond (.before ^Date java-date min-instant) min-instant
    (.after ^Date java-date max-instant) max-instant
    :else java-date))

(s/fdef java-date->instant-by-bounding
  :args (s/cat :java-date ::instant/java-date)
  :ret ::instant)

;;;TIME
(defn- parse-time
  [time-string]
  (let [anomaly {::anomalies/category ::anomalies/exception
                 ::anomalies/message  "bad time-string"
                 ::anomalies/fn       (var parse-time)}]
    (condp = (count (filter (fn [c]
                              (= (str c) ":"))
                      time-string))
      3
      (let [r (map (fn [sub]
                     (when-not (= "" sub)
                       (read-string (str sub))))
                (str/split time-string #":|\."))
            not-all-longs? (some false? (map m/long? r))]
        (if not-all-longs?
          anomaly
          (let [[hours minutes seconds ms us ticks] r
                t (+' (*' hours ticks-per-hour)
                    (*' minutes ticks-per-minute)
                    (*' seconds ticks-per-second)
                    (*' ms ticks-per-ms)
                    (*' us ticks-per-us)
                    ticks)]
            (if (intervals/in-interval? [m/min-long m/max-long] t)
              (long t)
              anomaly))))

      2
      (let [[hours minutes seconds] (map (fn [sub]
                                           (when-not (= "" sub)
                                             (read-string (str sub))))
                                      (str/split time-string #":"))
            [seconds ticks] (if (m/num? seconds)
                              (m/quot-and-mod' (* seconds ticks-per-second) ticks-per-second)
                              [0 0])
            ticks (m/round' ticks :up)
            r [hours minutes seconds ticks]
            not-all-longs? (some false? (map m/long? r))]
        (if not-all-longs?
          anomaly
          (let [[hours minutes seconds ticks] r
                t (+' (*' hours ticks-per-hour)
                    (*' minutes ticks-per-minute)
                    (*' seconds ticks-per-second)
                    ticks)]
            (if (intervals/in-interval? [m/min-long m/max-long] t)
              (long t)
              anomaly))))

      anomaly)))

(s/fdef parse-time
  :args (s/cat :time-string string?)
  :ret (s/or :ticks ::ticks
         :anomaly ::anomalies/anomaly))

;;;TICKS
(def lookup-of-ticks-per
  {::weeks   ticks-per-week
   ::days    ticks-per-day
   ::hours   ticks-per-hour
   ::minutes ticks-per-minute
   ::seconds ticks-per-second
   ::ms      ticks-per-ms
   ::us      ticks-per-us
   ::ticks   1})

(defn ticks->breakdown
  "Breaks down `ticks` into time units.

  Returns a map with keys like `::weeks`, `::days`, `::hours`, `::minutes`, `::seconds`, `::ms`, `::us`, and `::ticks`.
  Optional `ticks-form` set specifies which units to include.

  Example:
    (ticks->breakdown 123456789)
    ; => {::days 1 ::hours 10 ::minutes 23 ...}"
  ([ticks] (ticks->breakdown ticks (set ticks-breakdown-all)))
  ([ticks ticks-form]
   (let [want-ticks? (contains? ticks-form ::ticks)
         ticks-form (conj ticks-form ::ticks)
         m (first (reduce (fn [[acc t] k]
                            (if (contains? ticks-form k)
                              (let [t-per (get lookup-of-ticks-per k)
                                    [n t] (when t-per
                                            (m/quot-and-rem' t t-per))]
                                (if n
                                  [(assoc acc k n) t]
                                  [acc t]))
                              [acc t]))
                    [{} ticks]
                    ticks-breakdown-all))]
     (if (and (not want-ticks?) (zero? (get m ::ticks 0)))
       (dissoc m ::ticks)
       m))))

(s/fdef ticks->breakdown
  :args (s/cat :ticks ::ticks
          :ticks-form (s/? ::ticks-form))
  :ret ::ticks-breakdown)

(defn breakdown->ticks
  "Converts a `ticks-breakdown` map back to total ticks. Accepts a map with time unit keys and returns the total
  ticks. Returns an anomaly if the result exceeds long range.

  Example:
    (breakdown->ticks {::hours 2 ::minutes 30})"
  [ticks-breakdown]
  (let [{::keys [weeks days hours minutes seconds ms us ticks]
         :or    {weeks   0, days 0, hours 0, minutes 0,
                 seconds 0, ms 0, us 0, ticks 0}} ticks-breakdown
        t (+' (*' weeks ticks-per-week)
            (*' days ticks-per-day)
            (*' hours ticks-per-hour)
            (*' minutes ticks-per-minute)
            (*' seconds ticks-per-second)
            (*' ms ticks-per-ms)
            (*' us ticks-per-us)
            ticks)]
    (if (intervals/in-interval? [m/min-long m/max-long] t)
      (long t)
      {::anomalies/category ::anomalies/exception
       ::anomalies/message  (str "ticks out of long range: " t)
       ::anomalies/fn       (var breakdown->ticks)})))

(s/fdef breakdown->ticks
  :args (s/cat :ticks-breakdown ::ticks-breakdown)
  :ret (s/or :ticks ::ticks
         :anomaly ::anomalies/anomaly))

(defn format-ticks
  "Formats `ticks` as a human-readable string.
  
  Format: <weeks>w<days>dh<hours>m<minutes>s<seconds>.<fractional-seconds>
          or <average-years>ay when `show-average-years?` is true
  
  Function takes a map with:
  - ::ticks (required): The ticks' value to format
  - ::fraction-precision (optional): Shows fractional parts with specified
       precision. Default is 6.
  - ::show-zeros? (optional): When true shows zero weeks, days, hours, minutes,
       and seconds. Default is false (hide zeros).
  - ::show-average-years? (optional): When true, shows ticks as average years
       (ay) instead of detailed time components. Default is true.
  
  Example:
    (format-ticks {`::ticks` 123456789})
    ; => \"0.000003ay\"

    (format-ticks {`::show-average-years?` false
                   `::ticks` 123456789})
    ; => \"00.107917s\"
    
    (format-ticks {`::fraction-precision` 4
                   `::show-average-years?` false
                   `::ticks` 123456789})
    ; => \"00.1079s\"
    
    (format-ticks {`::show-average-years?` false
                   `::show-zeros?` true
                   `::ticks` 123456789})
    ; => \"0w0d00h00m00.107917s\"
    "
  [{::keys [ticks fraction-precision show-zeros? show-average-years?]
    :or    {fraction-precision  6
            show-zeros?         false
            show-average-years? true}}]
  (if show-average-years?
    ;; Format as average years
    (let [average-years (ticks->average-years ticks)]
      (if (or show-zeros? (not (zero? average-years)))
        (str (format (str "%." fraction-precision "f") average-years) "ay")
        ""))
    ;; Format as detailed time components
    (let [f2 (partial format "%02d")
          {::keys [weeks days hours minutes seconds ms us ticks]
           :or    {weeks 0, days 0, hours 0, minutes 0, seconds 0,
                   ms    0, us 0, ticks 0}
           :as    breakdown} (ticks->breakdown ticks)
          ticks (breakdown->ticks
                  (dissoc breakdown ::weeks ::days ::hours ::minutes ::seconds))
          seconds-fraction (double (/ ticks ticks-per-second))
          total-seconds (+ seconds seconds-fraction)
          weeks-part (when (or show-zeros? (not (zero? weeks)))
                       (str weeks "w"))
          days-part (when (or show-zeros? (not (zero? days)))
                      (str days "d"))
          hours-part (when (or show-zeros? (not (zero? hours)))
                       (str (f2 hours) "h"))
          minutes-part (when (or show-zeros? (not (zero? minutes)))
                         (str (f2 minutes) "m"))
          seconds-part (when (or show-zeros? (not (zero? total-seconds)))
                         (str (format (str "%0" (+ 3 fraction-precision) "." fraction-precision "f")
                                total-seconds) "s"))]
      (str weeks-part days-part hours-part minutes-part seconds-part))))

(s/fdef format-ticks
  :args (s/cat :format-map (s/keys :req [::ticks]
                             :opt [::fraction-precision
                                   ::show-average-years?
                                   ::show-zeros?]))
  :ret string?)

(defn- read-number
  [s anomaly else]
  (if (and s (not= "" s))
    (try (let [s2 (strings/trim-start s "0")
               s2 (if (= s2 "") "0" s2)
               ;; Handle case where trimming leaves ".xxx" 
               s2 (if (str/starts-with? s2 ".") (str "0" s2) s2)]
           (read-string s2))
      (catch Exception _ anomaly))
    else))

(defn parse-ticks
  "Parses a `ticks-string` into ticks.
  
  Accepts formats:
  - Average years: <average-years>ay (e.g., \"0.383527ay\")
  - Detailed time: <weeks>w<days>d<hours>h<minutes>m<seconds>s
  
  Returns ticks as a long or an anomaly if parsing fails.
  
  Examples:
    (parse-ticks \"0.383527ay\")
    (parse-ticks \"1w2d03h45m30.123456s\")"
  [ticks-string]
  (let [s ticks-string
        anomaly {::anomalies/category ::anomalies/exception
                 ::anomalies/message  "bad ticks-string"
                 ::anomalies/fn       (var parse-ticks)}]
    ;; Check if this is average years format
    (if (str/ends-with? s "ay")
      ;; Parse average years format
      (let [average-years-str (subs s 0 (- (count s) 2))
            average-years (read-number average-years-str anomaly nil)]
        (if (number? average-years)
          (long (* average-years ticks-per-average-year))
          anomaly))
      ;; Parse detailed time format
      (let [;; Parse components using regex to extract number-unit pairs
            weeks (if-let [match (re-find #"(\d+)w" s)]
                    (read-number (second match) anomaly 0)
                    0)
            days (if-let [match (re-find #"(\d+)d" s)]
                   (read-number (second match) anomaly 0)
                   0)
            hours (if-let [match (re-find #"(\d+)h" s)]
                    (read-number (second match) anomaly 0)
                    0)
            minutes (if-let [match (re-find #"(\d+)m" s)]
                      (read-number (second match) anomaly 0)
                      0)
            seconds-match (re-find #"([0-9]+(?:\.[0-9]+)?)s" s)
            seconds-decimal (if seconds-match
                              (read-number (second seconds-match) anomaly 0.0)
                              0.0)
            seconds-whole (if (number? seconds-decimal) (long seconds-decimal) 0)
            seconds-fractional (if (number? seconds-decimal)
                                 (- seconds-decimal seconds-whole)
                                 0.0)
            fractional-ticks (if (number? seconds-fractional)
                               (long (* seconds-fractional ticks-per-second))
                               0)
            not-all-longs? (some false? (map m/long? [weeks days hours minutes seconds-whole]))]
        (if (or not-all-longs? (and seconds-match (not (number? seconds-decimal))))
          anomaly
          (breakdown->ticks
            {::weeks   weeks
             ::days    days
             ::hours   hours
             ::minutes minutes
             ::seconds seconds-whole
             ::ticks   fractional-ticks}))))))

(s/fdef parse-ticks
  :args (s/cat :ticks-string string?)
  :ret (s/or :ticks ::ticks
         :anomaly ::anomalies/anomaly))

;;;MONTHS
(defn months->breakdown
  "Breaks down `months` into years and remaining months.
  
  Example:
    (months->breakdown 15)
    ; => {::years 1 ::months 3}"
  [months]
  (let [[years months] (m/quot-and-rem' months 12)]
    {::years  years
     ::months months}))

(s/fdef months->breakdown
  :args (s/cat :months ::months)
  :ret ::months-breakdown)

(defn breakdown->months
  "Converts a `months-breakdown` back to total months. Returns total months or an anomaly if out of range."
  [months-breakdown]
  (let [{::keys [years months]
         :or    {years 0, months 0}} months-breakdown
        m (+' months (*' years 12))]
    (if (intervals/in-interval? [m/min-long m/max-long] m)
      (long m)
      {::anomalies/category ::anomalies/exception
       ::anomalies/message  (str "months out of long range: " m)
       ::anomalies/fn       (var breakdown->months)})))

(s/fdef breakdown->months
  :args (s/cat :months-breakdown ::months-breakdown)
  :ret (s/or :months ::months
         :anomaly ::anomalies/anomaly))

;;;DATES
(defn date$
  "Returns the current date as ticks.
  
  Precision limited to milliseconds."
  []
  (instant->date (instant/inst$)))

(defn date->breakdown
  "Breaks down a tick `date` into calendar and time components.

  Returns a map with `::year`, `::month`, `::day-of-month` and optionally time units like `::hours`, `::minutes`, etc.
  Optional `date-form` set specifies which components to include. Use empty set `#{}` for just date components.

  Example:
    (date->breakdown date-2020)
    ; => {`::year` 2020 `::month` 1 `::day-of-month` 1}"
  ([date] (date->breakdown date (set date-breakdown-all)))
  ([date date-form]
   (let [[year ticks] (if (> date date-2045)
                        [2070 (- date date-2070)]
                        [2020 (- date date-2020)])
         [days ticks] (m/quot-and-mod' ticks ticks-per-day)
         [day month year] (loop [day (inc days)
                                 month 1
                                 year year]
                            (let [dm (instant/days-in-month [year month])]
                              (if (> day dm)
                                (recur (- day dm)
                                  (if (== month 12) 1 (inc month))
                                  (if (== month 12) (inc year) year))
                                [day month year])))
         [day month year] (loop [day day
                                 month month
                                 year year]
                            (if (m/non+? day)
                              (let [new-mo (if (m/one? month) 12 (dec month))]
                                (recur (+ day (instant/days-in-month [year new-mo]))
                                  new-mo
                                  (if (m/one? month) (dec year) year)))
                              [day month year]))
         ticks-bd (ticks->breakdown ticks
                    (set/difference date-form #{::year ::month ::day-of-month ::weeks ::days}))]
     (merge {::year         year
             ::month        month
             ::day-of-month day}
       ticks-bd))))

(s/fdef date->breakdown
  :args (s/cat :date ::date
          :date-form (s/? ::date-form))
  :ret ::date-breakdown)

(defn breakdown->date
  "Converts a `date-breakdown` map to tick date. Accepts a map with `::year`, `::month`, `::day-of-month` and optional
  time components. Returns ticks from epoch (2070). The date must be between 1814-07-08 and 2325-06-28.

  Example:
    (breakdown->date {`::year` 2020 `::month` 1 `::day-of-month` 1})"
  [date-breakdown]
  (let [{::keys [year month day-of-month]} date-breakdown
        ticks (breakdown->ticks (dissoc date-breakdown ::weeks ::days))
        days (+' (instant/days-until-month [year month])    ;includes leap-days
               ;;don't want to double-count leap-days
               (instant/passed-leap-days [epoch 1] [year 1])
               (* 365 (+' year (- epoch)))
               (dec day-of-month))
        date (+' (*' ticks-per-day days) ticks)]
    (long date)))

(s/fdef breakdown->date
  :args (s/cat :date-breakdown ::date-breakdown)
  :ret ::date)

(defn java-date->date-by-bounding
  "Converts Java Date to tick date, bounded to supported range. Clamps `java-date` outside 1814-2325 to the
  boundaries."
  [java-date]
  (instant->date (java-date->instant-by-bounding java-date)))

(s/fdef java-date->date-by-bounding
  :args (s/cat :java-date ::instant/java-date)
  :ret ::date)

(defn date-breakdown?
  "Returns true if `x` is a valid date breakdown. Checks that the breakdown has valid date components and is within
  the supported date range."
  [x]
  (and (s/valid? ::core-date-breakdown x)
    (date-breakdown-day-in-month? x)
    (date-breakdown-in-range? x)))

(s/fdef date-breakdown?
  :args (s/cat :x any?)
  :ret boolean?)

(defn format-date
  "Formats tick `date` as an ISO-like string.
  
  Format: YYYY-MM-DDTHH:MM:SS.mmm.uuu:ttt
  
  Optional `seconds-fraction-precision` shows seconds as decimal.
  
  Example:
    (format-date date-2020)
    ; => \"2020-01-01T00:00:00.000.000:0\""
  ([date]
   (let [f2 (partial format "%02d")
         f3 (partial format "%03d")
         {::keys [year month day-of-month hours minutes seconds ms us ticks]
          :or    {hours 0, minutes 0, seconds 0, ms 0, us 0, ticks 0}} (date->breakdown date)]
     (str (f2 year) "-" (f2 month) "-" (f2 day-of-month) "T" (f2 hours) ":" (f2 minutes) ":"
       (f2 seconds) "." (f3 ms) "." (f3 us) ":" ticks)))
  ([date seconds-fraction-precision]
   (let [f2 (partial format "%02d")
         {::keys [year month day-of-month hours minutes seconds ms us ticks]
          :or    {hours 0, minutes 0, seconds 0, ms 0, us 0, ticks 0}
          :as    breakdown} (date->breakdown date)
         ticks (breakdown->ticks (dissoc breakdown ::weeks ::days ::hours ::minutes ::seconds))
         seconds-fraction (double (/ ticks ticks-per-second))]
     (str (f2 year) "-" (f2 month) "-" (f2 day-of-month) "T" (f2 hours) ":" (f2 minutes) ":"
       (format (str "%0" (+ 3 seconds-fraction-precision) "." seconds-fraction-precision "f")
         (+ seconds seconds-fraction))))))

(s/fdef format-date
  :args (s/cat :date ::date
          :seconds-fraction-precision (s/? ::fraction-precision))
  :ret string?)

(defn parse-date
  "Parses a `date-string` into tick date.
  
  Accepts format: YYYY-MM-DDTHH:MM:SS.mmm.uuu:ttt
  
  Returns tick date or an anomaly if parsing fails.
  
  Example:
    (parse-date \"2020-01-01T00:00:00.000.000:0\")"
  [date-string]
  (let [s (str/split date-string #"-|T")
        [s1 s2 s3 s4] s
        anomaly {::anomalies/category ::anomalies/exception
                 ::anomalies/message  "bad date-string"
                 ::anomalies/fn       (var parse-date)}
        year (read-number s1 anomaly 2170)
        month (read-number s2 anomaly 1)
        day-of-month (read-number s3 anomaly 0)
        ticks (when s4 (parse-time s4))
        bd {::year         year
            ::month        month
            ::day-of-month day-of-month
            ::ticks        ticks}]
    (if (date-breakdown? bd)
      (breakdown->date bd)
      anomaly)))

(s/fdef parse-date
  :args (s/cat :date-string string?)
  :ret (s/or :date ::date
         :anomaly ::anomalies/anomaly))

(defn add-months-to-date
  "Adds `months` to a tick `date`. Preserves day-of-month when possible. Returns anomaly if the resulting date is
  invalid (e.g., Feb 30).

  Example:
    (add-months-to-date date-2020 3)  ; 3 months later
    (add-months-to-date date-2020 -6) ; 6 months earlier"
  [date months]
  (let [{::keys [year month]
         :as    date-breakdown} (date->breakdown date #{})
        [years months] (m/quot-and-mod' (dec (+ months month)) 12)
        year (+ year years)
        month (inc months)
        date-breakdown (assoc date-breakdown ::year year
                         ::month month)]
    (if (date-breakdown? date-breakdown)
      (breakdown->date date-breakdown)
      {::anomalies/category ::anomalies/exception
       ::anomalies/message  "bad date"
       ::anomalies/fn       (var add-months-to-date)})))

(s/fdef add-months-to-date
  :args (s/cat :date ::date
          :months ::months)
  :ret (s/or :date ::date
         :anomaly ::anomalies/anomaly))

(defn day-of-week
  "Returns the day of week for a given `date`.

  Returns one of: `:monday`, `:tuesday`, `:wednesday`, `:thursday`, `:friday`, `:saturday`, `:sunday`.

  Example:
    (day-of-week date-2020) ; => :wednesday"
  [date]
  (nth days-of-week (m/mod' (+ (m/quot' date ticks-per-day) 3) 7)))

(s/fdef day-of-week
  :args (s/cat :date ::date)
  :ret ::day-of-week)

(defn start-of-year
  "Returns the first moment of the year containing `date`.
  
  Example:
    (start-of-year some-date-in-2020) ; => 2020-01-01T00:00:00"
  [date]
  (let [date-breakdown (assoc (dissoc (date->breakdown date #{}) ::ticks)
                         ::month 1
                         ::day-of-month 1)]
    (if (date-breakdown? date-breakdown)
      (breakdown->date date-breakdown)
      {::anomalies/fn       (var start-of-year)
       ::anomalies/message  "not a valid date"
       ::anomalies/category ::anomalies/exception})))

(s/fdef start-of-year
  :args (s/cat :date ::date)
  :ret (s/or :date ::date
         :anomaly ::anomalies/anomaly))

(defn end-of-year
  "Returns the first moment of the next year after `date`.
  
  Example:
    (end-of-year some-date-in-2020) ; => 2021-01-01T00:00:00"
  [date]
  (let [date-breakdown (update
                         (assoc (dissoc (date->breakdown date #{}) ::ticks)
                           ::month 1
                           ::day-of-month 1)
                         ::year
                         inc)]
    (if (date-breakdown? date-breakdown)
      (breakdown->date date-breakdown)
      {::anomalies/fn       (var end-of-year)
       ::anomalies/message  "not a valid date"
       ::anomalies/category ::anomalies/exception})))

(s/fdef end-of-year
  :args (s/cat :date ::date)
  :ret (s/or :date ::date
         :anomaly ::anomalies/anomaly))

(defn start-of-month
  "Returns the first moment of the month containing `date`.
  
  Example:
    (start-of-month some-date-in-march) ; => 2020-03-01T00:00:00"
  [date]
  (let [date-breakdown (assoc (dissoc (date->breakdown date #{}) ::ticks) ::day-of-month 1)]
    (if (date-breakdown? date-breakdown)
      (breakdown->date date-breakdown)
      {::anomalies/fn       (var start-of-month)
       ::anomalies/message  "not a valid date"
       ::anomalies/category ::anomalies/exception})))

(s/fdef start-of-month
  :args (s/cat :date ::date)
  :ret (s/or :date ::date
         :anomaly ::anomalies/anomaly))

(defn end-of-month
  "Returns the first moment of the next month after `date`.
  
  Example:
    (end-of-month some-date-in-march) ; => 2020-04-01T00:00:00"
  [date]
  (let [{::keys [year month]
         :as    date-breakdown} (date->breakdown date #{})
        [year month] (if (= month 12)
                       [(inc year) 1]
                       [year (inc month)])
        date-breakdown (assoc (dissoc date-breakdown ::ticks)
                         ::year year
                         ::month month
                         ::day-of-month 1)]
    (if (date-breakdown? date-breakdown)
      (breakdown->date date-breakdown)
      {::anomalies/fn       (var end-of-month)
       ::anomalies/message  "not a valid date"
       ::anomalies/category ::anomalies/exception})))

(s/fdef end-of-month
  :args (s/cat :date ::date)
  :ret (s/or :date ::date
         :anomaly ::anomalies/anomaly))

(defn start-of-day
  "Returns the first moment (midnight) of the day containing `date`.
  
  Example:
    (start-of-day some-datetime) ; => YYYY-MM-DDTOO:00:00"
  [date]
  (let [bd (dissoc (date->breakdown date #{}) ::ticks)]
    (if (date-breakdown? bd)
      (breakdown->date bd)
      {::anomalies/fn       (var start-of-day)
       ::anomalies/message  "not a valid date"
       ::anomalies/category ::anomalies/exception})))

(s/fdef start-of-day
  :args (s/cat :date ::date)
  :ret (s/or :date ::date
         :anomaly ::anomalies/anomaly))

(defn end-of-day
  "Returns the first moment (midnight) of the next day after `date`.
  
  Example:
    (end-of-day some-datetime) ; => next day at 00:00:00"
  [date]
  (let [{::keys [year month day-of-month]
         :as    date-breakdown} (date->breakdown date #{})
        [month day-of-month] (if (= day-of-month (instant/days-in-month [year month]))
                               [(inc month) 1]
                               [month (inc day-of-month)])
        [year month] (if (= month 13)
                       [(inc year) 1]
                       [year month])
        date-breakdown (assoc (dissoc date-breakdown ::ticks)
                         ::year year
                         ::month month
                         ::day-of-month day-of-month)]
    (if (date-breakdown? date-breakdown)
      (breakdown->date date-breakdown)
      {::anomalies/fn       (var end-of-day)
       ::anomalies/message  "not a valid date"
       ::anomalies/category ::anomalies/exception})))

(s/fdef end-of-day
  :args (s/cat :date ::date)
  :ret (s/or :date ::date
         :anomaly ::anomalies/anomaly))

(defn start-of-week
  "Returns the first moment (midnight) of the week containing `date`.

  Weeks start on Sunday."
  [date]
  (let [dow-index (day-of-week->index (day-of-week date))
        days-to-subtract (* dow-index ticks-per-day)]
    (start-of-day (- date days-to-subtract))))

(s/fdef start-of-week
  :args (s/cat :date ::date)
  :ret ::date)

(defn end-of-week
  "Returns the first moment (midnight) of the next week after `date`.

  Weeks start on Sunday."
  [date]
  (let [dow-index (day-of-week->index (day-of-week date))
        days-to-add (* (- 7 dow-index) ticks-per-day)]
    (start-of-day (+ date days-to-add))))

(s/fdef end-of-week
  :args (s/cat :date ::date)
  :ret ::date)

(defn start-of-quarter
  "Returns the first moment of the quarter containing `date`.

  Quarters: Q1 (Jan-Mar), Q2 (Apr-Jun), Q3 (Jul-Sep), Q4 (Oct-Dec)."
  [date]
  (let [{::keys [year month]} (date->breakdown date #{})
        quarter-start-month (inc (* 3 (quot (dec month) 3)))]
    (breakdown->date {::year         year
                      ::month        quarter-start-month
                      ::day-of-month 1})))

(s/fdef start-of-quarter
  :args (s/cat :date ::date)
  :ret ::date)

(defn end-of-quarter
  "Returns the first moment of the next quarter after `date`.

  Quarters: Q1 (Jan-Mar), Q2 (Apr-Jun), Q3 (Jul-Sep), Q4 (Oct-Dec)."
  [date]
  (let [{::keys [year month]} (date->breakdown date #{})
        quarter-index (quot (dec month) 3)
        next-quarter-month (+ 4 (* 3 quarter-index))
        [year month] (if (> next-quarter-month 12)
                       [(inc year) 1]
                       [year next-quarter-month])]
    (breakdown->date {::year         year
                      ::month        month
                      ::day-of-month 1})))

(s/fdef end-of-quarter
  :args (s/cat :date ::date)
  :ret ::date)

(defn ticks-in-month
  "Returns the number of ticks in the month containing `date`. Accounts for varying month lengths and leap years."
  [date]
  (let [{::keys [year month]} (date->breakdown date)]
    (* (instant/days-in-month [year month]) ticks-per-day)))

(s/fdef ticks-in-month
  :args (s/cat :date ::date)
  :ret ::ticks-in-month)

;;;DATE RANGE
(defn- date-range->duration-raw
  [[start-date end-date]]
  (let [{start-year  ::year
         start-month ::month
         start-days  ::day-of-month
         start-ticks ::ticks} (date->breakdown start-date #{})
        {end-year  ::year
         end-month ::month
         end-days  ::day-of-month
         end-ticks ::ticks} (date->breakdown end-date #{})
        months (+ (* 12 (- end-year start-year)) (- end-month start-month))
        ticks (+ (* ticks-per-day (- end-days start-days)) (- (or end-ticks 0) (or start-ticks 0)))]
    [months ticks]))

(defn months-difference
  "Returns the calendar month difference between two dates.
  
  Example:
    (months-difference [start-date end-date]) ; => 6"
  [date-range]
  (first (date-range->duration-raw date-range)))

(s/fdef months-difference
  :args (s/cat :date-range ::date-range)
  :ret ::months)

(defn date-range->duration
  "Converts date range to calendar months plus remaining ticks.
  
  Returns [months ticks] tuple.
  
  Example:
    (date-range->duration [start end])
    ; => [6 12345678] ; 6 months + some ticks"
  [[start-date end-date]]
  (let [months (months-difference [start-date end-date])
        new-start-date (add-months-to-date start-date months)]
    (if (anomalies/anomaly? new-start-date)
      new-start-date
      [months (- end-date new-start-date)])))

(s/fdef date-range->duration
  :args (s/cat :date-range ::date-range)
  :ret (s/or :duration ::duration
         :anomaly ::anomalies/anomaly))

(defn date-range->months-floor
  "Returns the floor of months and remaining ticks from a date-range."
  [[start-date end-date]]
  (let [[months ticks] (date-range->duration-raw [start-date end-date])]
    (if (neg? ticks)
      (let [new-start-date (add-months-to-date start-date (dec months))]
        (if (anomalies/anomaly? new-start-date)
          new-start-date
          [(dec months) (- end-date new-start-date)]))
      [months ticks])))

(s/fdef date-range->months-floor
  :args (s/cat :date-range ::date-range)
  :ret (s/or :duration ::duration
         :anomaly ::anomalies/anomaly))

(defn date-range->months-ceil
  "Returns the ceil of months and remaining ticks from a date-range."
  [[start-date end-date]]
  (let [[months ticks] (date-range->duration-raw [start-date end-date])]
    (if (pos? ticks)
      (let [new-start-date (add-months-to-date start-date (inc months))]
        (if (anomalies/anomaly? new-start-date)
          new-start-date
          [(inc months) (- end-date new-start-date)]))
      [months ticks])))

(s/fdef date-range->months-ceil
  :args (s/cat :date-range ::date-range)
  :ret (s/or :duration ::duration
         :anomaly ::anomalies/anomaly))

(defn date-range->prorated-months
  "Converts date range to the prorated (fractional) months. Accounts for partial months at the start and end of the
  range.

  Example:
    (date-range->prorated-months [start end])
    ; => 6.23 ; 6.23 months"
  [[start-date end-date]]
  (let [end-of-start-month (end-of-month start-date)
        start-of-end-month (start-of-month end-date)
        whole-months (dec (first (date-range->duration [end-of-start-month start-of-end-month])))
        end-month-ticks (double (ticks-in-month end-date))]
    (max 0.0
      (if (neg? whole-months)
        (/ (- end-date (double start-date)) end-month-ticks)
        (let [start-month-ticks (ticks-in-month start-date)
              start-ticks-remaining (- end-of-start-month (double start-date))
              end-ticks-along (- end-date start-of-end-month)]
          (+ whole-months
            (/ start-ticks-remaining start-month-ticks)
            (/ end-ticks-along end-month-ticks)))))))

(s/fdef date-range->prorated-months
  :args (s/cat :date-range ::date-range)
  :ret (s/or :prorated-months (m/finite-non--spec 6130.686379808011)
         :anomaly ::anomalies/anomaly))

(defn format-duration
  "Formats `duration` as a human-readable string.

  Format:
  <years>y<months>mo<weeks>W<days>D<hours>h<minutes>m<seconds>s.<fractional-seconds>

  Takes a map with:
  - ::duration (required): The [months ticks] duration tuple to format
  - ::fraction-precision (optional): Shows fractional parts with specified precision. Default is 6.
  - ::show-zeros? (optional): When true shows zero weeks, days, hours, minutes,
         and seconds. Default is false (hide zeros).
  - ::show-average-years? (optional): When true, shows ticks as average years (ay) instead of
         detailed time components. Default is true.

  Example:
    (format-duration {::duration [3 123456789]})
    ; => \"3mo00.107917s\"

    (format-duration {::duration [15 123456789] ::fraction-precision 4})
    ; => \"1y3mo00.1079s\"
    
    (format-duration {::duration [3 123456789] ::show-zeros? true})
    ; => \"0y3mo0w0d00h00m00.107917s\"
    
    (format-duration {::duration [3 123456789] ::show-average-years? true})
    ; => \"3mo0.000003ay\"
    
    (format-duration {::duration [15 123456789]
                      ::fraction-precision 4
                      ::show-average-years? true})
    ; => \"1y3mo0.0000ay\"
    "
  [{::keys [duration fraction-precision show-average-years? show-zeros?]
    :or    {fraction-precision  6
            show-average-years? true
            show-zeros?         false}}]
  (let [[months ticks] duration
        {::keys [months years]} (months->breakdown months)
        years-part (when (or show-zeros? (not (zero? years)))
                     (str years "y"))
        months-part (when (or show-zeros? (not (zero? months)))
                      (str months "mo"))
        ticks-part (format-ticks
                     (cond->
                       {::show-average-years? show-average-years?
                        ::show-zeros?         show-zeros?
                        ::ticks               ticks}
                       fraction-precision (assoc ::fraction-precision fraction-precision)))]
    (str years-part months-part ticks-part)))

(s/fdef format-duration
  :args (s/cat :format-map (s/keys :req [::duration]
                             :opt [::fraction-precision
                                   ::show-average-years?
                                   ::show-zeros?]))
  :ret string?)

(defn parse-duration
  "Parses a `duration-string` into a duration tuple [months ticks].
  
  Accepts formats produced by format-duration:
  - Standard: \"3mo20w01h18m17.923283s\"
  - With years: \"1y3mo20w01h18m17.9233s\"  
  - With average years: \"3mo0.000003ay\"
  - With show-zeros: \"0y3mo0w0d00h00m00.107917s\"
  
  The ticks portion is parsed using parse-ticks, which automatically
  handles both detailed time format and average years format.
  
  Returns [months ticks] tuple or an anomaly if parsing fails.
  
  Examples:
    (parse-duration \"1y3mo20w01h18m17.9233s\")
    ; => [15 13843198424235230]
    
    (parse-duration \"3mo0.383527ay\")
    ; => [3 13843198424235230]"
  [duration-string]
  (let [s duration-string
        anomaly {::anomalies/category ::anomalies/exception
                 ::anomalies/message  "bad duration-string"
                 ::anomalies/fn       (var parse-duration)}
        ;; Parse years and months (handle negative values)
        years (if-let [match (re-find #"(-?\d+)y" s)]
                (let [parsed (read-number (second match) anomaly 0)]
                  (if (m/long? parsed) parsed 0))
                0)
        months-only (if-let [match (re-find #"(-?\d+)mo" s)]
                      (let [parsed (read-number (second match) anomaly 0)]
                        (if (m/long? parsed) parsed 0))
                      0)
        total-months (+ (* years 12) months-only)
        ;; Parse the ticks portion (everything that's not years/months)
        ;; parse-ticks now handles both detailed time format and average years
        ticks-string (-> s
                       (str/replace #"-?\d+y" "")
                       (str/replace #"-?\d+mo" ""))
        ticks (if (str/blank? ticks-string)
                0
                (parse-ticks ticks-string))
        not-all-valid? (or (some false? (map m/long? [years months-only total-months]))
                         (anomalies/anomaly? ticks))]
    (if not-all-valid?
      anomaly
      [total-months ticks])))

(s/fdef parse-duration
  :args (s/cat :duration-string string?)
  :ret (s/or :anomaly ::anomalies/anomaly
         :duration ::duration))

;;;AVERAGE YEARS
(defn ticks->average-years
  "Converts `ticks` to average years. Uses the average year length of 365.2425 days."
  [ticks]
  (/ ticks (double ticks-per-average-year)))

(s/fdef ticks->average-years
  :args (s/cat :ticks ::ticks)
  :ret ::instant/average-years)

(def ^:const max-average-years
  "Maximum average years that can be converted to ticks without overflow."
  (/ m/max-long (double ticks-per-average-year)))

(def ^:const min-average-years
  "Minimum average years that can be converted to ticks without overflow."
  (/ m/min-long (double ticks-per-average-year)))

(defn average-years->ticks
  "Converts `average-years` to ticks. Uses the average year length of 365.2425 days. Input must be in range
  `[min-average-years, max-average-years]` (approximately +/-255 years) to avoid long overflow."
  [average-years]
  (long (* average-years ticks-per-average-year)))

(s/fdef average-years->ticks
  :args (s/cat :average-years (s/and ::instant/average-years
                                #(<= min-average-years % max-average-years)))
  :ret ::ticks)

(defn date-range->average-years
  "Converts `date-range` to average years."
  [[start-date end-date]]
  (/ (- end-date (double start-date)) ticks-per-average-year))

(s/fdef date-range->average-years
  :args (s/cat :date-range ::date-range)
  :ret ::instant/average-years)

;;;PREDICATES
(defn weekend?
  "Returns true if `date` falls on Saturday or Sunday."
  [date]
  (let [dow (day-of-week date)]
    (or (= dow :saturday) (= dow :sunday))))

(s/fdef weekend?
  :args (s/cat :date ::date)
  :ret boolean?)

(defn weekday?
  "Returns true if `date` falls on Monday through Friday."
  [date]
  (not (weekend? date)))

(s/fdef weekday?
  :args (s/cat :date ::date)
  :ret boolean?)

(defn first-day-of-month?
  "Returns true if `date` is the first day of its month."
  [date]
  (m/one? (::day-of-month (date->breakdown date #{}))))

(s/fdef first-day-of-month?
  :args (s/cat :date ::date)
  :ret boolean?)

(defn last-day-of-month?
  "Returns true if `date` is the last day of its month."
  [date]
  (first-day-of-month? (+ date ticks-per-day)))

(s/fdef last-day-of-month?
  :args (s/cat :date ::date)
  :ret boolean?)

(defn same-day?
  "Returns true if both dates fall on the same calendar day. Ignores time-of-day components."
  [[start-date end-date]]
  (let [start-breakdown (dissoc (date->breakdown start-date #{}) ::ticks)
        end-breakdown (dissoc (date->breakdown end-date #{}) ::ticks)]
    (= start-breakdown end-breakdown)))

(s/fdef same-day?
  :args (s/cat :date-range ::date-range)
  :ret boolean?)

;;;HOLIDAY CALENDAR & BUSINESS DAYS
(defn business-day?
  "Returns true if `date` is a business day (weekday and not a holiday).

  Optional `holiday-set` is a set of dates to treat as holidays."
  ([date] (weekday? date))
  ([date holiday-set]
   (and (weekday? date)
     (not (contains? holiday-set (start-of-day date))))))

(s/fdef business-day?
  :args (s/cat :date ::date
          :holiday-set (s/? ::holiday-set))
  :ret boolean?)

(defn add-business-days
  "Adds `n` business days to `date`.

  Skips weekends and optionally holidays. Negative `n` subtracts days.

  Example:
    (add-business-days date-2020 5)        ; 5 business days later
    (add-business-days date-2020 -3 #{})   ; 3 business days earlier"
  ([date n] (add-business-days date n #{}))
  ([date n holiday-set]
   (if (zero? n)
     date
     (let [direction (if (pos? n) 1 -1)
           step (* direction ticks-per-day)]
       (loop [current date
              remaining (m/abs n)]
         (if (zero? remaining)
           current
           (let [next-date (+ current step)]
             (if (business-day? next-date holiday-set)
               (recur next-date (dec remaining))
               (recur next-date remaining)))))))))

(s/fdef add-business-days
  :args (s/cat :date ::date
          :n ::m/long
          :holiday-set (s/? ::holiday-set))
  :ret ::date)

(defn business-days-between
  "Returns the count of business days in `date-range` (exclusive of end).

  Excludes weekends and optionally holidays.

  Example:
    (business-days-between [start-date end-date])
    (business-days-between [start-date end-date] holiday-set)"
  ([[start-date end-date]] (business-days-between [start-date end-date] #{}))
  ([[start-date end-date] holiday-set]
   (let [start (start-of-day start-date)
         end (start-of-day end-date)
         [start end sign] (if (<= start end)
                            [start end 1]
                            [end start -1])]
     (* sign
       (loop [current start
              count 0]
         (if (>= current end)
           count
           (recur (+ current ticks-per-day)
             (if (business-day? current holiday-set)
               (inc count)
               count))))))))

(s/fdef business-days-between
  :args (s/cat :date-range ::date-range
          :holiday-set (s/? ::holiday-set))
  :ret ::m/long)

;;;FISCAL YEAR
(s/def ::fiscal-year-start-month ::month)

(defn fiscal-year
  "Returns the fiscal year number for `date`.

  `fiscal-year-start-month` is the month the fiscal year starts (1-12).
  Default is 1 (calendar year).

  Example:
    (fiscal-year date 10)  ; October fiscal year start
    ; A date in Nov 2020 returns fiscal year 2021"
  ([date] (fiscal-year date 1))
  ([date fiscal-year-start-month]
   (let [{::keys [year month]} (date->breakdown date #{})]
     (if (>= month fiscal-year-start-month)
       (if (= fiscal-year-start-month 1)
         year
         (inc year))
       year))))

(s/fdef fiscal-year
  :args (s/cat :date ::date
          :fiscal-year-start-month (s/? ::fiscal-year-start-month))
  :ret ::m/int)

(defn start-of-fiscal-year
  "Returns the first moment of the fiscal year containing `date`.

  `fiscal-year-start-month` is the month the fiscal year starts (1-12).
  Default is 1 (calendar year).

  Example:
    (start-of-fiscal-year some-date 7)  ; July fiscal year"
  ([date] (start-of-fiscal-year date 1))
  ([date fiscal-year-start-month]
   (let [{::keys [year month]} (date->breakdown date #{})
         fy-year (if (>= month fiscal-year-start-month)
                   year
                   (dec year))]
     (breakdown->date {::year         fy-year
                       ::month        fiscal-year-start-month
                       ::day-of-month 1}))))

(s/fdef start-of-fiscal-year
  :args (s/cat :date ::date
          :fiscal-year-start-month (s/? ::fiscal-year-start-month))
  :ret ::date)

(defn end-of-fiscal-year
  "Returns the first moment of the next fiscal year after `date`.

  `fiscal-year-start-month` is the month the fiscal year starts (1-12).
  Default is 1 (calendar year).

  Example:
    (end-of-fiscal-year some-date 7)  ; July fiscal year"
  ([date] (end-of-fiscal-year date 1))
  ([date fiscal-year-start-month]
   (let [{::keys [year month]} (date->breakdown date #{})
         fy-year (if (>= month fiscal-year-start-month)
                   (inc year)
                   year)]
     (breakdown->date {::year         fy-year
                       ::month        fiscal-year-start-month
                       ::day-of-month 1}))))

(s/fdef end-of-fiscal-year
  :args (s/cat :date ::date
          :fiscal-year-start-month (s/? ::fiscal-year-start-month))
  :ret ::date)

;;;DATE SEQUENCES
(defn date-seq
  "Returns a lazy sequence of dates starting from `start-date`.

  Options map:
  - :step-unit - one of :day, :week, :month, :year (default :day)
  - :step-amount - number of units per step (default 1)
  - :end-date - optional end date (exclusive)

  The sequence terminates if it reaches invalid dates (outside the
  supported range 1814-2325).

  Examples:
    (date-seq date-2020)                              ; daily from 2020
    (date-seq date-2020 {:step-unit :week})           ; weekly
    (date-seq date-2020 {:step-unit :month :step-amount 3})  ; quarterly
    (take 10 (date-seq date-2020 {:end-date date-2021}))     ; bounded"
  ([start-date] (date-seq start-date {}))
  ([start-date {:keys [step-unit step-amount end-date]
                :or   {step-unit :day step-amount 1}}]
   (let [step-ticks-day (* step-amount ticks-per-day)
         step-ticks-week (* step-amount ticks-per-week)
         advance (case step-unit
                   :day (fn [d]
                          (let [result (+' d step-ticks-day)]
                            (if (intervals/in-interval? [m/min-long m/max-long] result)
                              (long result)
                              {::anomalies/category ::anomalies/exception
                               ::anomalies/message  "date overflow"})))
                   :week (fn [d]
                           (let [result (+' d step-ticks-week)]
                             (if (intervals/in-interval? [m/min-long m/max-long] result)
                               (long result)
                               {::anomalies/category ::anomalies/exception
                                ::anomalies/message  "date overflow"})))
                   :month (fn [d] (add-months-to-date d step-amount))
                   :year (fn [d] (add-months-to-date d (* step-amount 12))))
         valid-date? (fn [d] (and (m/long? d)
                               (not (anomalies/anomaly? d))))]
     (cond->> (take-while valid-date? (iterate advance start-date))
       end-date (take-while #(< % end-date))))))

(s/fdef date-seq
  :args (s/cat :start-date ::date
          :opts (s/? (s/keys :opt-un [::step-unit ::step-amount ::end-date])))
  :ret (s/coll-of ::date))

;;;RANGE PREDICATES
(defn date-in-range?
  "Returns true if `date` is within `date-range` [start end).

  Uses half-open interval: includes start, excludes end.

  Example:
    (date-in-range? some-date [start-date end-date])"
  [date [start-date end-date]]
  (and (>= date start-date) (< date end-date)))

(s/fdef date-in-range?
  :args (s/cat :date ::date
          :date-range ::date-range)
  :ret boolean?)

(defn ranges-overlap?
  "Returns true if two date ranges overlap.

  Uses half-open intervals: [start1 end1) and [start2 end2).

  Example:
    (ranges-overlap? [start1 end1] [start2 end2])"
  [[start1 end1] [start2 end2]]
  (and (< start1 end2) (< start2 end1)))

(s/fdef ranges-overlap?
  :args (s/cat :range1 ::date-range
          :range2 ::date-range)
  :ret boolean?)

(defn range-intersection
  "Returns the intersection of two date ranges, or `nil` if no overlap.

  Example:
    (range-intersection [start1 end1] [start2 end2])
    ; => [max-start min-end] or nil"
  [[start1 end1] [start2 end2]]
  (when (ranges-overlap? [start1 end1] [start2 end2])
    [(max start1 start2) (min end1 end2)]))

(s/fdef range-intersection
  :args (s/cat :range1 ::date-range
          :range2 ::date-range)
  :ret (s/nilable ::date-range))

(defn range-contains?
  "Returns true if `outer-range` fully contains `inner-range`.

  Example:
    (range-contains? [outer-start outer-end] [inner-start inner-end])"
  [[outer-start outer-end] [inner-start inner-end]]
  (and (<= outer-start inner-start) (>= outer-end inner-end)))

(s/fdef range-contains?
  :args (s/cat :outer-range ::date-range
          :inner-range ::date-range)
  :ret boolean?)

;;;NAMED PERIODS
(def named-periods
  "Set of supported named period keywords."
  #{:ytd :mtd :qtd :wtd :last-7-days :last-30-days :last-90-days
    :last-week :last-month :last-quarter :last-year
    :trailing-12-months :trailing-3-months :trailing-6-months})

(s/def ::named-period named-periods)

(defn period->date-range
  "Converts a named period to a date range [start end) relative to `reference-date`.

  Supported periods:
  - :ytd - year to date
  - :mtd - month to date
  - :qtd - quarter to date
  - :wtd - week to date
  - :last-7-days, :last-30-days, :last-90-days
  - :last-week, :last-month, :last-quarter, :last-year
  - :trailing-12-months, :trailing-6-months, :trailing-3-months

  Example:
    (period->date-range :ytd reference-date)
    (period->date-range :last-quarter reference-date)"
  [period reference-date]
  (case period
    :ytd [(start-of-year reference-date) reference-date]
    :mtd [(start-of-month reference-date) reference-date]
    :qtd [(start-of-quarter reference-date) reference-date]
    :wtd [(start-of-week reference-date) reference-date]
    :last-7-days [(- reference-date (* 7 ticks-per-day)) reference-date]
    :last-30-days [(- reference-date (* 30 ticks-per-day)) reference-date]
    :last-90-days [(- reference-date (* 90 ticks-per-day)) reference-date]
    :last-week (let [sow (start-of-week reference-date)]
                 [(- sow ticks-per-week) sow])
    :last-month (let [som (start-of-month reference-date)]
                  [(add-months-to-date som -1) som])
    :last-quarter (let [soq (start-of-quarter reference-date)]
                    [(add-months-to-date soq -3) soq])
    :last-year (let [soy (start-of-year reference-date)]
                 [(add-months-to-date soy -12) soy])
    :trailing-12-months [(add-months-to-date reference-date -12) reference-date]
    :trailing-6-months [(add-months-to-date reference-date -6) reference-date]
    :trailing-3-months [(add-months-to-date reference-date -3) reference-date]))

(s/fdef period->date-range
  :args (s/cat :period ::named-period
          :reference-date ::date)
  :ret ::date-range)

(defn same-period-previous-year
  "Returns the date range for the same named period in the previous year.

  Useful for year-over-year comparisons.

  Example:
    (same-period-previous-year :last-month reference-date)"
  [period reference-date]
  (let [prev-year-ref (add-months-to-date reference-date -12)]
    (period->date-range period prev-year-ref)))

(s/fdef same-period-previous-year
  :args (s/cat :period ::named-period
          :reference-date ::date)
  :ret ::date-range)

;;;ISO WEEK DATES & ORDINAL DATES
(defn ordinal-day
  "Returns the day of the year (1-366) for `date`.

  January 1 = 1, December 31 = 365 or 366.

  Example:
    (ordinal-day some-date)  ; => 45 (Feb 14)"
  [date]
  (let [{::keys [year month day-of-month]} (date->breakdown date #{})
        days-before-month (instant/days-until-month [year month])]
    (+ days-before-month day-of-month)))

(s/fdef ordinal-day
  :args (s/cat :date ::date)
  :ret (s/int-in 1 367))

(defn- thursday-of-week
  "Returns the Thursday of the ISO week containing `date`.
  In ISO 8601, weeks run Monday(1) through Sunday(7)."
  [date]
  (let [dow (day-of-week date)
        ;; Convert to ISO day number: Monday=1, Tuesday=2, ..., Sunday=7
        iso-dow (case dow
                  :monday 1
                  :tuesday 2
                  :wednesday 3
                  :thursday 4
                  :friday 5
                  :saturday 6
                  :sunday 7)
        ;; Thursday is ISO day 4
        days-to-thursday (- 4 iso-dow)]
    (+ date (* days-to-thursday ticks-per-day))))

(defn iso-week-year
  "Returns the ISO week-numbering year for `date`. The ISO week year may differ from the calendar year for dates near
  year boundaries. Week 1 is the week containing January 4.

  Example:
    (iso-week-year date)  ; => 2020"
  [date]
  (let [thursday (thursday-of-week date)]
    (::year (date->breakdown thursday #{}))))

(s/fdef iso-week-year
  :args (s/cat :date ::date)
  :ret ::m/int)

(defn iso-week-number
  "Returns the ISO week number (1-53) for `date`. Week 1 is the week containing January 4 (first week with 4+ days in
  new year). Weeks start on Monday per ISO 8601.

  Example:
    (iso-week-number date)  ; => 15"
  [date]
  (let [thursday (thursday-of-week date)
        {::keys [year]} (date->breakdown thursday #{})
        jan4 (breakdown->date {::year year ::month 1 ::day-of-month 4})
        jan4-thursday (thursday-of-week jan4)
        week1-start (- jan4-thursday (* 3 ticks-per-day))   ; Monday of week 1
        days-since-week1 (quot (- thursday week1-start) ticks-per-day)]
    (int (inc (quot days-since-week1 7)))))

(s/fdef iso-week-number
  :args (s/cat :date ::date)
  :ret (s/int-in 1 54))

(defn format-iso-week-date
  "Formats `date` as an ISO week date string.

  Format: YYYY-Www-D (e.g., '2020-W15-3' for Wednesday of week 15)

  Example:
    (format-iso-week-date date)  ; => \"2020-W15-3\""
  [date]
  (let [year (int (iso-week-year date))
        week (int (iso-week-number date))
        dow (day-of-week date)
        ;; ISO day of week: Monday=1, Sunday=7
        dow-index (day-of-week->index dow)
        iso-dow (if (zero? dow-index) 7 dow-index)]
    (format "%d-W%02d-%d" year week iso-dow)))

(s/fdef format-iso-week-date
  :args (s/cat :date ::date)
  :ret string?)

(defn format-ordinal-date
  "Formats `date` as an ordinal date string.

  Format: YYYY-DDD (e.g., '2020-045' for February 14)

  Example:
    (format-ordinal-date date)  ; => \"2020-045\""
  [date]
  (let [{::keys [year]} (date->breakdown date #{})
        day (ordinal-day date)]
    (format "%d-%03d" year day)))

(s/fdef format-ordinal-date
  :args (s/cat :date ::date)
  :ret string?)

;;;RELATIVE FORMATTING
(defn- pluralize
  [n word]
  (let [n (long n)]
    (if (= n 1)
      (str n " " word)
      (str n " " word "s"))))

(defn format-relative
  "Formats the duration between `date` and `reference-date` as relative text.

  Examples:
    \"3 days ago\"
    \"in 2 weeks\"
    \"yesterday\"
    \"tomorrow\"
    \"just now\"

  Optional `reference-date` defaults to current time."
  ([date] (format-relative date (date$)))
  ([date reference-date]
   (let [diff (- date reference-date)
         abs-diff (m/abs diff)
         future? (pos? diff)
         ;; Calculate units
         minutes (long (quot abs-diff ticks-per-minute))
         hours (long (quot abs-diff ticks-per-hour))
         days (long (quot abs-diff ticks-per-day))
         weeks (long (quot abs-diff ticks-per-week))
         months (long (m/abs (first (date-range->duration-raw
                                      (if future?
                                        [reference-date date]
                                        [date reference-date])))))
         years (long (quot months 12))
         ;; Format based on magnitude
         text (cond
                (< abs-diff ticks-per-minute) "just now"
                (< abs-diff ticks-per-hour) (pluralize minutes "minute")
                (< abs-diff ticks-per-day) (pluralize hours "hour")
                (and (= days 1) (not future?)) "yesterday"
                (and (= days 1) future?) "tomorrow"
                (< abs-diff (* 7 ticks-per-day)) (pluralize days "day")
                (< abs-diff (* 30 ticks-per-day)) (pluralize weeks "week")
                (< months 12) (pluralize months "month")
                :else (pluralize years "year"))]
     (cond
       (= text "just now") text
       (= text "yesterday") text
       (= text "tomorrow") text
       future? (str "in " text)
       :else (str text " ago")))))

(s/fdef format-relative
  :args (s/cat :date ::date
          :reference-date (s/? ::date))
  :ret string?)
