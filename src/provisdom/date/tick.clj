(ns provisdom.date.tick
  "High-precision date and duration handling with tick-based arithmetic.

  Core Concepts:
  - 'ticks' are the fundamental time unit, providing sub-microsecond precision
  - 'date' represents ticks elapsed since epoch 2070, chosen to center the practical
    range (1814-2325) 100 years beyond the Unix epoch (1970)
  - Dates and ticks combine through simple arithmetic to create new dates
  - 'months' form a separate unit since month lengths vary
  - All units decompose into structured maps for flexible manipulation

  Common Operations:
  Most date calculations use basic arithmetic. For example:
    - Minutes from month start:
        (/ (- date (start-of-month date)) ticks-per-minute)
    - Minutes until month end:
        (/ (- (add-months-to-date (start-of-month date) 1) date) ticks-per-minute)

  Precision Design:
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

(declare breakdown->ticks breakdown->months ticks->java-duration)

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
(s/def ::months ::m/long)
(s/def ::weeks ::m/long)
(s/def ::days ::m/long)
(s/def ::hours ::m/long)
(s/def ::minutes ::m/long)
(s/def ::seconds ::m/long)
(s/def ::ms ::m/long)
(s/def ::us ::m/long)
(s/def ::duration (s/tuple ::months ::ticks))
(s/def ::date-range (s/tuple ::date ::date))
(s/def ::date-interval ::intervals/long-interval)
(s/def ::strict-date-interval (intervals/strict-interval-spec ::m/date))
;;formatting fractions of a second
(s/def ::seconds-fraction-precision (s/int-in 0 16))

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

(s/def ::day-of-week (set days-of-week))

;;;JAVA DURATION
(defn ticks->java-duration
  "Converts `ticks` to Java Duration, rounded to nearest nanosecond.
  
  Note: 1.144 ticks = 1 nanosecond."
  [ticks]
  (Duration/ofNanos (m/round (* 1000 (/ ticks ticks-per-us)) :up)))

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
    (m/round (intervals/bound-by-interval [m/min-long m/max-long] ticks) :up)))

(s/fdef java-duration->ticks-by-bounding
  :args (s/cat :java-duration ::java-duration)
  :ret ::ticks)

;;;INSTANT-MS
(defn date->instant-ms
  "Converts tick `date` to milliseconds since Unix epoch.
  
  Precision is limited to milliseconds."
  [date]
  (m/round (- (/ date ticks-per-ms) (/ date-1970 ticks-per-ms)) :up))

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
  "Bounds Java Date to supported instant range (1814-2325).
  
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
                              (m/quot-and-mod'
                                (* seconds ticks-per-second)
                                ticks-per-second)
                              [0 0])
            ticks (m/round ticks :up)
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
  
  Returns a map with keys like ::weeks, ::days, ::hours, ::minutes,
  ::seconds, ::ms, ::us, and ::ticks.
  
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
  "Converts a `ticks-breakdown` map back to total ticks.
  
  Accepts a map with time unit keys and returns the total ticks.
  Returns an anomaly if the result exceeds long range.
  
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
  
  Format: W<weeks>D<days>T<HH>:<MM>:<SS>.<ms>.<us>:<ticks>
  
  Optional `seconds-fraction-precision` shows seconds as decimal.
  
  Example:
    (format-ticks 123456789)
    ; => \"W0D1T10:23:45.123.456:789\"

    (format-ticks 123456789 4)
    ; => \"W0D1T10:23:45.1235\"
    "
  ([ticks]
   (let [f2 (partial format "%02d")
         f3 (partial format "%03d")
         {::keys [weeks days hours minutes seconds ms us ticks]
          :or    {weeks 0, days 0, hours 0, minutes 0, seconds 0,
                  ms    0, us 0, ticks 0}} (ticks->breakdown ticks)]
     (str "W" weeks "D" days "T" (f2 hours) ":" (f2 minutes) ":"
       (f2 seconds) "." (f3 ms) "." (f3 us) ":" ticks)))
  ([ticks seconds-fraction-precision]
   (let [f2 (partial format "%02d")
         {::keys [weeks days hours minutes seconds ms us ticks]
          :or    {weeks 0, days 0, hours 0, minutes 0, seconds 0,
                  ms    0, us 0, ticks 0}
          :as    breakdown} (ticks->breakdown ticks)
         ticks (breakdown->ticks
                 (dissoc breakdown ::weeks ::days ::hours ::minutes ::seconds))
         seconds-fraction (double (/ ticks ticks-per-second))]
     (str "W" weeks "D" days "T" (f2 hours) ":" (f2 minutes) ":"
       (format (str "%0" (+ 3 seconds-fraction-precision)
                 "." seconds-fraction-precision "f")
         (+ seconds seconds-fraction))))))

(s/fdef format-ticks
  :args (s/cat :ticks ::ticks
          :seconds-fraction-precision (s/? ::seconds-fraction-precision))
  :ret string?)

(defn- read-number
  [s anomaly else]
  (if (and s (not= "" s))
    (try (let [s2 (strings/trim-start s "0")
               s2 (if (= s2 "") "0" s2)]
           (read-string s2))
      (catch Exception _ anomaly))
    else))

(defn parse-ticks
  "Parses a `ticks-string` into ticks.
  
  Accepts format: W<weeks>D<days>T<HH>:<MM>:<SS>.<ms>.<us>:<ticks>
  
  Returns ticks as a long or an anomaly if parsing fails.
  
  Example:
    (parse-ticks \"W1D2T03:45:30.123.456:789\")"
  [ticks-string]
  (let [s ticks-string
        w? (str/includes? s "W")
        d? (str/includes? s "D")
        s (if-not d? (str "D" s) s)
        s (str/split s #"W|D|T")
        s (if w? (rest s) s)
        [s1 s2 s3] s
        anomaly {::anomalies/category ::anomalies/exception
                 ::anomalies/message  "bad ticks-string"
                 ::anomalies/fn       (var parse-ticks)}
        weeks (if w? (read-number s1 anomaly 0) 0)
        days (if d? (read-number s2 anomaly 0) 0)
        ticks (when s3 (parse-time s3))
        not-all-longs? (some false? (map m/long? [weeks days ticks]))]
    (if not-all-longs?
      anomaly
      (breakdown->ticks
        {::weeks weeks
         ::days  days
         ::ticks ticks}))))

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
  "Converts a `months-breakdown` back to total months.
  
  Returns total months or an anomaly if out of range."
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
  
  Returns a map with ::year, ::month, ::day-of-month and optionally
  time units like ::hours, ::minutes, etc.
  
  Optional `date-form` set specifies which components to include.
  Use empty set #{} for just date components.
  
  Example:
    (date->breakdown date-2020)
    ; => {::year 2020 ::month 1 ::day-of-month 1}"
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
                                (recur (+ day (instant/days-in-month
                                                [year
                                                 new-mo]))
                                  new-mo
                                  (if (m/one? month) (dec year) year)))
                              [day month year]))
         ticks-bd (ticks->breakdown
                    ticks
                    (set/difference
                      date-form
                      #{::year ::month ::day-of-month ::weeks ::days}))]
     (merge {::year         year
             ::month        month
             ::day-of-month day}
       ticks-bd))))

(s/fdef date->breakdown
  :args (s/cat :date ::date
          :date-form (s/? ::date-form))
  :ret ::date-breakdown)

(defn breakdown->date
  "Converts a `date-breakdown` map to tick date.
  
  Accepts a map with ::year, ::month, ::day-of-month and optional
  time components. Returns ticks from epoch (2070).
  
  Date must be between 1814-07-08 and 2325-06-28.
  
  Example:
    (breakdown->date {::year 2020 ::month 1 ::day-of-month 1})"
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
  "Converts Java Date to tick date, bounded to supported range.
  
  Clamps `java-date` outside 1814-2325 to the boundaries."
  [java-date]
  (instant->date (java-date->instant-by-bounding java-date)))

(s/fdef java-date->date-by-bounding
  :args (s/cat :java-date ::instant/java-date)
  :ret ::date)

(defn date-breakdown?
  "Returns true if `x` is a valid date breakdown.
  
  Checks that the breakdown has valid date components and
  is within the supported date range."
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
          :or    {hours 0, minutes 0, seconds 0, ms 0, us 0,
                  ticks 0}} (date->breakdown date)]
     (str (f2 year) "-" (f2 month) "-" (f2 day-of-month) "T" (f2 hours) ":"
       (f2 minutes) ":" (f2 seconds) "." (f3 ms) "." (f3 us) ":" ticks)))
  ([date seconds-fraction-precision]
   (let [f2 (partial format "%02d")
         {::keys [year month day-of-month hours minutes seconds ms us ticks]
          :or    {hours 0, minutes 0, seconds 0, ms 0, us 0, ticks 0}
          :as    breakdown} (date->breakdown date)
         ticks (breakdown->ticks
                 (dissoc breakdown ::weeks ::days ::hours ::minutes ::seconds))
         seconds-fraction (double (/ ticks ticks-per-second))]
     (str (f2 year) "-" (f2 month) "-" (f2 day-of-month) "T" (f2 hours) ":"
       (f2 minutes) ":" (format (str "%0" (+ 3 seconds-fraction-precision)
                                  "." seconds-fraction-precision "f")
                          (+ seconds seconds-fraction))))))

(s/fdef format-date
  :args (s/cat :date ::date
          :seconds-fraction-precision (s/? ::seconds-fraction-precision))
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
                 ::anomalies/message  "bad ticks-string"
                 ::anomalies/fn       (var parse-ticks)}
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
  "Adds `months` to a tick `date`.
  
  Preserves day-of-month when possible. Returns anomaly if
  the resulting date is invalid (e.g., Feb 30).
  
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
  
  Returns one of: :monday, :tuesday, :wednesday, :thursday,
  :friday, :saturday, :sunday.
  
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
  (let [date-breakdown (assoc (dissoc (date->breakdown date #{}) ::ticks)
                         ::day-of-month 1)]
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
        [month day-of-month] (if (= day-of-month
                                   (instant/days-in-month [year month]))
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

(defn ticks-in-month
  "Returns the number of ticks in the month containing `date`.
  
  Accounts for varying month lengths and leap years."
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
        months (+ (* 12 (- end-year start-year))
                 (- end-month start-month))
        ticks (+ (* ticks-per-day (- end-days start-days))
                (- (or end-ticks 0) (or start-ticks 0)))]
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
  "Converts date range to prorated (fractional) months.
  
  Accounts for partial months at start and end of the range.
  
  Example:
    (date-range->prorated-months [start end])
    ; => 6.23 ; 6.23 months"
  [[start-date end-date]]
  (let [end-of-start-month (end-of-month start-date)
        start-of-end-month (start-of-month end-date)
        whole-months (dec (first (date-range->duration
                                   [end-of-start-month start-of-end-month])))
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

  Format: Y<years>M<months>W<weeks>D<days>T<HH>:<MM>:<SS>.<ms>.<us>:<ticks>

  Optional `seconds-fraction-precision` shows seconds as decimal.

  Example:
    (format-duration [3 123456789])
    ; => \"Y0M3W0D1T10:23:45.123.456:789\"

    (format-duration [15 123456789] 4)
    ; => \"Y1M3W0D1T10:23:45.1235\""
  ([duration]
   (let [[months ticks] duration
         {::keys [months years]} (months->breakdown months)]
     (str "Y" years "M" months (format-ticks ticks))))
  ([duration seconds-fraction-precision]
   (let [[months ticks] duration
         {::keys [months years]} (months->breakdown months)]
     (str "Y" years "M" months (format-ticks ticks seconds-fraction-precision)))))

(s/fdef format-duration
  :args (s/cat :duration ::duration
          :seconds-fraction-precision (s/? ::seconds-fraction-precision))
  :ret string?)

;;;YEARLY PERIODS
(defn ticks->yearly-periods
  "Converts `ticks` to period in average years.
  
  Uses average year length of 365.2425 days."
  [ticks]
  (/ ticks (double ticks-per-average-year)))

(s/fdef ticks->yearly-periods
  :args (s/cat :ticks ::ticks)
  :ret ::instant/yearly-periods)

(defn date-range->yearly-periods
  "Converts `date-range` to period in average years."
  [[start-date end-date]]
  (/ (- end-date (double start-date)) ticks-per-average-year))

(s/fdef date-range->yearly-periods
  :args (s/cat :date-range ::date-range)
  :ret ::instant/yearly-periods)

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
  "Returns true if both dates fall on the same calendar day.
  
  Ignores time-of-day components."
  [[start-date end-date]]
  (let [start-breakdown (dissoc (date->breakdown start-date #{}) ::ticks)
        end-breakdown (dissoc (date->breakdown end-date #{}) ::ticks)]
    (= start-breakdown end-breakdown)))

(s/fdef same-day?
  :args (s/cat :date-range ::date-range)
  :ret boolean?)
