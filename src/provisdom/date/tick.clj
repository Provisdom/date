(ns provisdom.date.tick
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [clojure.string :as str]
    [clojure.set :as set]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.strings :as strings]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals]
    [provisdom.date.instant :as instant])
  (:import (java.util Date)))

;;;;This namespace was created to handle dates and durations in an easy and
;;;; intuitive manner.
;;;;
;;;; 'ticks' are the smallest unit of time.
;;;;
;;;; 'date' is the number of ticks from 2070, which was chosen to be centered
;;;; around a practical range (1814-2325) and is 100 years in the future from
;;;; 1970, the unix-epoch.
;;;;
;;;; Dates and ticks can be easily added to form new dates.
;;;;
;;;; Months are the other basic unit, as the number of ticks per month can vary.
;;;;
;;;; Dates, ticks, and months can be broken down into maps of various unit
;;;; types.
;;;;
;;;; There hasn't been much need for helper functions beyond the basics.
;;;; For example, to get minutes from start of month of `date`:
;;;;  (/ (- date (start-of-month date)) ticks-per-minute)
;;;;
;;;;  and to get minutes until end of month of `date`:
;;;;  (/ (- (add-months-to-date (start-of-month date) 1) date) ticks-per-minute)
;;;;
;;;; The tick size was chosen such that it is very likely that models that
;;;; partition parts of a time period will not lose accuracy. More specifically,
;;;; ticks were chosen to have 400 years be divisible by microseconds, and is
;;;; divisible by 2^12 and all numbers through 16. There is a
;;;; leap-day every 4 years excluding years divisible by 100, plus years
;;;; divisible by 400:
;;;;   400 years = 480 months = 20,871 weeks = 146,097 days
;;;; and
;;;;   146097*24*60*60*1000000*11*13*8=ticks in 400 years

(declare breakdown->ticks breakdown->months)

(def ^:const date-1970 -3610189440000000000)
(def ^:const date-2020 -1805144140800000000)
(def ^:const date-2045 -902522649600000000)
(def ^:const date-2070 0)
(def ^:const epoch "2070" 2070)
(def ^:const ticks-per-average-year "36101153088000000" 36101153088000000)
(def ^:const ticks-per-average-month "3008429424000000" 3008429424000000)
(def ^:const ticks-per-week "691891200000000" 691891200000000)
(def ^:const ticks-per-day "98841600000000" 98841600000000)
(def ^:const ticks-per-hour "4118400000000" 4118400000000)
(def ^:const ticks-per-minute "68640000000" 68640000000)
(def ^:const ticks-per-second "1144000000" 1144000000)
(def ^:const ticks-per-ms "millisecond: 1144000" 1144000)
(def ^:const ticks-per-us "microsecond: 1144" 1144)

(s/def ::ticks ::m/long)                                    ;;1/1144 of a microsecond
(s/def ::date ::m/long)                                     ;;ticks from epoch
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
(s/def ::date-interval ::intervals/long-interval)
(s/def ::seconds-fraction-precision (s/int-in 0 16))        ;;formatting fractions of a second

(s/def ::instant-ms (s/int-in -4906628144104 11218148144105))

(defn- instant-in-range?
  [instant]
  (intervals/in-interval? [-4906628144104 11218148144104]
                          (instant/instant->instant-ms instant)))

(s/def ::instant
  (s/with-gen (s/and inst? instant-in-range?)
              #(gen/fmap instant/instant-ms->instant (s/gen ::instant-ms))))

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

(defn- date-breakdown-ticks-less-than-a-day?
  [date-breakdown]
  (let [t (breakdown->ticks (dissoc date-breakdown ::weeks ::days))]
    (and (not (anomalies/anomaly? t))
         (intervals/in-interval? [0 (dec ticks-per-day)] t))))

(defn- date-breakdown-in-range?
  [{::keys [year month day-of-month]}]
  (cond (= year 1814)
        (or (> month 7)
            (and (= month 7) (>= day-of-month 8)))

        (= year 2325)
        (or (< month 6)
            (and (= month 6) (<= day-of-month 29)))

        :else
        true))

(defn- date-breakdown-valid-day-of-month?
  [{::keys [year month day-of-month]}]
  (let [dm (instant/days-in-month [year month])]
    (<= day-of-month dm)))

(s/def ::date-breakdown
  (s/and (s/keys :req [::year ::month ::day-of-month]
                 :opt [::hours ::minutes ::seconds ::ms ::us ::ticks])
         date-breakdown-ticks-less-than-a-day?
         date-breakdown-in-range?
         date-breakdown-valid-day-of-month?))

(def days-of-week
  [:sunday :monday :tuesday :wednesday :thursday :friday :saturday])

(s/def ::day-of-week (set days-of-week))

;;;INSTANT-MS
(defn date->instant-ms
  "Returns an ::instant-ms. Loses precision below millisecond."
  [date]
  (m/round (- (/ date ticks-per-ms) (/ date-1970 ticks-per-ms)) :up))

(s/fdef date->instant-ms
  :args (s/cat :date ::date)
  :ret ::instant-ms)

(defn instant-ms->date
  "Converts `instant-ms` to ::date."
  [instant-ms]
  (condp = instant-ms
    -4906628144104 m/min-long
    11218148144104 m/max-long
    (* ticks-per-ms (+ (/ date-1970 ticks-per-ms) instant-ms))))

(s/fdef instant-ms->date
  :args (s/cat :instant-ms ::instant-ms)
  :ret ::date)

;;;INSTANT
(defn date->instant
  "Returns an instant. Loses precision below millisecond."
  [date]
  (Date. ^long (date->instant-ms date)))

(s/fdef date->instant
  :args (s/cat :date ::date)
  :ret ::instant)

(defn instant->date
  "Converts an `instant` to a date."
  [instant]
  (instant-ms->date (instant/instant->instant-ms instant)))

(s/fdef instant->date
  :args (s/cat :instant ::instant)
  :ret ::date)

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
  "`ticks` can be broken down as a map of the keys ::weeks, ::days, ::hours,
  ::minutes, ::seconds, ::ms (milliseconds), ::us (microseconds), and ::ticks.
  Optionally, a `ticks-form` set can breakdown ticks as a subset of the
  keywords."
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
  "Returns ticks as a long. Ticks are 1/1144 of a us (microsecond)."
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
  "Formats `ticks` as a string. Optionally, use `seconds-fraction-precision` to
  get seconds as a fraction."
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
         (catch Exception e anomaly))
    else))

(defn parse-ticks
  "Creates ticks from `ticks-string`."
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
  "`months` can be broken down into a map of ::months and ::years."
  [months]
  (let [[years months] (m/quot-and-rem' months 12)]
    {::years  years
     ::months months}))

(s/fdef months->breakdown
  :args (s/cat :months ::months)
  :ret ::months-breakdown)

(defn breakdown->months
  "Returns ::months."
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
  "Now, returned to the nearest millisecond."
  []
  (instant->date (instant/instant$)))

(defn date->breakdown
  "A `date` can be broken down as a map of the keys ::year, ::month,
  ::day-of-month, ::hours, ::minutes, ::seconds, ::ms (milliseconds),
  ::us (microseconds), and ::ticks. Optionally, a `date-form` set can breakdown
  the date as a subset of the keywords. Common use case is to provide an empty
  set, which will only contain ::ticks (if necessary) and the required ::year,
  ::month, and ::day-of-month."
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
  "A date is a long in tick units representing the number of ticks starting from
  'epoch' (2070) in the UTC time zone. A date must be later than 7/8/1814 and
  earlier than 6/29/2325."
  [date-breakdown]
  (let [{::keys [year month day-of-month]} date-breakdown
        ticks (breakdown->ticks (dissoc date-breakdown ::weeks ::days))
        days (+' (instant/days-until-month [year month])    ;includes leap-days
                 (instant/passed-leap-days [epoch 1] [year 1]) ;don't want to double-count leap-days
                 (* 365 (+' year (- epoch)))
                 (dec day-of-month))
        date (+' (*' ticks-per-day days) ticks)]
    (long date)))

(s/fdef breakdown->date
  :args (s/cat :date-breakdown ::date-breakdown)
  :ret ::date)

(defn format-date
  "Formats `date` as a string. Optionally, use `seconds-fraction-precision` to
  get seconds as a fraction."
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
  "Creates ::date from `date-string`."
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
        not-all-longs? (some false?
                             (map m/long? [year month day-of-month ticks]))]
    (if not-all-longs?
      anomaly
      (if (and (intervals/in-interval? [1814 2325] year)
               (intervals/in-interval? [1 12] month)
               (intervals/in-interval? [1 31] day-of-month))
        (breakdown->date {::year         year
                          ::month        month
                          ::day-of-month day-of-month
                          ::ticks        ticks})
        anomaly))))

(s/fdef parse-date
  :args (s/cat :date-string string?)
  :ret (s/or :date ::date
             :anomaly ::anomalies/anomaly))

(defn add-months-to-date
  "Adds `months` to `date`."
  [date months]
  (let [{::keys [year month day-of-month]
         :as    date-breakdown} (date->breakdown date #{})
        [years months] (m/quot-and-mod' (dec (+ months month)) 12)
        year (+ year years)
        month (inc months)
        date-breakdown (assoc date-breakdown ::year year
                                             ::month month)]
    (if (and (intervals/in-interval? [1814 2325] year)
             (<= day-of-month (instant/days-in-month [year month])))
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
  "From a supplied `date`, returns the day of the week as a keyword :monday,
  :tuesday, :wednesday, :thursday, :friday, :saturday, :sunday."
  [date]
  (nth days-of-week (m/mod' (+ (m/quot' date ticks-per-day) 3) 7)))

(s/fdef day-of-week
  :args (s/cat :date ::date)
  :ret ::day-of-week)

(defn start-of-year
  "Returns start of year of `date`."
  [date]
  (breakdown->date
    (assoc (dissoc (date->breakdown date #{}) ::ticks)
      ::month 1
      ::day-of-month 1)))

(s/fdef start-of-year
  :args (s/cat :date ::date)
  :ret ::date)

(defn start-of-month
  "Returns start of month of `date`."
  [date]
  (breakdown->date
    (assoc (dissoc (date->breakdown date #{}) ::ticks)
      ::day-of-month 1)))

(s/fdef start-of-month
  :args (s/cat :date ::date)
  :ret ::date)

(defn start-of-day
  "Returns start of day of `date`."
  [date]
  (breakdown->date (dissoc (date->breakdown date #{}) ::ticks)))

(s/fdef start-of-day
  :args (s/cat :date ::date)
  :ret ::date)

;;;DATE INTERVALS
(defn date-interval->months-calendar
  "Returns the number of calendar months and remaining ticks from a
  date-interval."
  [[start-date end-date]]
  (let [{start-year  ::year
         start-month ::month} (date->breakdown start-date #{})
        {end-year  ::year
         end-month ::month} (date->breakdown end-date #{})
        months (+ (* 12 (- end-year start-year))
                  (- end-month start-month))
        new-start-date (add-months-to-date start-date months)]
    (if (anomalies/anomaly? new-start-date)
      new-start-date
      [months (- end-date new-start-date)])))

(s/fdef date-interval->months-calendar
  :args (s/cat :date-interval ::date-interval)
  :ret (s/or :duration ::duration
             :anomaly ::anomalies/anomaly))

(defn date-interval->months-floor
  "Returns the floor of months and remaining ticks from a date-interval."
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
    (if (neg? ticks)
      (let [new-start-date (add-months-to-date start-date (dec months))]
        (if (anomalies/anomaly? new-start-date)
          new-start-date
          [(dec months) (- end-date new-start-date)]))
      [months ticks])))

(s/fdef date-interval->months-floor
  :args (s/cat :date-interval ::date-interval)
  :ret (s/or :duration ::duration
             :anomaly ::anomalies/anomaly))

(defn date-interval->months-ceil
  "Returns the ceil of months and remaining ticks from a date-interval."
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
    (if (pos? ticks)
      (let [new-start-date (add-months-to-date start-date (inc months))]
        (if (anomalies/anomaly? new-start-date)
          new-start-date
          [(inc months) (- end-date new-start-date)]))
      [months ticks])))

(s/fdef date-interval->months-ceil
  :args (s/cat :date-interval ::date-interval)
  :ret (s/or :duration ::duration
             :anomaly ::anomalies/anomaly))

;;;PERIODS
(defn ticks->period
  "Returns ::instant/period (in average-years) from `ticks`."
  [ticks]
  (/ ticks (double ticks-per-average-year)))

(s/fdef ticks->period
  :args (s/cat :ticks ::ticks)
  :ret ::instant/period)

(defn date-interval->period
  "Returns period from `date-interval`."
  [date-interval]
  (/ (- (second date-interval) (double (first date-interval)))
     ticks-per-average-year))

(s/fdef date-interval->period
  :args (s/cat :date-interval ::date-interval)
  :ret (s/and ::instant/period m/non-?))

;;;PREDICATES
(defn weekend?
  "Returns whether a supplied `date` occurs on a Saturday or Sunday."
  [date]
  (let [dow (day-of-week date)]
    (or (= dow :saturday) (= dow :sunday))))

(s/fdef weekend?
  :args (s/cat :date ::date)
  :ret boolean?)

(defn weekday?
  "Returns whether a supplied `date` occurs on Monday through Friday."
  [date]
  (not (weekend? date)))

(s/fdef weekday?
  :args (s/cat :date ::date)
  :ret boolean?)

(defn first-day-of-month?
  "Returns whether a supplied `date` occurs on the first day of a month."
  [date]
  (m/one? (::day-of-month (date->breakdown date #{}))))

(s/fdef first-day-of-month?
  :args (s/cat :date ::date)
  :ret boolean?)

(defn last-day-of-month?
  "Returns whether a supplied `date` occurs on the last day of a month."
  [date]
  (first-day-of-month? (+ date ticks-per-day)))

(s/fdef last-day-of-month?
  :args (s/cat :date ::date)
  :ret boolean?)

(defn date-interval?
  "Returns true if `x` is a ::date-interval. "
  [x]
  (and (sequential? x)
       (= 2 (count x))
       (m/long? (first x))
       (m/long? (second x))
       (<= (first x) (second x))))

(s/fdef date-interval?
  :args (s/cat :x any?)
  :ret boolean?)

(defn strict-date-interval?
  "Returns true if `x` is a ::strict-date-interval."
  [x]
  (and (date-interval? x) (< (first x) (second x))))

(s/fdef strict-date-interval?
  :args (s/cat :x any?)
  :ret boolean?)

(defn same-day?
  "Returns true if `date1 `and `date2 `are on the same day."
  [date1 date2]
  (let [breakdown1 (dissoc (date->breakdown date1 #{}) ::ticks)
        breakdown2 (dissoc (date->breakdown date2 #{}) ::ticks)]
    (= breakdown1 breakdown2)))

(s/fdef same-day?
  :args (s/cat :date1 ::date
               :date2 ::date)
  :ret boolean?)
