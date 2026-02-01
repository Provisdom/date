(ns provisdom.date.timezone
  "Timezone conversions between internal UTC representation and user-facing local times.

  This namespace provides functionality to convert between the internal tick-based UTC
  representation (`::tick/date`) and local wall-clock times in specific timezones.

  Core Concepts:
  - All internal dates are in UTC
  - Timezones are specified as IANA timezone IDs (e.g., \"America/New_York\")
  - Local times are represented as breakdowns (maps with `::tick/year`, `::tick/month`, etc.)
  - DST transitions create ambiguous or invalid local times

  DST Edge Case Handling:
  - Spring-forward (gap): Use `::gap-resolution` option
    Values: `:pre-gap`, `:post-gap` (default), or `:error`
  - Fall-back (overlap): Use `::dst-resolution` option
    Values: `:earlier` (default), `:later`, or `:error`

  Common Patterns:
    ;; Convert UTC date to local breakdown
    (date->local-breakdown \"America/New_York\" some-utc-date)

    ;; Convert local time to UTC date
    (local-breakdown->date \"America/New_York\"
      {::tick/year 2024 ::tick/month 7 ::tick/day-of-month 4 ::tick/hours 12})

    ;; Format date for display in a timezone
    (date->local-string \"America/New_York\" some-utc-date)

    ;; Check if a date is in DST
    (dst? \"America/New_York\" some-utc-date)"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]
    [provisdom.date.instant :as instant]
    [provisdom.date.tick :as tick]
    [provisdom.math.core :as m]
    [provisdom.utility-belt.anomalies :as anomalies])
  (:import
    (java.time Instant LocalDateTime ZoneId ZonedDateTime ZoneOffset)
    (java.time.zone ZoneOffsetTransition ZoneRules)))

;;;SPECS
(def ^:private valid-zone-ids
  "Set of valid IANA timezone IDs."
  (set (ZoneId/getAvailableZoneIds)))

(s/def ::zone-id
  (s/with-gen
    (s/and string? #(contains? valid-zone-ids %))
    #(gen/elements (vec (take 50 (sort valid-zone-ids))))))

(s/def ::offset-ticks ::m/long)
(s/def ::offset-hours (s/int-in -18 19))
(s/def ::offset-minutes (s/int-in -59 60))
(s/def ::offset-seconds (s/int-in -59 60))

(s/def ::offset
  (s/keys :req [::offset-hours ::offset-minutes ::offset-ticks]
    :opt [::offset-seconds]))

;; Custom generator for local-breakdown that generates safe year ranges
;; to avoid overflow when converting to tick dates across timezones
(s/def ::local-breakdown
  (s/with-gen
    ::tick/core-date-breakdown
    #(gen/fmap
       (fn [[year month day hours minutes seconds ms us ticks]]
         {::tick/day-of-month (min day (instant/days-in-month [year month]))
          ::tick/hours        hours
          ::tick/minutes      minutes
          ::tick/month        month
          ::tick/ms           ms
          ::tick/seconds      seconds
          ::tick/ticks        ticks
          ::tick/us           us
          ::tick/year         year})
       (gen/tuple
         (gen/choose 1850 2300)     ; safe year range within tick bounds
         (gen/choose 1 12)          ; month
         (gen/choose 1 28)          ; day (safe for all months)
         (gen/choose 0 23)          ; hours
         (gen/choose 0 59)          ; minutes
         (gen/choose 0 59)          ; seconds
         (gen/choose 0 999)         ; ms
         (gen/choose 0 999)         ; us
         (gen/choose 0 1143)))))

(s/def ::dst-resolution #{:earlier :later :error})
(s/def ::gap-resolution #{:pre-gap :post-gap :error})

(s/def ::conversion-opts
  (s/keys :opt [::dst-resolution ::gap-resolution]))

(s/def ::transition-type #{:gap :overlap})
(s/def ::transition-date ::tick/date)
(s/def ::offset-before ::offset)
(s/def ::offset-after ::offset)

(s/def ::dst-transition
  (s/keys :req [::transition-date ::transition-type ::offset-before ::offset-after]))

;;;PRIVATE HELPERS
(defn- tick-date->java-instant
  "Converts a tick date to a Java Instant."
  [date]
  (Instant/ofEpochMilli (tick/date->instant-ms date)))

(defn- java-instant->tick-date
  "Converts a Java Instant to a tick date."
  [^Instant instant]
  (tick/instant-ms->date (.toEpochMilli instant)))

(defn- zone-id->java-zone
  "Converts a zone-id string to a Java ZoneId."
  ^ZoneId [zone-id]
  (ZoneId/of ^String zone-id))

(defn- local-breakdown->java-local-date-time
  "Converts a local breakdown to a Java LocalDateTime."
  ^LocalDateTime [local-breakdown]
  (let [{::tick/keys [year month day-of-month hours minutes seconds ms us ticks]
         :or         {hours 0, minutes 0, seconds 0, ms 0, us 0, ticks 0}} local-breakdown
        nanos (+ (* ms 1000000) (* us 1000) (long (/ ticks 1.144)))]
    (LocalDateTime/of ^int year ^int month ^int day-of-month
      ^int hours ^int minutes ^int seconds ^int nanos)))

(defn- java-zoned-date-time->local-breakdown
  "Converts a Java ZonedDateTime to a local breakdown."
  [^ZonedDateTime zdt date-form]
  (let [want-ticks? (or (empty? date-form) (contains? date-form ::tick/ticks))
        want-us? (or (empty? date-form) (contains? date-form ::tick/us))
        want-ms? (or (empty? date-form) (contains? date-form ::tick/ms))
        want-seconds? (or (empty? date-form) (contains? date-form ::tick/seconds))
        want-minutes? (or (empty? date-form) (contains? date-form ::tick/minutes))
        want-hours? (or (empty? date-form) (contains? date-form ::tick/hours))
        nanos (.getNano zdt)
        ms (quot nanos 1000000)
        us (quot (mod nanos 1000000) 1000)
        sub-us-nanos (mod nanos 1000)
        ticks (long (* sub-us-nanos 1.144))
        base {::tick/day-of-month (.getDayOfMonth zdt)
              ::tick/month        (.getMonthValue zdt)
              ::tick/year         (.getYear zdt)}]
    (cond-> base
      want-hours? (assoc ::tick/hours (.getHour zdt))
      want-minutes? (assoc ::tick/minutes (.getMinute zdt))
      want-seconds? (assoc ::tick/seconds (.getSecond zdt))
      want-ms? (assoc ::tick/ms ms)
      want-us? (assoc ::tick/us us)
      want-ticks? (assoc ::tick/ticks ticks))))

(defn- zone-offset->offset-map
  "Converts a Java ZoneOffset to an offset map."
  [^ZoneOffset offset]
  (let [total-seconds (.getTotalSeconds offset)
        hours (m/quot' total-seconds 3600)
        remaining (m/mod' (m/abs' total-seconds) 3600)
        mins (m/quot' remaining 60)
        secs (m/mod' remaining 60)
        ticks (* total-seconds tick/ticks-per-second)]
    {::offset-hours   hours
     ::offset-minutes (if (neg? total-seconds) (- mins) mins)
     ::offset-seconds (if (neg? total-seconds) (- secs) secs)
     ::offset-ticks   ticks}))

;;;ZONE UTILITIES
(defn valid-zone-id?
  "Returns true if `zone-id-string` is a valid IANA timezone ID."
  [zone-id-string]
  (and (string? zone-id-string)
    (contains? valid-zone-ids zone-id-string)))

(s/fdef valid-zone-id?
  :args (s/cat :zone-id-string any?)
  :ret boolean?)

(defn system-zone-id$
  "Returns the system's default timezone ID.

  Non-deterministic: depends on system configuration."
  []
  (.getId (ZoneId/systemDefault)))

(s/fdef system-zone-id$
  :args (s/cat)
  :ret ::zone-id)

(defn available-zone-ids
  "Returns a sorted vector of all available IANA timezone IDs."
  []
  (vec (sort valid-zone-ids)))

(s/fdef available-zone-ids
  :args (s/cat)
  :ret (s/coll-of ::zone-id :kind vector?))

(defn zone-offset-at-date
  "Returns the UTC offset for `zone-id` at the given `date`.

  Returns a map with `::offset-hours`, `::offset-minutes`, `::offset-seconds`, and `::offset-ticks`.

  Example:
    (zone-offset-at-date \"America/New_York\" tick/date-2020)
    ;; => {::offset-hours -5 ::offset-minutes 0 ::offset-seconds 0 ::offset-ticks ...}"
  [zone-id date]
  (let [zone (zone-id->java-zone zone-id)
        ^Instant instant (tick-date->java-instant date)
        ^ZoneRules rules (.getRules zone)
        offset (.getOffset rules instant)]
    (zone-offset->offset-map offset)))

(s/fdef zone-offset-at-date
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date)
  :ret ::offset)

(defn format-offset
  "Formats an offset map as a string like \"+05:30\" or \"-08:00\"."
  [offset]
  (let [{::keys [offset-hours offset-minutes]} offset
        sign (if (or (neg? offset-hours)
                   (and (zero? offset-hours) (neg? offset-minutes)))
               "-" "+")
        abs-hours (m/abs' offset-hours)
        abs-mins (m/abs' offset-minutes)]
    (format "%s%02d:%02d" sign abs-hours abs-mins)))

(s/fdef format-offset
  :args (s/cat :offset ::offset)
  :ret string?)

;;;DST DETECTION
(defn dst?
  "Returns true if `date` is in daylight saving time for `zone-id`."
  [zone-id date]
  (let [zone (zone-id->java-zone zone-id)
        instant (tick-date->java-instant date)
        rules (.getRules zone)]
    (.isDaylightSavings rules instant)))

(s/fdef dst?
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date)
  :ret boolean?)

(defn ambiguous-local-time?
  "Returns true if `local-breakdown` represents an ambiguous local time in `zone-id`.

  Ambiguous times occur during DST fall-back transitions when clocks are set back, creating an
  overlap where the same local time occurs twice."
  [zone-id local-breakdown]
  (try
    (let [zone (zone-id->java-zone zone-id)
          ldt (local-breakdown->java-local-date-time local-breakdown)
          rules (.getRules zone)
          valid-offsets (.getValidOffsets rules ldt)]
      (> (count valid-offsets) 1))
    (catch Exception _
      false)))

(s/fdef ambiguous-local-time?
  :args (s/cat :zone-id ::zone-id
          :local-breakdown ::local-breakdown)
  :ret boolean?)

(defn invalid-local-time?
  "Returns true if `local-breakdown` represents an invalid local time in `zone-id`.

  Invalid times occur during DST spring-forward transitions when clocks skip ahead, creating a gap
  where certain local times don't exist."
  [zone-id local-breakdown]
  (try
    (let [zone (zone-id->java-zone zone-id)
          ldt (local-breakdown->java-local-date-time local-breakdown)
          rules (.getRules zone)
          valid-offsets (.getValidOffsets rules ldt)]
      (zero? (count valid-offsets)))
    (catch Exception _
      false)))

(s/fdef invalid-local-time?
  :args (s/cat :zone-id ::zone-id
          :local-breakdown ::local-breakdown)
  :ret boolean?)

(defn next-dst-transition
  "Returns the next DST transition after `date` for `zone-id`, or nil if none.

  Returns a map with:
  - `::transition-date` - the tick date when the transition occurs
  - `::transition-type` - `:gap` (spring-forward) or `:overlap` (fall-back)
  - `::offset-before` - the offset before the transition
  - `::offset-after` - the offset after the transition"
  [zone-id date]
  (let [zone (zone-id->java-zone zone-id)
        instant (tick-date->java-instant date)
        rules (.getRules zone)
        transition (.nextTransition rules instant)]
    (when transition
      (let [transition-instant (.getInstant ^ZoneOffsetTransition transition)
            offset-before (.getOffsetBefore ^ZoneOffsetTransition transition)
            offset-after (.getOffsetAfter ^ZoneOffsetTransition transition)
            is-gap (.isGap ^ZoneOffsetTransition transition)]
        {::offset-after    (zone-offset->offset-map offset-after)
         ::offset-before   (zone-offset->offset-map offset-before)
         ::transition-date (java-instant->tick-date transition-instant)
         ::transition-type (if is-gap :gap :overlap)}))))

(s/fdef next-dst-transition
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date)
  :ret (s/nilable ::dst-transition))

;;;CORE CONVERSIONS
(defn date->local-breakdown
  "Converts a UTC tick `date` to a local time breakdown in `zone-id`.

  Optional `date-form` set specifies which time components to include.
  Use empty set `#{}` for just date components (year, month, day-of-month).

  Example:
    (date->local-breakdown \"America/New_York\" tick/date-2020)
    ;; => {::tick/year 2019 ::tick/month 12 ::tick/day-of-month 31 ::tick/hours 19 ...}"
  ([zone-id date] (date->local-breakdown zone-id date (set tick/date-breakdown-all)))
  ([zone-id date date-form]
   (let [zone (zone-id->java-zone zone-id)
         instant (tick-date->java-instant date)
         zdt (ZonedDateTime/ofInstant instant zone)]
     (java-zoned-date-time->local-breakdown zdt date-form))))

(s/fdef date->local-breakdown
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date
          :date-form (s/? ::tick/date-form))
  :ret ::local-breakdown)

(defn local-breakdown->date
  "Converts a local time `local-breakdown` in `zone-id` to a UTC tick date.

  Options map:
  - `::dst-resolution` - for ambiguous times (fall-back)
    Values: `:earlier` (default), `:later`, or `:error`
  - `::gap-resolution` - for invalid times (spring-forward)
    Values: `:pre-gap`, `:post-gap` (default), or `:error`

  Returns an anomaly if:
  - The local time is ambiguous and `::dst-resolution` is `:error`
  - The local time is invalid and `::gap-resolution` is `:error`
  - The resulting date is outside the valid tick date range

  Example:
    (local-breakdown->date \"America/New_York\"
      {::tick/year 2024 ::tick/month 7 ::tick/day-of-month 4 ::tick/hours 12})"
  ([zone-id local-breakdown]
   (local-breakdown->date zone-id local-breakdown {}))
  ([zone-id local-breakdown opts]
   (let [{::keys [dst-resolution gap-resolution]
          :or    {dst-resolution :earlier
                  gap-resolution :post-gap}} opts
         zone (zone-id->java-zone zone-id)
         ldt (local-breakdown->java-local-date-time local-breakdown)
         rules (.getRules zone)
         valid-offsets (.getValidOffsets rules ldt)]
     (cond
       ;; Invalid time (gap - spring forward)
       (zero? (count valid-offsets))
       (if (= gap-resolution :error)
         {::anomalies/category ::anomalies/exception
          ::anomalies/fn       (var local-breakdown->date)
          ::anomalies/message  "Invalid local time (gap during DST transition)"}
         (let [transition (.getTransition rules ldt)
               instant (if (= gap-resolution :pre-gap)
                         (.minusNanos (.getInstant ^ZoneOffsetTransition transition) 1)
                         (.getInstant ^ZoneOffsetTransition transition))]
           (java-instant->tick-date instant)))

       ;; Ambiguous time (overlap - fall back)
       (> (count valid-offsets) 1)
       (if (= dst-resolution :error)
         {::anomalies/category ::anomalies/exception
          ::anomalies/fn       (var local-breakdown->date)
          ::anomalies/message  "Ambiguous local time (overlap during DST transition)"}
         (let [offset (if (= dst-resolution :earlier)
                        (first valid-offsets)
                        (second valid-offsets))
               zdt (ZonedDateTime/of ldt ^ZoneOffset offset)
               instant (.toInstant zdt)]
           (java-instant->tick-date instant)))

       ;; Normal case - exactly one valid offset
       :else
       (let [zdt (ZonedDateTime/of ldt zone)
             instant (.toInstant zdt)]
         (java-instant->tick-date instant))))))

(s/fdef local-breakdown->date
  :args (s/cat :zone-id ::zone-id
          :local-breakdown ::local-breakdown
          :opts (s/? ::conversion-opts))
  :ret (s/or :date ::tick/date
         :anomaly ::anomalies/anomaly))

;;;STRING FORMATTING/PARSING
(defn date->local-string
  "Formats a UTC tick `date` as a local time string in `zone-id`.

  Optional `seconds-fraction-precision` controls decimal places for seconds (default: 6).

  Format: YYYY-MM-DDTHH:MM:SS.ffffff (ISO-like format)

  Example:
    (date->local-string \"America/New_York\" tick/date-2020)
    ;; => \"2019-12-31T19:00:00.000000\""
  ([zone-id date]
   (date->local-string zone-id date 6))
  ([zone-id date seconds-fraction-precision]
   (let [breakdown (date->local-breakdown zone-id date)
         {::tick/keys [year month day-of-month hours minutes seconds ms us ticks]
          :or         {hours 0, minutes 0, seconds 0, ms 0, us 0, ticks 0}} breakdown
         sub-second-ticks (+ (* ms tick/ticks-per-ms)
                            (* us tick/ticks-per-us)
                            ticks)
         seconds-fraction (double (/ sub-second-ticks tick/ticks-per-second))
         total-seconds (+ seconds seconds-fraction)
         f2 (partial format "%02d")]
     (str (f2 year) "-" (f2 month) "-" (f2 day-of-month) "T"
       (f2 hours) ":" (f2 minutes) ":"
       (format (str "%0" (+ 3 seconds-fraction-precision) "." seconds-fraction-precision "f")
         total-seconds)))))

(s/fdef date->local-string
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date
          :seconds-fraction-precision (s/? ::tick/fraction-precision))
  :ret string?)

(defn local-string->date
  "Parses a local time string in `zone-id` to a UTC tick date.

  Accepts format: YYYY-MM-DDTHH:MM:SS.ffffff

  Options map:
  - `::dst-resolution` - for ambiguous times: `:earlier` (default), `:later`, or `:error`
  - `::gap-resolution` - for invalid times: `:pre-gap`, `:post-gap` (default), or `:error`

  Returns an anomaly if parsing fails or DST resolution fails.

  Example:
    (local-string->date \"America/New_York\" \"2024-07-04T12:00:00.000000\")"
  ([zone-id local-string]
   (local-string->date zone-id local-string {}))
  ([zone-id local-string opts]
   (let [anomaly {::anomalies/category ::anomalies/exception
                  ::anomalies/fn       (var local-string->date)
                  ::anomalies/message  "bad local-string"}]
     (try
       (let [parts (str/split local-string #"-|T")
             [year-str month-str day-time-str time-str] parts]
         (if (< (count parts) 3)
           anomaly
           (let [year (parse-long year-str)
                 month (parse-long month-str)
                 day-str (if time-str day-time-str (first (str/split day-time-str #":")))
                 day-of-month (parse-long day-str)
                 time-part (or time-str
                             (when (str/includes? day-time-str ":")
                               (str/join ":" (rest (str/split day-time-str #":")))))
                 time-parts (when time-part (str/split time-part #":"))
                 hours (if time-parts (parse-long (first time-parts)) 0)
                 minutes (if (and time-parts (> (count time-parts) 1))
                           (parse-long (second time-parts)) 0)
                 seconds-str (when (and time-parts (> (count time-parts) 2))
                               (nth time-parts 2))
                 seconds-decimal (if seconds-str (parse-double seconds-str) 0.0)
                 seconds-whole (long seconds-decimal)
                 seconds-frac (- seconds-decimal seconds-whole)
                 total-sub-second-ticks (long (* seconds-frac tick/ticks-per-second))
                 ms (quot total-sub-second-ticks tick/ticks-per-ms)
                 remaining-ticks (mod total-sub-second-ticks tick/ticks-per-ms)
                 us (quot remaining-ticks tick/ticks-per-us)
                 ticks (mod remaining-ticks tick/ticks-per-us)]
             (if (or (nil? year) (nil? month) (nil? day-of-month)
                   (nil? hours) (nil? minutes))
               anomaly
               (let [breakdown {::tick/day-of-month day-of-month
                                ::tick/hours        hours
                                ::tick/minutes      minutes
                                ::tick/month        month
                                ::tick/ms           ms
                                ::tick/seconds      seconds-whole
                                ::tick/ticks        ticks
                                ::tick/us           us
                                ::tick/year         year}]
                 (local-breakdown->date zone-id breakdown opts))))))
       (catch Exception _
         anomaly)))))

(s/fdef local-string->date
  :args (s/cat :zone-id ::zone-id
          :local-string string?
          :opts (s/? ::conversion-opts))
  :ret (s/or :date ::tick/date
         :anomaly ::anomalies/anomaly))

(defn format-with-zone
  "Formats a UTC tick `date` with the timezone name appended.

  Example:
    (format-with-zone \"America/New_York\" tick/date-2020)
    ;; => \"2019-12-31T19:00:00.000000 America/New_York\""
  [zone-id date]
  (str (date->local-string zone-id date) " " zone-id))

(s/fdef format-with-zone
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date)
  :ret string?)

;;;ZONE-AWARE CALENDAR OPS
(defn start-of-day-in-zone
  "Returns the UTC tick date for the start of the day (midnight) in `zone-id` containing `date`.

  May return an anomaly if the start of day falls in a DST gap.

  Example:
    (start-of-day-in-zone \"America/New_York\" some-date)"
  [zone-id date]
  (let [breakdown (date->local-breakdown zone-id date #{})
        midnight (assoc breakdown
                   ::tick/hours 0
                   ::tick/minutes 0
                   ::tick/seconds 0
                   ::tick/ms 0
                   ::tick/us 0
                   ::tick/ticks 0)]
    (local-breakdown->date zone-id midnight {::gap-resolution :post-gap})))

(s/fdef start-of-day-in-zone
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date)
  :ret (s/or :date ::tick/date
         :anomaly ::anomalies/anomaly))

(defn end-of-day-in-zone
  "Returns the UTC tick date for the end of the day (midnight of next day) in `zone-id` containing
  `date`.

  May return an anomaly if the end of day falls in a DST gap.

  Example:
    (end-of-day-in-zone \"America/New_York\" some-date)"
  [zone-id date]
  (let [breakdown (date->local-breakdown zone-id date #{})
        {::tick/keys [year month day-of-month]} breakdown
        next-day (if (= day-of-month (instant/days-in-month [year month]))
                   (if (= month 12)
                     {::tick/day-of-month 1 ::tick/month 1 ::tick/year (inc year)}
                     {::tick/day-of-month 1 ::tick/month (inc month) ::tick/year year})
                   {::tick/day-of-month (inc day-of-month) ::tick/month month ::tick/year year})
        midnight (assoc next-day
                   ::tick/hours 0
                   ::tick/minutes 0
                   ::tick/seconds 0
                   ::tick/ms 0
                   ::tick/us 0
                   ::tick/ticks 0)]
    (local-breakdown->date zone-id midnight {::gap-resolution :post-gap})))

(s/fdef end-of-day-in-zone
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date)
  :ret (s/or :date ::tick/date
         :anomaly ::anomalies/anomaly))

(defn local-day-of-week
  "Returns the day of week for `date` as seen in `zone-id`.

  Returns one of: `:monday`, `:tuesday`, `:wednesday`, `:thursday`, `:friday`, `:saturday`,
  `:sunday`.

  Example:
    (local-day-of-week \"America/New_York\" tick/date-2020)
    ;; => :tuesday (Dec 31, 2019 in Eastern time)"
  [zone-id date]
  (let [zone (zone-id->java-zone zone-id)
        instant (tick-date->java-instant date)
        zdt (ZonedDateTime/ofInstant instant zone)
        dow (.getDayOfWeek zdt)
        dow-value (.getValue dow)]
    (nth [:monday :tuesday :wednesday :thursday :friday :saturday :sunday]
      (dec dow-value))))

(s/fdef local-day-of-week
  :args (s/cat :zone-id ::zone-id
          :date ::tick/date)
  :ret ::tick/day-of-week)
