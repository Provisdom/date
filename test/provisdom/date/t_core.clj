(ns provisdom.date.t-core
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.date :refer :all]
    [provisdom.math.core :as m]))

(deftest t-leap-year?
  (is (leap-year? 2000))
  (is-not (leap-year? 2001))
  (is (leap-year? 2004)))

(deftest t-days-per-month
  (is= 31 (days-per-month 2014 3))
  (is= 28 (days-per-month 2014 2))
  (is= 31 (days-per-month 2014 1))
  (is= 29 (days-per-month 2004 2)))

(deftest t-days-until-month
  (is= 59 (days-until-month 2014 3))
  (is= 31 (days-until-month 2014 2))
  (is (zero? (days-until-month 2014 1)))
  (is= 31 (days-until-month 2004 2)))

(deftest t-passed-leap-days
  (is= 2070 epoch)
  (is= -13 (passed-leap-days 2016 3))
  (is= -14 (passed-leap-days 2014 2))
  (is= 2 (passed-leap-days 2014 2 2020 9))
  (is (zero? (passed-leap-days 2014 2 2014 5)))
  (is= 1 (passed-leap-days 2000 1 2000 3))
  (is= 1 (passed-leap-days 2000 1 2004 1))
  (is= 2 (passed-leap-days 2000 1 2004 3))
  (is= 2 (passed-leap-days 2000 1 2005 1))
  (is= 25 (passed-leap-days 2000 1 2100 1))
  (is= 25 (passed-leap-days 2000 1 2100 3))
  (is= 97 (passed-leap-days 2000 1 2400 1))
  (is= 98 (passed-leap-days 2000 1 2400 3))
  (is (zero? (passed-leap-days 2000 1 1996 3)))
  (is= -1 (passed-leap-days 2000 1 1996 1))
  (is= -24 (passed-leap-days 2000 1 1900 3))
  (is= -24 (passed-leap-days 2000 1 1900 1))
  (is= -96 (passed-leap-days 2000 1 1600 3))
  (is= -97 (passed-leap-days 2000 1 1600 1)))

(deftest t-read-ticks
  (is=
    {:da 6, :hr 15, :mi 23, :ms 318, :se 33, :ti 904, :us 504, :wk 1}
    (read-ticks 1348333636369480))
  (is=
    {:da 13, :ti 904, :us 55413318504}
    (read-ticks 1348333636369480 [:da :us])))

(deftest t-read-duration
  (is=
    {:ti 529,
     :yr 2,
     :hr 0,
     :mo 2,
     :us 575,
     :mi 0,
     :se 0,
     :da 2,
     :wk 0,
     :ms 10}
    (read-duration [26 197683212098329]))
  (is=
    {:hr 48, :mi 0, :mo 26, :ti 12098329}
    (read-duration [26 197683212098329] [:hr :mi])))

(deftest t-read-date
  (is=
    {:ti 0,
     :yr 1814,
     :hr 0,
     :mo 7,
     :tz 0,
     :us 0,
     :mi 0,
     :se 0,
     :da 9,
     :ms 0}
    (read-date -9223305062400000000))
  (is=
    {:ti 0,
     :yr 2325,
     :hr 0,
     :mo 6,
     :tz 0,
     :us 0,
     :mi 0,
     :se 0,
     :da 28,
     :ms 0}
    (read-date 9223305062400000000))
  (is=
    {:ti 656,
     :yr 2014,
     :hr 0,
     :mo 3,
     :tz 0,
     :us 446,
     :mi 0,
     :se 0,
     :da 3,
     :ms 11}
    (read-date -2015676748786905120))
  (is=
    {:da 3, :mo 3, :ms 11, :se 0, :ti 510880, :tz 0, :yr 2014}
    (read-date -2015676748786905120 [:se :ms]))
  (is=
    {:da 3, :mo 3, :ms 11, :se 0, :ti 510880, :tz 3, :yr 2014}
    (read-date -2015676748786905120 [:se :ms] 3))
  (is=
    {:ti 0,
     :yr 2041,
     :hr 0,
     :mo 1,
     :tz 0,
     :us 0,
     :mi 0,
     :se 0,
     :da 29,
     :ms 0}
    (read-date -1044162662400000000))
  (is=
    {:ti 226,
     :yr 2041,
     :hr 3,
     :mo 4,
     :tz 0,
     :us 814,
     :mi 31,
     :se 7,
     :da 12,
     :ms 997}
    (read-date -1036932733410500558))
  (is=
    {:ti 0,
     :yr 2016,
     :hr 0,
     :mo 2,
     :tz 0,
     :us 0,
     :mi 0,
     :se 0,
     :da 22,
     :ms 0}
    (read-date 2015 13 53 0))
  (is=
    {:da 22, :mo 2, :ms 0, :se 0, :ti 0, :tz 0, :yr 2016}
    (read-date 2015 13 53 0 [:se :ms]))
  (is=
    {:da 22, :mo 2, :ms 0, :se 0, :ti 0, :tz -8, :yr 2016}
    (read-date 2015 13 53 0 [:se :ms] -8))
  (is=
    {:ti 0,
     :yr 2015,
     :hr 0,
     :mo 1,
     :tz 0,
     :us 0,
     :mi 0,
     :se 0,
     :da 1,
     :ms 0}
    (read-date (date 2015)))
  (is= [:hr :mi :se :ms :us] full-date-map)
  (is=
    {:ti 78,
     :yr 2014,
     :hr 5,
     :mo 3,
     :tz -8,
     :us 939,
     :mi 25,
     :se 49,
     :da 3,
     :ms 731}
    (read-date 2014 3 3 22364893338294 full-date-map -8)))

(deftest t-day-of-week
  (is= :we (day-of-week (date 2014 1 1 0)))
  (is= :th (day-of-week (date 2015 1 1 0))))

(deftest t-instant
  (is= #inst "2014-01-01T00:00:00.000-00:00" (instant (date 2014)))
  (is= -2051753932800000000 (instant->date (instant (date 2013 3 3)))))

(deftest t-time-millis
  (is= 1388534400000 (time-millis (date 2014)))
  (is= -2021706086400000000 (time-millis->date 1388534400000)))

(deftest t-create-date-with-time-zone
  (is=
    [-2021706086400000000 -8]
    (joda->date-with-time-zone (joda (date 2014 1 1 0) -8))))

(deftest t-period
  (is= 0.05749604714675866 (period (as-ticks 3)))
  (is= 0.9993360575508053 (period [(date 2014) (date 2015)])))

(deftest t-as-ticks
  (is= 2075673600000000 (as-ticks 3))
  (is= 2372198400000000 (as-ticks 3 3))
  (is= 2372198400000003 (as-ticks 3 3 3))
  (is= 2372224739456003 (as-ticks 3 3 3 :se 23 :ms 24)))

(deftest t-maybe-duration-as-ticks
  (is= 2347 (maybe-duration-as-ticks [0 2347]))
  (is (nil? (maybe-duration-as-ticks [1 2346]))))

(deftest t-duration
  (is= [24 0] (duration 2))
  (is= [26 0] (duration 2 2))
  (is= [26 197683200000000] (duration 2 2 2))
  (is= [26 197683212098329] (duration 2 2 2 12098329))
  (is=
    [18 224010436395792]
    (duration 1 6 0 63392836369480 :hr 39 :us 23)))

(deftest t-date
  (is= -9223305062400000000 (date 1814 7 9 0))
  (is (thrown? Exception (date 1814 7 8 0)))
  (is= 9223305062400000000 (date 2325 6 28 0))
  (is (thrown? Exception (date 2325 6 29 0)))
  (is= -2021706086400000000 (date 2014))
  (is= -2015874432000000000 (date 2014 3))
  (is= -2015676748800000000 (date 2014 3 3))
  (is= -2015676748786905120 (date 2014 3 3 13094880))
  (is= -2015670365266905120 (date 2014 3 3 13094880 :mi 93))
  (is= -902522649600000000 (date 2045 1 1 0))
  (is= -1985628902400000000 (date 2015 1 1 0))
  (is= -1043866137600000000 (date 2041 2 1 0))
  (is= -1044162662400000000 (date 2041 1 29 0))
  (is= -1036848384000000000 (date 2041 4 13 0))
  (is=
    {:da 13, :mo 1, :ti 0, :tz 0, :yr 2042}
    (read-date (date 2041 13 13 0) []))
  (is=
    {:da 1, :mo 2, :ti 0, :tz 0, :yr 2042}
    (read-date (date 2041 13 32 0) []))
  (is=
    {:da 31, :mo 10, :ti 0, :tz 0, :yr 2040}
    (read-date (date 2041 -1 -1 0) [])))

(deftest t-year (is= [12 0] year))

(deftest t-month (is= [1 0] month))

(deftest t-adding
  (is= 8994585612321993 (+ (as-ticks 13) 12321993))
  (is= 8994585612321993 (add-ticks (as-ticks 13) 12321993))
  (is=
    [13 9093427200000000]
    (add-ticks (as-ticks 13) (duration 1 1 1)))
  (is=
    [13 9093427200000000]
    (add-ticks (duration 1 1 1) (as-ticks 13)))
  (is=
    {:da 2, :mo 4, :ti 0, :tz 0, :yr 2014}
    (read-date (+ (date 2014) (as-ticks 13)) []))
  (is=
    {:da 2, :mo 4, :ti 0, :tz 0, :yr 2014}
    (read-date (+ (as-ticks 13) (date 2014)) []))
  (is=
    {:da 1, :mo 4, :ti 0, :tz 0, :yr 2014}
    (read-date (add-duration (date 2014) [3 0]) []))
  (is=
    {:da 1, :mo 4, :ti 0, :tz 0, :yr 2014}
    (read-date (add-duration [3 0] (date 2014)) []))
  (is= [6 1] (add-duration [3 0] [3 1]))
  (is= [6 1] (add-ticks [3 0] [3 1]))
  (is (thrown? Exception (add-duration (date 2014) (date 2015))))
  (is= -4007334988800000000 (+ (date 2014) (date 2015)))
  (is=
    {:da 15, :mo 4, :ti 0, :tz 0, :yr 2014}
    (read-date (add-duration (date 2014) [3 (as-ticks 2)]) []))
  (is=
    {:da 5, :mo 4, :ti 0, :tz 0, :yr 2014}
    (read-date
      (add-duration (date 2014) (duration -1 2 3) (duration 1 1 1))
      [])))

(deftest t-subtracting
  (is= 8994585587678007 (- (as-ticks 13) 12321993))
  (is= 8994585587678007 (sub-ticks (as-ticks 13) 12321993))
  (is=
    [13 -8895744000000000]
    (sub-ticks (duration 1 1 1) (as-ticks 13)))
  (is=
    [-13 8895744000000000]
    (sub-ticks (as-ticks 13) (duration 1 1 1)))
  (is=
    {:da 2, :mo 10, :ti 0, :tz 0, :yr 2013}
    (read-date (- (date 2014) (as-ticks 13)) []))
  (is=
    {:da 3, :mo 4, :ti 0, :tz 0, :yr 2126}
    (read-date (- (as-ticks 13) (date 2014)) []))
  (is=
    {:da 1, :mo 10, :ti 0, :tz 0, :yr 2013}
    (read-date (sub-dates (date 2014) [3 0]) []))
  (is (thrown? Exception (read-date (sub-dates [3 0] (date 2014)) [])))
  (is= [10 -197683200000003] (sub-dates (date 2015) (date 2014 3 3 3)))
  (is= 30047846399999997 (- (date 2015) (date 2014 3 3 3)))
  (is= 30047846399999997 (sub-ticks (date 2015) (date 2014 3 3 3)))
  (is= [12 0] (sub-dates (date 2015) (date 2014)))
  (is=
    {:da 1, :hr 0, :mi 0, :ms 0, :se 0, :ti 0, :us 0, :wk 52}
    (read-ticks (- (date 2015) (date 2014)))))

(deftest t-multiply-a-duration-by-a-long
  (is= [39 296524800000000] (mul (duration 1 1 1) 3))
  (is= [39 296524800000000] (mul 3 (duration 1 1 1)))
  (is= 9 (mul 3 3))
  (is (thrown? Exception (mul (duration 1 1 1) (duration 1 1 1))))
  (is= [39 296524800000000] (mul 3.0 (duration 1 1 1)))
  (is (thrown? Exception (mul 3.3 (duration 1 1 1)))))

(deftest t-convert-duration-to-ticks
  (is= 39240115200000000 (to-ticks (duration 1 1 1) (date 2014)))
  (is= 39141273600000000 (to-ticks (duration 1 1 1) (date 2014) true)))

(deftest t-convert-ticks-to-duration
  (is=
    {:da 1, :mo 3, :ti 0}
    (read-duration (to-duration (as-ticks 13) (date 2014)) [:da]))
  (is=
    {:da -1, :mo 3, :ti 0}
    (read-duration (to-duration (as-ticks 13) (date 2014) true) [:da])))

(deftest t-time-zone-for-current-environment
  (is= -8 (environment-time-zone)))

(deftest t-lazy-period
  (is=
    [{:da 3, :mo 3, :ti 0, :tz 0, :yr 2014}
     {:da 3, :mo 6, :ti 0, :tz 0, :yr 2014}
     {:da 3, :mo 9, :ti 0, :tz 0, :yr 2014}]
    (map
      #(read-date % [])
      (take 3 (lazy-period (date 2014 3 3) [3 0]))))
  (is=
    [{:da 3, :mo 3, :ti 0, :tz 0, :yr 2014}
     {:da 3, :mo 4, :ti 0, :tz 0, :yr 2014}
     {:da 4, :mo 5, :ti 0, :tz 0, :yr 2014}]
    (map
      #(read-date % [])
      (take 3 (lazy-period (date 2014 3 3) [0 (as-ticks 4 3)])))))

(deftest t-intervals
  (is=
    [[-2021706086400000000 -2018697656976000000]
     [-2018697656976000000 -2009672368704000000]
     [-2009672368704000000 -1994630221584000000]]
    (intervals m/sq (date 2014) 3 average-month))
  (is=
    [[-2021706086400000000 -2018641996800000000]
     [-2018641996800000000 -2009845094400000000]
     [-2009845094400000000 -1994722329600000000]]
    (intervals m/sq (date 2014) 3 month))
  (is=
    [[-2021706086400000000 -2018697656976000000]
     [-2018697656976000000 -2015689227552000000]
     [-2015689227552000000 -2012680798128000000]]
    (intervals (date 2014) 3 average-month))
  (is=
    [[-2021706086400000000 -2018641996800000000]
     [-2018641996800000000 -2015874432000000000]
     [-2015874432000000000 -2012810342400000000]]
    (intervals (date 2014) 3 month)))

(deftest t-dates
  (is=
    [-2021706086400000000
     -2018641996800000000
     -2015874432000000000
     -2012810342400000000]
    (dates (intervals (date 2014) 3 month))))

(deftest t-periodic t-lazy-period t-intervals t-dates)

(deftest t-unparse-day-of-week
  (is= "Wednesday" (unparse-day-of-week :we))
  (is= "Wed" (unparse-day-of-week :we true))
  (is (nil? (unparse-day-of-week :w))))

(deftest t-unparse-month
  (is= "August" (unparse-month 8))
  (is= "Aug" (unparse-month 8 true))
  (is (nil? (unparse-month 0))))

(deftest t-unparse-duration
  (is=
    "M0T0:48:33.752.913:861"
    (unparse-duration (read-duration [0 3333333333333])))
  (is=
    "M3D3T8:56:15.291.375:333"
    (unparse-duration (read-duration [3 333333333333333])))
  (is=
    "Y2M11D3T8:56:15.291.375:333"
    (unparse-duration (read-duration [35 333333333333333])))
  (is=
    "M35T0:0:0.0.0:333333333333333"
    (unparse-duration (read-duration [35 333333333333333] [])))
  (is=
    "Y2M11D3H8.938"
    (unparse-duration (read-duration [35 333333333333333]) 3)))

(deftest t-unparse-ticks
  (is= "T0:0:0.0.2:1045" (unparse-ticks (read-ticks 3333)))
  (is=
    "T0:48:33.752.913:861"
    (unparse-ticks (read-ticks 3333333333333)))
  (is=
    "W481D5T9:34:51.375.291:429"
    (unparse-ticks (read-ticks 333333333333333333)))
  (is=
    "T0:0:0.0.0:333333333333333"
    (unparse-ticks (read-ticks 333333333333333 [])))
  (is=
    "D3H8.937580937580856"
    (unparse-ticks (read-ticks 333333333333333) 15))
  (is= "H0.001" (unparse-ticks (read-ticks 3333333333) 3)))

(deftest t-unparse-date-map
  (is=
    "2014-03-03T0:4:51.375.291:429Z"
    (unparse-date-map (read-date 2014 3 3 333333333333)))
  (is=
    "2014-03-03T0:4:51.375.291:429Z-8"
    (unparse-date-map
      (read-date 2014 3 3 333333333333 full-date-map -8)))
  (is=
    "2014-03-03T0:0:0.0.0:333333333333Z-8"
    (unparse-date-map (read-date 2014 3 3 333333333333 [] -8)))
  (is=
    "2014-03-03H0.081Z"
    (unparse-date-map (read-date 2014 3 3 333333333333) 3)))

(deftest t-unparse-time
  (is= "03:31" (unparse-time 14547533489534 :hour-minute)))

(deftest t-unparse-date
  (is=
    "2069-12-31T18:23:32.252.364:761Z"
    (unparse-date -23094783294823))
  (is= "20691231" (unparse-date -23094783294823 :basic-date))
  (is= "2069-12-31H18.3923Z" (unparse-date -23094783294823 nil 4)))

(deftest t-parse-day-of-week
  (is= :we (parse-day-of-week "Wednesday"))
  (is= :we (parse-day-of-week "Wed"))
  (is (nil? (parse-day-of-week "W"))))

(deftest t-parse-month
  (is= 8 (parse-month "August"))
  (is= 8 (parse-month "Aug"))
  (is (nil? (parse-month "A"))))

(deftest t-parse-duration
  (is=
    {:mo 0, :ti 3333333333333}
    (read-duration (parse-duration "M0T0:48:33.752.913:861") []))
  (is= [3 333333333333333] (parse-duration "M3D3T8:56:15.291.375:333"))
  (is=
    [35 333333333333333]
    (parse-duration "Y2M11D3T8:56:15.291.375:333"))
  (is=
    [35 333333333333333]
    (parse-duration "M35T0:0:0.0.0:333333333333333"))
  (is= [35 333335059200000] (parse-duration "Y2M11D3H8.938")))

(deftest t-parse-ticks
  (is= 3333 (parse-ticks "T0:0:0.0.2:1045"))
  (is= 3333333333333 (parse-ticks "T0:48:33.752.913:861"))
  (is= 333333333333333333 (parse-ticks "W481D5T9:34:51.375.291:429"))
  (is= 333333333333333 (parse-ticks "T0:0:0.0.0:333333333333333"))
  (is= 333333333333333 (parse-ticks "D3H8.937580937580856"))
  (is= 4118400000 (parse-ticks "H0.001")))

(deftest t-parse-time
  (is= 86898240000000 (parse-time "21:06" :hour-minute)))

(deftest t-parse-date
  (is=
    {:da 3, :mo 3, :ti 333333333333, :tz 0, :yr 2014}
    (read-date
      (first (parse-date "2014-03-03T0:4:51.375.291:429Z"))
      []))
  (is= [-2008856678400000000 0] (parse-date "20140511" :basic-date))
  (is (nil? (parse-date "20140231" :date)))
  (is=
    [-2015676415466666667 -8]
    (parse-date "2014-03-03T0:4:51.375.291:429Z-8"))
  (is=
    [-2015676415466666667 -8]
    (parse-date "2014-03-03T0:0:0.0.0:333333333333Z-8"))
  (is=
    {:ti 0,
     :yr 2014,
     :hr 0,
     :mo 3,
     :tz 0,
     :us 0,
     :mi 4,
     :se 51,
     :da 3,
     :ms 600}
    (read-date (first (parse-date "2014-03-03H0.081Z")))))

(deftest t-weekend
  (is-not (weekend? (date 2014 1 1)))
  (is (weekend? (date 2014 1 4))))

(deftest t-weekday
  (is (weekday? (date 2014 1 1)))
  (is-not (weekday? (date 2014 1 4))))

(deftest t-first-day-of-month
  (is (first-day-of-month? (date 2014 1 1)))
  (is-not (first-day-of-month? (date 2014 1 2))))

(deftest t-last-day-of-month
  (is-not (last-day-of-month? (date 2014 1 1)))
  (is (last-day-of-month? (date 2013 1 31))))

(deftest t-interval
  (is (interval? [(date 2012 1 1) (date 2012 1 2)]))
  (is (interval? [(date 2012 1 1) (date 2012 1 1)]))
  (is-not (interval? [[(date 2012 1 1) (date 2012 1 1)]]))
  (is-not (interval? [[(date 2012 1 1) (date 2012 1 1)]]))
  (is-not
    (interval? [(date 2012 1 1) (date 2012 1 1) (date 2012 1 1)]))
  (is-not (interval? (date 2012 1 1)))
  (is-not (interval? [(date 2012 1 1) (date 2011 1 2)])))

(deftest t-positive-interval
  (is (interval+? [(date 2012 1 1) (date 2012 1 2)]))
  (is-not (interval+? [(date 2012 1 1) (date 2012 1 1)]))
  (is-not (interval+? [(date 2012 1 1) (date 2011 1 2)])))

(deftest t-integrate-interval
  (is=
    0.9993360575508053
    (integrate-interval (fn [d] 1.0) [(date 2014) (date 2015)])))