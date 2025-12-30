# Provisdom Date

A Clojure library for handling dates and time with a focus on accuracy and performance.

## Overview

Provisdom Date provides tools for working with dates, time, and temporal concepts in a way that avoids common pitfalls found in other date libraries. It offers:

- Long-based date representation centered around 2070 as an epoch ("ticks")
- Specialized collections optimized for date operations
- Comprehensive date/time breakdown functions
- Support for date ranges, intervals, and time periods
- Business day calculations with holiday support
- Fiscal year handling with configurable start month
- Lazy date sequences (daily, weekly, monthly, yearly)
- Named periods (:ytd, :mtd, :last-quarter, :trailing-12-months, etc.)
- ISO week date support (week numbers, week year)
- Relative time formatting ("3 days ago", "in 2 weeks")
- Fully spec'd API with predicates

The library consists of three main namespaces:

1. `provisdom.date.collections` - Specialized collections for dates based on clojure.data.int-map
2. `provisdom.date.instant` - Basic helpers for working with Java's `inst` and millisecond timestamps
3. `provisdom.date.tick` - Core date and time functionality using ticks (1/1144 of a microsecond) as the base unit

## Installation

Add to your deps.edn:

```clojure
{:deps {provisdom/date {:git/url "https://github.com/Provisdom/date.git" :sha "..."}}}
```

## Choosing a Namespace

The library provides two main approaches for working with dates:

### `provisdom.date.tick` (Recommended for most use cases)
High-precision date handling using "ticks" (1/1144 of a microsecond) as the base unit. Use this when you need:
- Sub-millisecond precision
- Accurate date arithmetic that avoids floating-point errors
- Financial or scientific calculations where precision matters

### `provisdom.date.instant`
Utilities for working with Java's `Date` and millisecond timestamps. Includes:
- Date component extraction (year, month, day, hour, etc.)
- Day-of-week computation and predicates (weekend?, weekday?)
- Duration arithmetic (add-ms, add-days, add-weeks, etc.)
- Calendar navigation (start-of-day, start-of-month, etc.)
- Interval operations (contains?, overlaps?, intersection, union)
- ISO-8601 parsing and formatting

Use this when:
- You only need millisecond precision
- You're doing simple Java Date interop
- You need constants like `ms-per-day` or `average-days-per-year`

### `provisdom.date.collections`
Specialized high-performance collections for dates using `clojure.data.int-map`. Includes:
- date-map and date-set constructors with predicates
- Set operations (union, intersection, difference)
- Range constructors for creating date sequences
- Slice operations for extracting date ranges
- Floor/ceiling lookups for nearest-date queries
- Filter operations for maps

Use when storing many dates in maps or sets for O(1) operations.

## Usage

### Basic Date Operations

```clojure
(require '[provisdom.date.tick :as tick])

;; Get current date (in ticks from 2070 epoch)
(tick/date$)

;; Convert between Java dates and ticks
(def now (tick/date$))
(def java-date (tick/date->instant now))
(tick/instant->date java-date)

;; Date breakdown
(tick/date->breakdown now)
;; => #::tick{:year 2023, :month 4, :day-of-month 12, ...}

;; Format date as string
(tick/format-date now)
;; => "2023-04-12T10:30:45.123.456:789"

;; Create date from year/month/day
(tick/breakdown->date 
  #::tick{:year 2024, :month 2, :day-of-month 29})
```

### Date Manipulations

```clojure
;; Add months to a date
(tick/add-months-to-date now 3)

;; Start of month/day/year
(tick/start-of-month now)
(tick/start-of-day now)
(tick/start-of-year now)

;; End of month/day/year
(tick/end-of-month now)
(tick/end-of-day now)
(tick/end-of-year now)

;; Day of week
(tick/day-of-week now) ;; => :monday, :tuesday, etc.

;; Predicates
(tick/weekend? now)
(tick/weekday? now)
(tick/first-day-of-month? now)
(tick/last-day-of-month? now)
```

### Date Intervals and Ranges

```clojure
(def date-range [start-date end-date])

;; Get periods
(tick/date-range->average-years date-range)  ;; in average years
(tick/months-difference date-range)          ;; whole months
(tick/date-range->prorated-months date-range)  ;; fractional months

;; Check if same day
(tick/same-day? date-range)

;; Range predicates
(tick/date-in-range? some-date date-range)  ;; is date within range?
(tick/ranges-overlap? range1 range2)        ;; do ranges overlap?
(tick/range-intersection range1 range2)     ;; get overlapping portion
(tick/range-contains? outer inner)          ;; does outer contain inner?
```

### Business Days

```clojure
;; Check if business day (weekday, not a holiday)
(tick/business-day? date)
(tick/business-day? date #{holiday1 holiday2})

;; Add business days (skips weekends and holidays)
(tick/add-business-days date 5)
(tick/add-business-days date -3 holiday-set)

;; Count business days in range
(tick/business-days-between [start end])
(tick/business-days-between [start end] holiday-set)
```

### Fiscal Years

```clojure
;; October fiscal year (common for US government)
(tick/fiscal-year date 10)           ;; Nov 2020 => FY 2021
(tick/start-of-fiscal-year date 10)  ;; => Oct 1 of fiscal year
(tick/end-of-fiscal-year date 10)    ;; => Oct 1 of next fiscal year
```

### Date Sequences

```clojure
;; Lazy sequence of dates
(take 10 (tick/date-seq start-date))                          ;; daily
(take 10 (tick/date-seq start-date {:step-unit :week}))       ;; weekly
(take 10 (tick/date-seq start-date {:step-unit :month}))      ;; monthly
(take 10 (tick/date-seq start-date {:step-unit :month :step-amount 3})) ;; quarterly

;; Bounded sequence
(tick/date-seq start-date {:end-date end-date})
```

### Named Periods

```clojure
;; Get date range for common periods relative to a reference date
(tick/period->date-range :ytd reference-date)   ;; year to date
(tick/period->date-range :mtd reference-date)   ;; month to date
(tick/period->date-range :qtd reference-date)   ;; quarter to date
(tick/period->date-range :last-month reference-date)
(tick/period->date-range :last-quarter reference-date)
(tick/period->date-range :trailing-12-months reference-date)

;; Year-over-year comparison
(tick/same-period-previous-year :ytd reference-date)
```

### ISO Week Dates & Ordinal Dates

```clojure
;; ISO week number and year
(tick/iso-week-number date)  ;; 1-53
(tick/iso-week-year date)    ;; may differ from calendar year at boundaries

;; Formatting
(tick/format-iso-week-date date)  ;; "2020-W15-3" (Wed of week 15)
(tick/format-ordinal-date date)   ;; "2020-045" (45th day of year)

;; Day of year
(tick/ordinal-day date)  ;; 1-366
```

### Relative Formatting

```clojure
;; Human-readable relative time
(tick/format-relative past-date reference-date)
;; => "3 days ago", "in 2 weeks", "yesterday", "tomorrow", "just now"

;; Using current time as reference
(tick/format-relative some-date)
```

### Working with Instants (Millisecond Precision)

```clojure
(require '[provisdom.date.instant :as instant])

;; Get current instant
(instant/inst$)

;; Extract components
(instant/inst->year #inst "2024-03-15")
(instant/inst->month #inst "2024-03-15")
(instant/inst->day-of-week #inst "2024-03-15") ;; => :friday

;; Duration arithmetic
(instant/add-days #inst "2024-03-15" 7)
(instant/add-hours #inst "2024-03-15" 24)

;; Calendar navigation
(instant/start-of-month #inst "2024-03-15")
(instant/end-of-quarter #inst "2024-03-15")

;; Predicates
(instant/weekend? #inst "2024-03-16") ;; => true (Saturday)
(instant/same-day? #inst "2024-03-15T08:00:00" #inst "2024-03-15T20:00:00")

;; Interval operations
(instant/interval-contains?
  [#inst "2024-03-01" #inst "2024-03-31"]
  #inst "2024-03-15") ;; => true

;; Parsing and formatting
(instant/parse-inst "2024-03-15T14:30:45.123Z")
(instant/format-inst #inst "2024-03-15T14:30:45.123")
```

### Working with Collections

```clojure
(require '[provisdom.date.collections :as cols])

;; Create optimized maps and sets for dates
(def dm (cols/date-map tick/date-2020 :value1))
(def ds (cols/date-set [tick/date-2020 tick/date-2045]))
(def dense-ds (cols/dense-date-set consecutive-dates))

;; Set operations
(cols/set-union ds1 ds2)
(cols/set-intersection ds1 ds2)
(cols/set-difference ds1 ds2)

;; Range constructors
(cols/date-range-set start-date end-date tick/ticks-per-day)
(cols/date-range-map start-date end-date tick/ticks-per-day value-fn)

;; Slice operations
(cols/set-slice ds start-date end-date)
(cols/map-slice dm start-date end-date)

;; Nearest-date lookups
(cols/set-floor ds query-date)   ;; greatest date <= query
(cols/set-ceiling ds query-date) ;; least date >= query
(cols/map-floor dm query-date)
(cols/map-ceiling dm query-date)

;; Filter operations
(cols/map-filter-vals pos? dm)
(cols/map-filter-keys weekend? dm)

;; Merge operations
(cols/map-merge dm1 dm2)
(cols/map-merge-with + dm1 dm2)
```

## Design Decisions

### Ticks as Base Unit

This library uses "ticks" (1/1144 of a microsecond) as the base unit for time. This precise unit was carefully chosen to ensure accuracy in date/time calculations:

- 400 years = 146,097 days
- Divisible by 2^12 and all integers 1-16
- Avoids precision loss in most time period partitioning operations

### 2070 as Epoch

The library centers dates around 2070 as an epoch (rather than 1970/Unix epoch) to:
- Center around a practical range (1814-2325)
- Provide balanced positive/negative values for likely usage scenarios

## License

Copyright © 2016 Provisdom

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
