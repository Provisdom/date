# Provisdom Date

A Clojure library for handling dates and time with a focus on accuracy and performance.

## Overview

Provisdom Date provides tools for working with dates, time, and temporal concepts in a way that avoids common pitfalls found in other date libraries. It offers:

- Long-based date representation centered around 2070 as an epoch ("ticks")
- Specialized collections optimized for date operations
- Comprehensive date/time breakdown functions
- Support for date ranges, intervals, and time periods
- Helpers for various date calculations like months difference, leap years, etc.
- Fully spec'd API with predicates

The library consists of three main namespaces:

1. `provisdom.date.instant` - Basic helpers for working with Java's `inst` and millisecond timestamps
2. `provisdom.date.tick` - Core date and time functionality using ticks (1/1144 of a microsecond) as the base unit
3. `provisdom.date.collections` - Specialized collections for dates based on clojure.data.int-map

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
Basic utilities for working with Java's `Date` and millisecond timestamps. Use this when:
- You only need millisecond precision
- You're doing simple Java Date interop
- You need constants like `ms-per-day` or `average-days-per-year`

### `provisdom.date.collections`
Specialized high-performance collections for dates. Use when storing many dates in maps or sets.

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
(tick/date-range->period date-range)  ;; in average years
(tick/months-difference date-range)   ;; whole months
(tick/date-range->prorated-months date-range)  ;; fractional months

;; Check if same day
(tick/same-day? date-range)
```

### Working with Collections

```clojure
(require '[provisdom.date.collections :as cols])

;; Create optimized maps and sets for dates
(def date-map (cols/date-map))
(def date-set (cols/date-set))
(def dense-date-set (cols/dense-date-set))

;; Use specialized operations
(cols/map-merge date-map1 date-map2)
(cols/set-union date-set1 date-set2)
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
