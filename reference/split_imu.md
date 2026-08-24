# Split an IMU vector at regular intervals

Split the bursts in an IMU vector into bursts of a given time duration.
The result is a list of vectors of the same length as the input, with
the same class as `x`.

## Usage

``` r
split_imu(x, interval)
```

## Arguments

- x:

  An IMU vector (`acc`, `mag`, or `gyro`)

- interval:

  Numeric or
  [units](https://r-quantities.github.io/units/reference/units.html)
  object defining the time intervals at which `x` will be split. If no
  units are provided, the interval is assumed to be in seconds.

## Value

A list of vectors (same class as `x`), the same length as `x`. Each
element contains the split pieces of the corresponding input burst.

## Details

Bursts with `NA` frequency will not be split, as a burst duration can't
be derived. In these cases, the burst is returned unchanged.

## Examples

``` r
a <- acc(
  list(cbind(X = 1:60, Y = 1:60), cbind(X = 101:140)),
  frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
  start = as.POSIXct(c(0, 10), origin = "1970-01-01", tz = "UTC")
)

x <- split_imu(a, units::set_units(1, "s"))
x
#> [[1]]
#> <acceleration[3]>
#> [1] (10.5 10.5) (30.5 30.5) (50.5 50.5)
#> # frequency: 20 [Hz]
#> [[2]]
#> <acceleration[1]>
#> [1] (120.5)
#> # frequency: 40 [Hz]

# Flatten to a single vector
flat <- purrr::reduce(x, c)
flat
#> <acceleration[4]>
#> [1] (10.5 10.5) (30.5 30.5) (50.5 50.5) (120.5)    
#> # frequency: 20 [Hz] - 40 [Hz]

# Start times are updated to match the start of each split component
starts(flat)
#> [1] "1970-01-01 00:00:00 UTC" "1970-01-01 00:00:01 UTC"
#> [3] "1970-01-01 00:00:02 UTC" "1970-01-01 00:00:10 UTC"

# As the bursts immediately follow one another, they can be remerged:
m <- merge_imu(flat, drop = TRUE)
m
#> <acceleration[2]>
#> [1] (30.5 30.5) (120.5)    
#> # frequency: 20 [Hz] - 40 [Hz]

identical(m, a)
#> [1] TRUE

if (FALSE) { # \dontrun{
# In a dataframe, split and unnest to retain index matching
library(dplyr)
library(tidyr)

tbl <- tibble::tibble(id = c("a", "b"), burst = a)

tbl <- tbl |>
  mutate(burst = split_imu(burst, units::set_units(1, "s"))) |>
  unnest(burst) |>
  mutate(timestamp = starts(burst))

tbl

# Use merge_imu() to recover original bursts
tbl |>
  mutate(burst = merge_imu(burst, ids = id, drop = FALSE))
} # }
```
