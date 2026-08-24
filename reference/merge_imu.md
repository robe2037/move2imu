# Merge adjacent bursts in an IMU vector

For a given IMU vector, identify temporally adjacent bursts and merge
them into a single burst. Bursts whose end time coincides with the start
time of the next burst (within `gap_tol`) and whose frequencies agree
(within the relative `freq_tol`) are considered adjacent. Bursts with
different frequencies, axes, or burst data units will not be merged.

To merge bursts with differing units, convert them to a common unit
first with
[`set_imu_units()`](https://move2universe.github.io/move2imu/reference/set_imu_units.md).

## Usage

``` r
merge_imu(x, ids = NULL, gap_tol = 1e-06, freq_tol = 0.01, drop = FALSE)
```

## Arguments

- x:

  An IMU vector (`acc`, `mag`, or `gyro`)

- ids:

  Vector indicating groups to which the elements in `x` belong. If
  provided, bursts in `x` will not be merged across different values of
  this vector, even if their timestamps and frequencies align.

- gap_tol:

  Absolute tolerance (in seconds) to use when determining whether two
  bursts are adjacent in time and can be merged. Two bursts are adjacent
  when the gap between the first burst's end and the second burst's
  start is within `gap_tol`.

  For example, setting `gap_tol = 0.02` would allow a burst that starts
  up to 0.02 seconds after the end of the previous burst to be merged.
  See details.

- freq_tol:

  Relative tolerance to use when determining whether two bursts share a
  sampling frequency. Bursts can only be merged if their frequencies are
  consistent, within `freq_tol`. Bursts can be merged when the faster
  frequency is at most `(1 + freq_tol)` times the slower. For example,
  `freq_tol = 0.01` merges bursts whose frequencies are within 1% of
  each other.

- drop:

  Logical indicating whether to drop entries that have been merged into
  other bursts. If `drop = FALSE` (default), the output will have the
  same length as the input `x`, with `NA` values at positions where
  bursts were merged into a preceding burst. This is useful for
  retaining index matching between the input and output vectors.

## Value

A vector of the same class as `x`.

## Details

A burst's end is taken as one sample period after its last sample. A
burst of `n` samples at frequency `f` therefore ends `n / f` seconds
after its start. The next burst is adjacent when it starts within
`gap_tol` of that point.

After merging, the burst's frequency is recomputed from its new sample
count and overall time span. The gap between the bursts therefore
impacts the derived output frequency of the new burst. If the `gap_tol`
is set to allow any timestamp noise, the gap between the two bursts may
not precisely correspond with the sampling periods of the bursts being
merged. In these cases, the recorded output frequency of the merged
burst will vary slightly from the values of its component bursts.

This approach preserves overall burst time span at the expense of
preserving a consistent burst frequency. If you instead prefer to
preserve frequency, you will need to manually adjust the frequency of
the output burst (see
[`freqs()`](https://move2universe.github.io/move2imu/reference/imu-fields.md))
or correct timestamps in the input data.

Bursts with missing frequencies (e.g. a burst with only one sample) are
not merged. To merge such bursts, you must assign them a sampling
frequency (see
[`freqs()`](https://move2universe.github.io/move2imu/reference/imu-fields.md)).

Note that because the burst duration incorporates the elapsed time of
the period after the last recorded sample, timestamp noise can make a
subsequent burst appear to start slightly "before" the previous burst
ends (a small negative time gap). This jitter will also be incorporated
into `gap_tol`.

## Examples

``` r
a <- acc(
  list(cbind(X = 1:60, Y = 1:60), cbind(X = 61:100, Y = 61:100), cbind(X = 101:140)),
  frequency = units::set_units(20, "Hz"),
  start = as.POSIXct(c(0, 3, 5), origin = "1970-01-01", tz = "UTC")
)

merge_imu(a)
#> <acceleration[3]>
#> [1] (50.5 50.5) <NA>        (120.5)    
#> # frequency: 20 [Hz]
```
