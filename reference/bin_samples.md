# Count samples within time bins for a set of sensors

Summarize sensor sampling regimes for a set of `imu` or timestamp
vectors by counting samples within fixed time bins. This produces the
data that is used by
[`plot_sampling_effort()`](https://move2universe.github.io/move2imu/reference/plot_sampling_effort.md),
exposed for custom plots.

## Usage

``` r
bin_samples(..., ids = NULL, bin_width = NULL, from = NULL, to = NULL)
```

## Arguments

- ...:

  Any number of `imu` or timestamp vectors. All vectors must be the same
  length. Timestamps can be in `POSIXct`, `POSIXlt`, `Date`, or a number
  of seconds since `1970-01-01 00:00:00 UTC`. `Date` objects are treated
  as being recorded at midnight, UTC.

- ids:

  Vector of IDs used to group the observations in `...`. All
  observations for each group will be included in a single panel. Must
  be the same length as each of the vectors in `...`.

- bin_width:

  Width of the time bins within which samples are counted. Provided as a
  [units](https://r-quantities.github.io/units/reference/units.html)
  object, a [difftime](https://rdrr.io/r/base/difftime.html) object, or
  a numeric value which will be interpreted as seconds. By default, the
  time range of the plot is divided into roughly 300 bins.

  Decrease the `bin_width` to increase plot resolution, at the expense
  of legibility for sparsely collected data.

- from, to:

  Start and end timestamps defining the range within which samples will
  be counted. Accepts the same formats as timestamps in `...`. By
  default, the full temporal extent of the data is used.

## Value

A data frame with one row per non-empty lane, id, and bin, containing
the following columns:

- `lane`: The source vector in `...`, labeled with that argument's name
  or expression. Each lane is drawn as one row by
  [`plot_sampling_effort()`](https://move2universe.github.io/move2imu/reference/plot_sampling_effort.md).

- `id`: Grouping identifier, as a factor. Absent when `ids` is `NULL`.

- `time`: Bin start, in the timezone of the first lane.

- `n`: Samples recorded in the bin.

- `rate`: Effective sampling rate, `n / bin_width`, in Hz.

- `effort`: `rate` as a fraction of the highest rate in the same lane,
  taken across all values of `ids`.

The bin width in seconds is attached as the `"bin_width"` attribute.

## Details

Sampling effort is calculated by

1.  splitting the plotted time range into equal-width bins. The binned
    grid starts at `from` if provided. Otherwise, it starts at the
    closest multiple of `bin_width` at or before the first sample.

2.  counting the number of samples each sensor recorded in each bin,
    separately for every track.

3.  dividing each count by the bin width, giving an effective sampling
    rate in Hz.

4.  normalizing those rates within each lane, so that each lane's
    sampling effort values are relative to the maximum sampling effort
    in that lane, across all values of `ids`.

## See also

[`plot_sampling_effort()`](https://move2universe.github.io/move2imu/reference/plot_sampling_effort.md)
to plot the output of `bin_samples()`

## Examples

``` r
alb <- albatrosses()

bin_samples(acc = as_acc(alb), ids = move2::mt_track_id(alb))
#>    lane            id                time  n      rate     effort
#> 1   acc 4266-84831108 2008-07-27 00:00:51 38 3.1686030 0.65517241
#> 2   acc 4266-84831108 2008-07-27 00:01:03 22 1.8344544 0.37931034
#> 3   acc 4266-84831108 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 4   acc 4266-84831108 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 5   acc 4266-84831108 2008-07-27 00:29:50 13 1.0839958 0.22413793
#> 6   acc 4266-84831108 2008-07-27 00:30:02 47 3.9190616 0.81034483
#> 7   acc 4266-84831108 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 8   acc 4266-84831108 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 9   acc 4266-84831108 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 10  acc 4266-84831108 2008-07-27 01:00:01 53 4.4193674 0.91379310
#> 11  acc     4261-2228 2008-07-27 00:00:03  3 0.2501529 0.05172414
#> 12  acc     4261-2228 2008-07-27 00:00:15 57 4.7529046 0.98275862
#> 13  acc     4261-2228 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 14  acc     4261-2228 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 15  acc     4261-2228 2008-07-27 00:29:50 13 1.0839958 0.22413793
#> 16  acc     4261-2228 2008-07-27 00:30:02 47 3.9190616 0.81034483
#> 17  acc     4261-2228 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 18  acc     4261-2228 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 19  acc     4261-2228 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 20  acc     4261-2228 2008-07-27 01:00:01 53 4.4193674 0.91379310
#> 21  acc     2131-2131 2008-07-27 00:00:51 43 3.5855245 0.74137931
#> 22  acc     2131-2131 2008-07-27 00:01:03 17 1.4175329 0.29310345
#> 23  acc     2131-2131 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 24  acc     2131-2131 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 25  acc     2131-2131 2008-07-27 00:29:50 13 1.0839958 0.22413793
#> 26  acc     2131-2131 2008-07-27 00:30:02 47 3.9190616 0.81034483
#> 27  acc     2131-2131 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 28  acc     2131-2131 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 29  acc     2131-2131 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 30  acc     2131-2131 2008-07-27 01:00:01 53 4.4193674 0.91379310
#> 31  acc     1163-1163 2008-07-27 00:00:39  8 0.6670743 0.13793103
#> 32  acc     1163-1163 2008-07-27 00:00:51 52 4.3359831 0.89655172
#> 33  acc     1163-1163 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 34  acc     1163-1163 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 35  acc     1163-1163 2008-07-27 00:30:02 58 4.8362888 1.00000000
#> 36  acc     1163-1163 2008-07-27 00:30:14  2 0.1667686 0.03448276
#> 37  acc     1163-1163 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 38  acc     1163-1163 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 39  acc     1163-1163 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 40  acc     1163-1163 2008-07-27 01:00:01 53 4.4193674 0.91379310
#> 41  acc    3275-30662 2008-07-27 00:00:39 43 3.5855245 0.74137931
#> 42  acc    3275-30662 2008-07-27 00:00:51 17 1.4175329 0.29310345
#> 43  acc    3275-30662 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 44  acc    3275-30662 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 45  acc    3275-30662 2008-07-27 00:29:50 13 1.0839958 0.22413793
#> 46  acc    3275-30662 2008-07-27 00:30:02 47 3.9190616 0.81034483
#> 47  acc    3275-30662 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 48  acc    3275-30662 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 49  acc    3275-30662 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 50  acc    3275-30662 2008-07-27 01:00:01 53 4.4193674 0.91379310
#> 51  acc  unbanded-153 2008-07-27 00:00:51 43 3.5855245 0.74137931
#> 52  acc  unbanded-153 2008-07-27 00:01:03 17 1.4175329 0.29310345
#> 53  acc  unbanded-153 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 54  acc  unbanded-153 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 55  acc  unbanded-153 2008-07-27 00:29:50 13 1.0839958 0.22413793
#> 56  acc  unbanded-153 2008-07-27 00:30:02 47 3.9190616 0.81034483
#> 57  acc  unbanded-153 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 58  acc  unbanded-153 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 59  acc  unbanded-153 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 60  acc  unbanded-153 2008-07-27 01:00:01 53 4.4193674 0.91379310
#> 61  acc  unbanded-154 2008-07-27 00:00:51 38 3.1686030 0.65517241
#> 62  acc  unbanded-154 2008-07-27 00:01:03 22 1.8344544 0.37931034
#> 63  acc  unbanded-154 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 64  acc  unbanded-154 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 65  acc  unbanded-154 2008-07-27 00:29:50 13 1.0839958 0.22413793
#> 66  acc  unbanded-154 2008-07-27 00:30:02 47 3.9190616 0.81034483
#> 67  acc  unbanded-154 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 68  acc  unbanded-154 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 69  acc  unbanded-154 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 70  acc  unbanded-154 2008-07-27 01:00:01 53 4.4193674 0.91379310
#> 71  acc  unbanded-156 2008-07-27 00:00:15 53 4.4193674 0.91379310
#> 72  acc  unbanded-156 2008-07-27 00:00:27  7 0.5836900 0.12068966
#> 73  acc  unbanded-156 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 74  acc  unbanded-156 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 75  acc  unbanded-156 2008-07-27 00:29:50 13 1.0839958 0.22413793
#> 76  acc  unbanded-156 2008-07-27 00:30:02 47 3.9190616 0.81034483
#> 77  acc  unbanded-156 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 78  acc  unbanded-156 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 79  acc  unbanded-156 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 80  acc  unbanded-156 2008-07-27 01:00:01 53 4.4193674 0.91379310
#> 81  acc  unbanded-159 2008-07-27 00:00:03  8 0.6670743 0.13793103
#> 82  acc  unbanded-159 2008-07-27 00:00:15 52 4.3359831 0.89655172
#> 83  acc  unbanded-159 2008-07-27 00:14:51 16 1.3341486 0.27586207
#> 84  acc  unbanded-159 2008-07-27 00:15:03 44 3.6689088 0.75862069
#> 85  acc  unbanded-159 2008-07-27 00:29:50 13 1.0839958 0.22413793
#> 86  acc  unbanded-159 2008-07-27 00:30:02 47 3.9190616 0.81034483
#> 87  acc  unbanded-159 2008-07-27 00:44:49 10 0.8338429 0.17241379
#> 88  acc  unbanded-159 2008-07-27 00:45:01 50 4.1692145 0.86206897
#> 89  acc  unbanded-159 2008-07-27 00:59:49  7 0.5836900 0.12068966
#> 90  acc  unbanded-159 2008-07-27 01:00:01 53 4.4193674 0.91379310
```
