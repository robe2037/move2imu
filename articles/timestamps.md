# Handling timestamp noise in IMU data

Biologging data often contain occasional inconsistencies and errors.
Tags can only communicate data with so much precision, and they may run
out of battery or fail entirely. On top of this, many tags support
dynamic sampling settings, and can adapt their sampling rates in
response to external conditions, like battery charge, time of day, and
more.

All of these issues mean that the IMU data stored in a `move2` are not
always going to be entirely regular, particularly when it comes to the
timestamps that are recorded. Correctly identifying bursts of IMU data
thus requires diagnosing and correcting for any unexpected noise in the
input timestamps before analysis. move2imu provides several tools that
help facilitate this process.

## Data structures

Recall that when extracting IMU data from a `move2` object into an IMU
vector, move2imu supports two general data structures:

- In **compact data**, bursts are stored in a single row. IMU values are
  provided as a space-delimited string and sampling frequencies and axes
  are contained as separate metadata columns.

- In **expanded data**, each row represents an individual sample. Each
  sample’s timestamp is recorded explicitly. Sequences of timestamps are
  used to derive the burst boundaries and sampling frequencies.

In both of these cases, noise in the recorded timestamp data can affect
the IMU bursts that are identified in a `move2` object. In this
vignette, we walk through some common pre-processing strategies to
navigate these issues.

Before proceeding, we’ll load the libraries used in this vignette.

``` r

library(move2imu)
library(move2)
library(dplyr)
```

## Data order

First, we’ll load sample data from our package. The
[`gulls()`](https://move2universe.github.io/move2imu/reference/example_data.md)
data are stored in expanded format. These data are actually already
quite clean, so for the purposes of demonstration we’ll use a subset of
the data and add some fake timestamps:

``` r

gul <- gulls()

# Select first 21 rows where ACC data are present
gul <- gul[acc_sample_rows(gul), ][1:21, ]

# Artificially modify the timestamps for this vignette
ts <- c(
  rep(29, 2), 
  cumsum(c(0, rep(.05, 5), .0525, 0.0475, rep(.05, 4))), 
  seq(30, 35),
  60
)

gul$timestamp <- as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")
```

To be able to identify burst start and endpoints,
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md)
(and other `as_*()` functions) requires that the input `move2` is
ordered by timestamp within each track ID. Duplicate timestamps within
the same track must also be resolved before parsing IMU data.

If we try to build bursts from unordered data, we get an error:

``` r

as_acc(gul)
#> Error in `as_imu_move2_()`:
#> ! Timestamps must be strictly increasing within each track.
#> ℹ Order data by track and time and remove duplicate timestamps. See
#>   `move2::mt_filter_unique()`.
```

We can use move2 to order our data and to filter out any duplicate
records. Here, we use move2 to sort on track ID and time and then select
the first record in all cases where timestamps are duplicated:

``` r

# Order by track ID and timestamp
gul <- gul[order(mt_track_id(gul), mt_time(gul)), ]

# Select a single record for any duplicated timestamps
gul <- move2::mt_filter_unique(gul, criterion = "first")
```

Now, it’s possible to build our IMU bursts:

``` r

a <- as_acc(gul, drop = TRUE)

a
#> <acceleration[5]>
#> [1] (7 226 1905.5)          (-144 472 1972)         (-235 484 2101)        
#> [4] (-80.14 282.86 1910.29) (-1 332 1675)          
#> # frequency:  1 [Hz] - 20 [Hz]
```

## Addressing parsing issues in expanded data

From the output, we can see that we have multiple sampling frequencies
represented, so we know that the data are not fully uniform.

``` r

is_uniform(a)
#> [1] FALSE
```

This in and of itself is not necessarily a problem, and indeed may be
expected if the tag sampling settings were modified during the course of
a study. Still, it’s worth a look to make sure that the bursts were
parsed as expected, as small discrepancies in the recorded timestamps
can create bursts that don’t correspond directly to what you might
expect given the tag settings.

We can get an overview of the burst properties with
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r

summary(a)
#> 5 acc bursts
#> from 1970-01-01 to 1970-01-01 00:01:00 UTC 
#> 
#> Axes: XYZ (5) 
#> Frequencies: 1 -- 20 [Hz] 
#> Samples per burst: 1 -- 7 
#> Durations: 0.25 -- 7 [s] 
#> Intervals: [ 0 / 12 / 24 / 26.2 / 28.4 ] [s]  (min/Q1/med/Q3/max) 
#> 
#> Values:  [ -288 / -2.5 / 308 / 1878.25 / 2150 ]  (min/Q1/med/Q3/max) 
#> Units:   NULL
```

As we can see, we have bursts that span from a 1Hz to a 20Hz sampling
frequency, and some bursts only have 1 sample. Further, the durations of
the bursts span from 0.25 seconds at a minimum to a maximum of 7
seconds.

To explore in more depth, we can look at the frequency and sample
numbers themselves:

``` r

tibble::tibble(
  f = freqs(a),
  n = n_samples(a)
)
#> # A tibble: 5 × 2
#>      f     n
#>   [Hz] <int>
#> 1   20     6
#> 2   NA     1
#> 3   20     5
#> 4    1     7
#> 5   NA     1
```

Here we see two bursts at 20Hz, 1 burst of 7 samples at 1Hz, and two
bursts with only a single sample and no frequency. To understand where
this output might be coming from, we first need to describe how bursts
are identified in expanded-format data.

### How expanded-format data are parsed

In expanded-format data, the sampling frequency of the sensor is not
explicitly recorded. Instead, each sample is recorded with its own
individual timestamp. The sampling frequency thus has to be inferred
from the sequence of sample timestamps.

To do so,
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md)
proceeds through the samples chronologically and measures the time
interval between each pair. In this case, we have:

``` r

gaps <- diff(mt_time(gul))
gaps
#> Time differences in secs
#>  [1]  0.0500  0.0500  0.0500  0.0500  0.0500  0.0525  0.0475  0.0500  0.0500
#> [10]  0.0500  0.0500 28.4500  1.0000  1.0000  1.0000  1.0000  1.0000  1.0000
#> [19] 25.0000
```

Each interval represents a local estimate of the sampling frequency. For
instance, a signal collected at 20Hz would produce intervals of 0.05
seconds. When a sequence of intervals are all identical, the samples are
grouped into a single burst with a frequency derived from that interval.
Whenever the time interval between samples changes, a new burst is
initiated.

The problem is that without an explicitly-recorded sampling frequency,
it is impossible to distinguish a change in sampling frequency from
noise in the recorded timestamps: either will produce irregular sampling
intervals.

### Adding tolerance when parsing bursts

Looking again at the gaps between the recorded timestamps, we can see
that there is a small discrepancy that occurs between position 5 and 8.

``` r

gaps[5:8]
#> Time differences in secs
#> [1] 0.0500 0.0525 0.0475 0.0500
```

Up to this point, the timestamps had been arriving regularly every 50
milliseconds (ms), or at 20Hz. However, the 7th timestamp comes slightly
late: 52.5ms elapse before it arrives, and the subsequent timestamp thus
comes relatively quickly (47.5ms) as the sampling frequency
reestablishes itself.

This local sampling frequency change initiates a new burst starting with
the 7th timestamp. However, since this perturbation is quite small and
the 20Hz sampling frequency starts again immediately afterward, it seems
more likely that this individual timestamp was simply recorded late
rather than representing a new collection frequency.

We can smooth over these kinds of issues with the `freq_tol` argument to
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md).
`freq_tol` allows you to specify a tolerance around the sampling
frequency, preventing slight deviations in the sampling frequency from
triggering a new burst.

`freq_tol` is defined relative to the running frequency. That is, two
frequencies will be considered to belong to the same burst when the
faster is at most `(1 + freq_tol)` times the slower. In our case, the
largest frequency gap occurs in the deviation between the 7th and 8th
timestamp. Here, the difference in the sampling periods (0.005, or 5ms)
is roughly 10.5% of the local sampling period (0.0475).

Thus, setting `freq_tol = 0.11` should allow this discrepancy to be
absorbed into the surrounding burst:

``` r

a <- as_acc(gul, freq_tol = 0.11, drop = TRUE)

a
#> <acceleration[3]>
#> [1] (-106.42 354 1992.5)    (-80.14 282.86 1910.29) (-1 332 1675)          
#> # frequency:  1 [Hz] - 20 [Hz]
```

This has collapsed what was previously five bursts into three distinct
bursts, with only one burst at each sampling frequency:

``` r

freqs(a)
#> Units: [Hz]
#> [1] 20  1 NA

n_samples(a)
#> [1] 12  7  1
```

Note that the second burst, recorded at 1Hz, does not get subsumed into
the first, as the sampling frequencies are much more than 10.5%
different. However, at very extreme values of freq_tol, you *could* in
theory force these signals to merge as well:

``` r

# Don't do this!
as_acc(gul, freq_tol = 600, drop = TRUE)
#> <acceleration[1]>
#> [1] (-91.95 328 1947.85)
#> # frequency: 0.316667 [Hz]
```

As you can see, this produces a completely spurious burst sampling
frequency of 0.317Hz, as the tolerance blends signals from the two
frequencies together, masking the gap between them. Essentially, we’ve
spread out the time gap between the two bursts across all of the samples
in this single burst, which drastically distorts the true sampling rate.
Thus, you should strive to keep the `freq_tol` as low as possible while
still accommodating small deviations in your data. If you have larger
deviations, you may want to consider analyzing them as separate bursts,
or manually updating timestamps to avoid unexpected behavior of
`freq_tol` elsewhere in your data.

### Isolated IMU samples

You’ll notice above that we still have one irregular “burst” of data
that contains only one sample and no frequency. This stems from the fact
that a sampling frequency can’t be derived from only one sample, as no
sampling interval exists to imply a certain frequency.

These single-sample bursts can emerge because of timestamp noise, in
which case they may be able to be incorporated into adjacent bursts by
adapting the `freq_tol`.

Some tags also report single samples of IMU data alongside GPS
recordings, which are typically reported at much slower frequencies. In
these cases, you may not even want to treat the IMU data as belonging to
the same burst. You can set a minimum allowable frequency when parsing
expanded-format data using the `min_freq` argument. This prevents
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md)
from building bursts from samples collected below a given frequency,
even if the sampling intervals are regular.

This may be useful if you prefer to treat each sample in isolation
alongside its corresponding GPS fix.

For instance, if we set `min_freq = 2`, we would explode the 1Hz burst
from above into individual samples:

``` r

as_acc(gul, freq_tol = 0.11, min_freq = 2, drop = TRUE)
#> <acceleration[9]>
#> [1] (-106.42 354 1992.5) (-215 331 2102)      (-78 228 1968)      
#> [4] (-23 212 1876)       (-4 222 1812)        (-25 250 1795)      
#> [7] (-72 325 1853)       (-144 412 1966)      (-1 332 1675)       
#> # frequency: 20 [Hz]
```

Note that individual samples will always have an `NA` frequency, which
will prevent them from merging back together (e.g. with
[`merge_imu()`](https://move2universe.github.io/move2imu/reference/merge_imu.md)),
so this tool is primarily useful for analyses that are coarse enough
that individual samples are sufficient.

## Addressing merging issues in compact data

With compact data, several samples are contained in a single row, and
the sampling frequency is recorded explicitly. This means that we don’t
need to identify burst boundaries based on the raw timestamps, as we do
with expanded-format data. However, timestamp noise can still produce
some irregularities when building bursts from compact data.

This time, we’ll load the albatross data from the package. Again, we’ll
need to artificially modify the file to introduce some noise, as the
data are otherwise quite clean.

``` r

alb <- albatrosses()

# Filter to just acc data for this demo
alb <- alb[!is.na(alb$eobs_accelerations_raw), ]

# Simulate jittered timestamps for demo
jitter <- rep(c(0, .02, .01, .03, .02), 9)
ts <- rep(c(0, 12, 24, 636, 648), 9)

alb$timestamp <- as.POSIXct(ts + jitter, origin = "1970-01-01", tz = "UTC")
```

As before, we’ll make sure our data are sorted and deduplicated before
proceeding:

``` r

alb <- alb[order(mt_track_id(alb), mt_time(alb)), ]
alb <- move2::mt_filter_unique(alb, criterion = "first")
```

Let’s go ahead and extract our acceleration data:

``` r

a <- as_acc(alb)

a
#> <acceleration[45]>
#>  [1] (1824.17 1913.83) (1904.3 1926.5)   (1823.27 1913.42) (1826.7 1915.7)  
#>  [5] (1719.07 1908.8)  (1943.47 2028.05) (1927.98 2013.72) (1926.7 2008.82) 
#>  [9] (1940.97 2023.93) (1940.02 2021.87) (1969.07 2044.2)  (1962.88 2033.35)
#> [13] (2062.08 2023.48) (2090.73 2025.83) (2083.92 2051.48) (1887.58 1927.13)
#> [17] (1846.38 1948.13) (1881.15 1938.03) (1841.58 2093.23) (1778.92 2129.55)
#> [21] (1812.17 1952.13) (1790.4 1927.4)   (1811.85 1923.43) (1804.47 1935.05)
#> [25] (1811.45 1937.57) (2012.53 2227.17) (1970.52 2238.62) (2130.75 2082.43)
#> [29] (1997.18 2074.33) (2049.4 2060.38)  (1975.93 1868.22) (1920.02 2128.58)
#> [33] (1879.35 1920.62) (1927.5 1941.72)  (1949.27 1962.57) (1936.87 1918.93)
#> [37] (1672.52 1865.77) (1942.15 1922.33) (2072.85 1952.1)  (1617.97 1944.63)
#> [41] (1845.87 1921.13) (1838.77 1929.6)  (1843.57 1929.52) (1852.25 1929.65)
#> [45] (1829.97 1932.28)
#> # frequency: 5 [Hz]
```

In this case, we appear to have quite regular burst properties:

``` r

is_uniform(a)
#> [1] TRUE

summary(a)
#> 45 acc bursts
#> from 1970-01-01 to 1970-01-01 00:10:48 UTC 
#> 
#> Axes: XY (45) 
#> Frequencies: 5 -- 5 [Hz] 
#> Samples per burst: 60 -- 60 
#> Durations: 12 -- 12 [s] 
#> Intervals: [ -660.02 / -0.01 / -0.01 / 0.02 / 600.02 ] [s]  (min/Q1/med/Q3/max) 
#> 
#> Values:  [ 1462 / 1875 / 1936 / 2010 / 2988 ]  (min/Q1/med/Q3/max) 
#> Units:   NULL
```

However, notice that our distribution of burst *intervals* has some very
small values hovering around 0.

Indeed, if we look at the gaps in the raw burst timestamps, we notice
that the bursts appear to be offset by roughly 12 seconds, with
occasional larger gaps. (We’ve already ordered our data, so the large
negative time gaps come at borders between individuals.)

``` r

diff(mt_time(alb))
#> Time differences in secs
#>  [1]   12.02   11.99  612.02   11.99 -648.02   12.02   11.99  612.02   11.99
#> [10] -648.02   12.02   11.99  612.02   11.99 -648.02   12.02   11.99  612.02
#> [19]   11.99 -648.02   12.02   11.99  612.02   11.99 -648.02   12.02   11.99
#> [28]  612.02   11.99 -648.02   12.02   11.99  612.02   11.99 -648.02   12.02
#> [37]   11.99  612.02   11.99 -648.02   12.02   11.99  612.02   11.99
```

If we look at the duration of our bursts, we notice that they’re all
also 12 seconds long:

``` r

burst_dur(a)
#> Units: [s]
#>  [1] 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12
#> [26] 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12
```

This means that in some cases each of our bursts lasts nearly as long as
the time gap between bursts.

This pattern typically indicates that the data were collected
*continuously*, despite being reported in individual “bursts.” This
stems from the fact that tags typically can only report so many bytes of
data in a single payload, so continuous data are provided in chunks.

Typically, adjacent bursts of continuous data for the same individual
are merged automatically (see the `merge_continuous` argument to
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md)).
In this case, however, the slight timestamp jitter at the burst
boundaries prevents the bursts from appearing adjacent to each other in
time.

### Adding tolerance when merging bursts

To address this issue, you can set the `gap_tol` parameter to relax the
requirement that an adjacent burst start immediately as the previous
burst ends. For instance, a `gap_tol` of 0.03 would allow any two bursts
separated by a gap of at most 0.03 seconds (and with the same sampling
frequency) to be merged.

``` r

a <- as_acc(alb, gap_tol = 0.03, drop = TRUE)
```

Now we can see that the remaining gaps are all around 600 seconds long,
and so clearly don’t belong to any adjacent bursts:

``` r

burst_intervals(a)
#> Units: [s]
#>  [1]        NA  600.0199 -660.0199  600.0199 -660.0199  600.0199 -660.0199
#>  [8]  600.0199 -660.0199  600.0199 -660.0199  600.0199 -660.0199  600.0199
#> [15] -660.0199  600.0199 -660.0199  600.0199
```

### Recalculating burst frequencies

Notice that the frequencies of the output have shifted slightly, so that
they no longer align with the 5Hz frequency recorded in the input data
source itself:

``` r

freqs(a)
#> Units: [Hz]
#>  [1] 4.9986 5.0021 4.9986 5.0021 4.9986 5.0021 4.9986 5.0021 4.9986 5.0021
#> [11] 4.9986 5.0021 4.9986 5.0021 4.9986 5.0021 4.9986 5.0021
```

When merging bursts, the burst frequency is *recalculated* after the
merge from the new number of samples and the elapsed time span of the
burst. When noise is present in the timestamps, the overall time span of
a merged burst thus incorporates this noise into its overall time span,
leading to a derived burst frequency that is slightly different from the
recorded frequency.

The recalculated frequency ensures that the elapsed time from the first
to last sample remains true to the actual time span of the merged
burst’s samples as recorded in the data, while also retaining the
assumed property that samples are uniformly distributed throughout the
burst.

If you instead want to treat the bursts as being recorded at the
original sampling frequency, you must manually override the frequency
values:

``` r

freqs(a) <- round(freqs(a))

freqs(a)
#> Units: [Hz]
#>  [1] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
```

Now, the burst frequencies align again with what was recorded in the
input data. Note that this means that the bursts are no longer true to
the recorded timestamps in the input `move2`, as the noise in the burst
gaps has been ignored.

In general, these discrepancies should be small and justifiably
ignorable. If larger gaps exist between your bursts, you may need to
treat them separately in your analysis or consult the manual for the
tags in question to determine where the timestamp noise originates from
and whether it is reasonable to ignore.

(If you want to ensure that you avoid recalculating frequencies and
instead work with continuous data in separate bursts, you can also set
`merge_continuous = FALSE` in
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md).)

Note as well that while we have now successfully parsed our data into
the correct bursts as described in the data, this does not imply that
the data are perfectly uniform:

``` r

is_uniform(a)
#> [1] FALSE
```

In this case, this stems from the fact that some of the merged bursts
have more samples than others.

``` r

unique(n_samples(a))
#> [1] 180 120
```

This is true to the input data, but may need to be resolved depending on
the analysis you intend to run.
