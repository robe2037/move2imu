# Access and modify fields of an IMU vector

Access or update the underlying burst matrices, sampling frequencies, or
start times for each burst in an IMU vector.

## Usage

``` r
bursts(x)

bursts(x) <- value

freqs(x)

freqs(x) <- value

starts(x)

starts(x) <- value
```

## Arguments

- x:

  An IMU vector (`acc`, `mag`, or `gyro`)

- value:

  Replacement value.

## Value

For accessors, the corresponding field of `x`. For setters, `x` with the
updated value in the indicated field.

## Details

Frequencies assigned with `freqs<-` are converted to Hz if they are
provided with compatible units attached. If no units are provided, the
values are assumed to be in Hz already.

Start times assigned with `starts<-` accept `POSIXct`, `POSIXlt`,
`Date`, or a number of seconds since `1970-01-01 00:00:00 UTC`. Start
times are converted and stored as `POSIXct`. `Date` objects are treated
as being recorded at midnight, UTC.

## Examples

``` r
x <- acc(
  bursts = list(
    cbind(X = sin(1:20 / 10), Y = cos(1:20 / 10), Z = 1)
  ),
  frequency = units::as_units(20, "Hz"),
  start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
)

bursts(x)
#> <acc_list[1]>
#> [[1]]
#>                X           Y Z
#>  [1,] 0.09983342  0.99500417 1
#>  [2,] 0.19866933  0.98006658 1
#>  [3,] 0.29552021  0.95533649 1
#>  [4,] 0.38941834  0.92106099 1
#>  [5,] 0.47942554  0.87758256 1
#>  [6,] 0.56464247  0.82533561 1
#>  [7,] 0.64421769  0.76484219 1
#>  [8,] 0.71735609  0.69670671 1
#>  [9,] 0.78332691  0.62160997 1
#> [10,] 0.84147098  0.54030231 1
#> [11,] 0.89120736  0.45359612 1
#> [12,] 0.93203909  0.36235775 1
#> [13,] 0.96355819  0.26749883 1
#> [14,] 0.98544973  0.16996714 1
#> [15,] 0.99749499  0.07073720 1
#> [16,] 0.99957360 -0.02919952 1
#> [17,] 0.99166481 -0.12884449 1
#> [18,] 0.97384763 -0.22720209 1
#> [19,] 0.94630009 -0.32328957 1
#> [20,] 0.90929743 -0.41614684 1
#> 

freqs(x)
#> 20 [Hz]

starts(x)
#> [1] "2020-01-01 UTC"

freqs(x) <- units::as_units(25, "Hz")
freqs(x)
#> 25 [Hz]
```
