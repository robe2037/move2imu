test_that("create zero length", {
  expect_length(acc(), 0)
})

test_that("manipulation", {
  x <- acc_example()
  expect_identical(head(x, 1), x[1])
  expect_length(x[1], 1)
  expect_length(x[rep(1, 3)], 3)
})

test_that("logical", {
  expect_false(is_acc(NA))
  expect_false(is_acc(cbind(1, 1:3)))
  expect_true(is_acc(acc()))
  expect_true(is_acc(acc_example()))
  expect_true(is_acc(acc(list(NULL), frequency = NA)))
})

test_that("properties are correctly calculated", {
  xa <- acc(
    c(
      acc_burst_example(x = sin(1:30 / 10), y = cos(1:30 / 10), z = 1),
      acc_burst_example(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
    ),
    frequency = units::as_units(c(20, 30), "Hz"),
    start = .as.POSIXct(c(1, 2))
  )

  x <- c(xa, NA)
  expect_true(is_acc(x))
  expect_length(x, 3)
  expect_identical(is.na(x), c(F, F, T))
  expect_identical(n_axis(x), c(3L, 2L, NA))
  expect_identical(n_samples(x), c(30L, 20L, NA))
  expect_false(is_uniform(x))
  expect_true(is_uniform(x[c(1, 3)]))

  x2 <- vec_c(NA, xa)
  expect_true(is_acc(x2))
  expect_length(x2, 3)
  expect_identical(is.na(x2), c(T, F, F))
  expect_identical(n_axis(x2), c(NA, 3L, 2L))
  expect_identical(n_samples(x2), c(NA, 30L, 20L))
  expect_false(is_uniform(x2))
  expect_true(is_uniform(x2[c(1, 3)]))
})

test_that("is_uniform checks burst units strictly", {
  burst_unit <- function(vals, unit) {
    new_burst_list(
      list(units::set_units(cbind(X = vals), unit, mode = "standard")),
      "acc"
    )
  }

  same_units <- acc(
    c(burst_unit(1:5, "m/s^2"), burst_unit(6:10, "m/s^2")),
    frequency = units::as_units(c(10, 10), "Hz")
  )
  expect_true(is_uniform(same_units))

  mixed_units <- acc(
    c(burst_unit(1:5, "m/s^2"), burst_unit(6:10, "g")),
    frequency = units::as_units(c(10, 10), "Hz")
  )
  expect_false(is_uniform(mixed_units))

  mixed_class <- acc(
    c(burst_unit(1:5, "m/s^2"), acc_burst_example(6:10)),
    frequency = units::as_units(c(10, 10), "Hz")
  )
  expect_false(is_uniform(mixed_class))
})

test_that("constructor replaces metadata with NA when bursts are missing", {
  a <- acc(
    list(NULL),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(0)
  )
  expect_true(is.na(a))
  expect_true(vctrs::vec_detect_missing(a))
  expect_true(is.na(freqs(a)))
  expect_true(is.na(starts(a)))
})

test_that("frequency must be in frequency-compatible units", {
  expect_error(
    acc(list(cbind(X = 1:3)), frequency = units::set_units(10, "km")),
    "frequency unit"
  )

  # Valid frequency units are accepted and converted to Hz
  expect_no_error(
    a <- acc(list(cbind(X = 1:3)), frequency = units::set_units(10, "kHz"))
  )
  expect_equal(units::deparse_unit(freqs(a)), "Hz")
  expect_equal(as.numeric(freqs(a)), 10000)

  # Bare numeric is coerced to Hz
  a <- acc(list(cbind(X = 1:3)), frequency = 10)
  expect_equal(units::deparse_unit(freqs(a)), "Hz")
})

test_that("`freqs<-` coerces the replacement to Hz", {
  a <- acc(list(cbind(X = 1:3)), frequency = units::set_units(20, "Hz"))

  # Compatible frequency units are converted to Hz
  freqs(a) <- units::set_units(1, "kHz")
  expect_equal(units::deparse_unit(freqs(a)), "Hz")
  expect_equal(as.numeric(freqs(a)), 1000)

  # Bare numeric is assumed to be Hz
  freqs(a) <- 50
  expect_equal(units::deparse_unit(freqs(a)), "Hz")
  expect_equal(as.numeric(freqs(a)), 50)

  # Non-frequency units are rejected
  expect_error(
    freqs(a) <- units::set_units(1, "km"),
    "frequency unit"
  )
})

test_that("bursts must be numeric matrices with unique X, Y, or Z columns", {
  # Unnamed columns
  expect_error(
    acc(list(matrix(1:6, ncol = 2)), frequency = 10),
    "named"
  )

  # A mix of named and unnamed bursts should also error
  expect_error(
    acc(list(cbind(X = 1:3, Y = 4:6), matrix(1:6, ncol = 2)), frequency = 10),
    "named"
  )

  # Invalid column names
  expect_error(
    acc(list(cbind(A = 1:3, B = 4:6)), frequency = 10),
    "named"
  )

  # Duplicated column names
  expect_error(
    acc(list(cbind(X = 1:3, X = 4:6)), frequency = 10),
    "Bursts must be"
  )

  # Bursts must be 2-dimensional and numeric
  arr <- array(1:12, c(2, 3, 2), dimnames = list(NULL, c("X", "Y", "Z"), NULL))

  expect_error(
    acc(list(arr), frequency = 10),
    "Bursts must be"
  )
  expect_error(
    acc(list(cbind(X = c("a", "b"))), frequency = 10),
    "Bursts must be"
  )

  # `units` bursts are still numeric
  units_burst <- units::set_units(cbind(X = 1:3), "m/s^2", mode = "standard")

  expect_no_error(
    acc(list(units_burst), frequency = 10)
  )

  # NULL bursts (NA entries) don't need column names
  expect_no_error(
    acc(list(NULL), frequency = NA)
  )
})

test_that("c() normalizes different frequency units to Hz", {
  a1 <- acc(
    acc_burst_example(1:10, 1:10),
    frequency = units::set_units(20, "kHz"),
    start = .as.POSIXct(0)
  )
  a2 <- acc(
    acc_burst_example(1:10, 1:10),
    frequency = units::set_units(20, "Hz"),
    start = .as.POSIXct(1)
  )

  a <- c(a1, a2)
  expect_length(a, 2)

  # Frequency is always stored in Hz, so combined vectors share the unit
  expect_identical(units::deparse_unit(freqs(a)), "Hz")

  # Each input is converted to Hz (20 kHz -> 20000 Hz; 20 Hz -> 20 Hz)
  expect_equal(as.numeric(freqs(a)[1]), 20000)
  expect_equal(as.numeric(freqs(a)[2]), 20)

  # NA frequencies are preserved
  a_na <- acc(list(NULL), frequency = units::set_units(NA, "Hz"))
  a <- c(a1, a_na)
  expect_length(a, 2)
  expect_equal(as.numeric(freqs(a)[1]), 20000)
  expect_true(is.na(freqs(a)[2]))

  # Combining with empty acc
  a <- c(a1, acc())
  expect_length(a, 1)
  expect_equal(as.numeric(freqs(a)[1]), 20000)

  # Three-way combine with mixed units, all normalized to Hz
  a3 <- acc(
    acc_burst_example(1:10, 1:10),
    frequency = units::set_units(0.001, "MHz"),
    start = .as.POSIXct(2)
  )
  a <- c(a1, a2, a3)
  expect_length(a, 3)
  expect_equal(as.numeric(freqs(a)), c(20000, 20, 1000))
  expect_identical(units::deparse_unit(freqs(a)), "Hz")
})

test_that("c() preserves non-UTC timezone", {
  tz <- "America/New_York"
  a1 <- acc(
    acc_burst_example(1:10),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(1730610000, tz)
  )
  a2 <- acc(
    acc_burst_example(11:20),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(1730610010, tz)
  )

  a <- c(a1, a2)
  expect_identical(attr(starts(a), "tzone"), tz)
  expect_identical(starts(a), c(starts(a1), starts(a2)))
})

test_that("duration is correctly calculated", {
  a <- acc(
    c(
      acc_burst_example(x = sin(1:30 / 10), y = cos(1:30 / 10), z = 1),
      acc_burst_example(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
    ),
    frequency = units::as_units(c(20, 30), "Hz"),
    start = .as.POSIXct(c(1, 2))
  )

  b <- bursts(a)
  f <- freqs(a)

  d <- burst_dur(a)

  expect_equal(d[[1]], units::set_units(nrow(b[[1]]) / f[[1]], "s"))
  expect_equal(d[[1]], units::set_units(1.5, "s"))
  expect_equal(d[[2]], units::set_units(nrow(b[[2]]) / f[[2]], "s"))
  expect_equal(d[[2]], units::set_units(2 / 3, "s"))

  expect_equal(as.numeric(burst_dur(acc(acc_burst_example(1, 1), 20))), 0.05)
  expect_true(is.na(burst_dur(acc(acc_burst_example(1, 1), NA))))
})

test_that("burst_dur converts non-Hz frequencies to seconds", {
  # 30 samples at 60/min (= 1 Hz)
  a <- acc(
    acc_burst_example(1:30),
    frequency = units::set_units(60, "1/min")
  )
  expect_equal(burst_dur(a), units::set_units(30, "s"))
})

test_that("burst_intervals measures gaps and start-to-start", {
  b <- acc_burst_example(1:20, 1:20, 1:20)

  a <- acc(
    rep(b, 3),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120)
  )

  expect_equal(
    burst_intervals(a, from = "start"),
    units::set_units(c(NA, 60, 60), "s")
  )

  expect_equal(
    burst_intervals(a),
    units::set_units(c(NA, 59, 59), "s")
  )
})

test_that("burst_intervals is NA where a neighbouring start is missing", {
  b <- acc_burst_example(1:20, 1:20, 1:20)

  a <- acc(
    rep(b, 4),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-01-01 00:02:00", "2020-01-01 00:03:00"), tz = "UTC")
  )

  expect_equal(
    burst_intervals(a, from = "start"),
    units::set_units(c(NA, NA, NA, 60), "s")
  )
  expect_equal(
    burst_intervals(a, from = "end"),
    units::set_units(c(NA, NA, NA, 59), "s")
  )
})

test_that("burst_intervals does not measure across ID boundaries", {
  b <- acc_burst_example(1:20, 1:20, 1:20)

  a <- acc(
    rep(b, 4),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120, 180)
  )

  # First burst of each group has no preceding burst within the group -> NA
  expect_equal(
    burst_intervals(a, from = "start", ids = c("a", "a", "b", "b")),
    units::set_units(c(NA, 60, NA, 60), "s")
  )

  expect_error(
    burst_intervals(a, ids = c("a", "b")),
    "same length"
  )
})

test_that("burst_intervals does not bridge across an interleaved group", {
  b <- acc_burst_example(1:20, 1:20, 1:20)

  a <- acc(
    rep(b, 3),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120)
  )

  # Intervals are measured against the immediate neighbour in vector order,
  # never the nearest same-group burst, so the third "a" is not measured
  # across the intervening "b".
  expect_equal(
    burst_intervals(a, from = "start", ids = c("a", "b", "a")),
    units::set_units(c(NA, NA, NA), "s")
  )
})

test_that("burst_intervals skips missing bursts", {
  m <- cbind(X = 1:20, Y = 1:20, Z = 1:20)

  # Position 2 is a missing burst; the constructor forces its start to NA.
  a <- acc(
    list(m, NULL, m, m),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120, 180)
  )

  # The interval bridges the missing burst rather than being masked by it; the
  # missing burst itself stays NA. Bursts are 20 samples at 20 Hz -> 1 s each.
  expect_equal(
    burst_intervals(a, from = "start"),
    units::set_units(c(NA, NA, 120, 60), "s")
  )
  expect_equal(
    burst_intervals(a, from = "end"),
    units::set_units(c(NA, NA, 119, 59), "s")
  )
})

test_that("burst_intervals does not skip bursts with missing metadata", {
  m <- cbind(X = 1:20, Y = 1:20, Z = 1:20)
  
  # Burst with one entry with no frequency
  a <- acc(
    list(m, m, m),
    frequency = units::as_units(c(20, NA, 20), "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120)
  )
  
  expect_equal(
    burst_intervals(a, from = "start"),
    units::set_units(c(NA, 60, 60), "s")
  )
  expect_equal(
    burst_intervals(a, from = "end"),
    units::set_units(c(NA, 59, NA), "s")
  )
  
  # Burst with one entry with no start
  a <- acc(
    list(m, m, m, m),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct(
      c("2020-01-01 00:00:00", NA, "2020-01-01 00:02:00", "2020-01-01 00:03:00"),
      tz = "UTC"
    )
  )
  
  expect_equal(
    burst_intervals(a, from = "start"),
    units::set_units(c(NA, NA, NA, 60), "s")
  )
})

test_that("burst_intervals respects ID boundaries around missing bursts", {
  m <- cbind(X = 1:20, Y = 1:20, Z = 1:20)

  a <- acc(
    list(m, NULL, m, m),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120, 180)
  )

  # Missing burst in idx 2 is skipped, but idx 3 is still computed
  # as they belong to the same group
  expect_equal(
    burst_intervals(a, from = "start", ids = c("a", "a", "a", "b")),
    units::set_units(c(NA, NA, 120, NA), "s")
  )
})

test_that("imu_units are safely extracted", {
  a <- acc_example()

  # Unitless bursts return NA
  expect_identical(imu_units(a), c(NA_character_, NA_character_))

  # Units bursts return unit string
  a_u <- set_imu_units(a, "m/s^2")
  expect_identical(imu_units(a_u), c("m/s^2", "m/s^2"))

  # NA acc elements return NA
  a_na <- c(a_u[1], acc(list(NULL), units::set_units(NA, "Hz")), a_u[2])
  expect_identical(imu_units(a_na), c("m/s^2", NA_character_, "m/s^2"))

  # Mixed units are reported per element
  a_mixed <- c(
    set_imu_units(a[1], "m/s^2"),
    set_imu_units(a[2], "standard_free_fall")
  )
  expect_identical(imu_units(a_mixed), c("m/s^2", "standard_free_fall"))
})

test_that("`start` accepts anything that denotes an instant", {
  b <- list(cbind(X = 1:10))
  f <- units::set_units(10, "Hz")

  # POSIXct keeps its own zone
  expect_identical(starts(acc(b, f, start = .as.POSIXct(0, "CET"))), .as.POSIXct(0, "CET"))

  # POSIXlt records the same instant and zone, and is stored as POSIXct
  lt <- as.POSIXlt(.as.POSIXct(0, "CET"))
  expect_identical(starts(acc(b, f, start = lt)), as.POSIXct(lt))

  # Date is midnight UTC, not a count of seconds
  expect_identical(
    starts(acc(b, f, start = as.Date("2020-01-01"))),
    as.POSIXct("2020-01-01", tz = "UTC")
  )

  # numeric is seconds since the epoch, UTC -- the same rule move2 uses
  expect_identical(starts(acc(b, f, start = 0)), .as.POSIXct(0))
  expect_identical(starts(acc(b, f, start = 10L)), .as.POSIXct(10))

  # `NULL` (unknown) and a bare `NA` both mean "no start time"
  expect_identical(starts(acc(b, f)), .as.POSIXct(NA_real_))
  expect_identical(starts(acc(b, f, start = NA)), .as.POSIXct(NA_real_))

  # The same rule holds for the other sensors
  expect_identical(starts(mag(b, f, start = 0)), .as.POSIXct(0))
  expect_identical(starts(gyro(b, f, start = as.Date("2020-01-01"))),
                   as.POSIXct("2020-01-01", tz = "UTC"))
})

test_that("`start` rejects anything that would have to be guessed at", {
  b <- list(cbind(X = 1:10))
  f <- units::set_units(10, "Hz")

  # Parsing a string needs a format and a zone, and `as.POSIXct()` would read
  # it in local time
  expect_error(acc(b, f, start = "2020-01-01"), "must be a timestamp")

  # Durations are not instants, and their bare numeric drops the unit
  expect_error(acc(b, f, start = as.difftime(1, units = "hours")), "must be a timestamp")
  expect_error(acc(b, f, start = units::set_units(1, "h")), "must be a timestamp")

  # The error names the user's argument
  expect_error(acc(b, f, start = "2020-01-01"), "`start`")
})

test_that("`starts<-` accepts the same set as the constructors", {
  a <- acc_example()

  starts(a) <- c(0, 10)
  expect_identical(starts(a), .as.POSIXct(c(0, 10)))

  starts(a) <- as.Date(c("2020-01-01", "2020-01-02"))
  expect_identical(starts(a), as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"))

  starts(a) <- .as.POSIXct(c(5, 15), "CET")
  expect_identical(starts(a), .as.POSIXct(c(5, 15), "CET"))

  expect_error(starts(a) <- c("a", "b"), "must be a timestamp")
  expect_error(starts(a) <- c("a", "b"), "`value`")
})
