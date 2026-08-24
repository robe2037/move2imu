test_that("summary returns imu_summary object", {
  s <- summary(acc_example())
  expect_s3_class(s, "imu_summary")
  expect_equal(s$n, 2)
  expect_equal(s$n_na, 0)
})

test_that("summary prints header with burst count and NAs", {
  out <- capture.output(print(summary(c(acc_example(), NA))))
  expect_true(any(grepl("3 acc bursts", out)))
  expect_true(any(grepl("1 NA", out)))
})

test_that("summary prints axis combinations inline", {
  out <- capture.output(print(summary(acc_example())))
  axes_line <- out[grepl("Axes:", out)]
  expect_length(axes_line, 1)
  expect_match(axes_line, "XY")
  expect_match(axes_line, "XYZ")
})

test_that("summary handles empty acc", {
  s <- summary(acc())
  expect_s3_class(s, "imu_summary")
  expect_equal(s$n, 0)
  out <- capture.output(print(s))
  expect_true(any(grepl("0 acc bursts", out)))
})

test_that("summary shows units when present", {
  out <- capture.output(print(summary(units::set_units(acc_example(), "m/s^2"))))
  expect_true(any(grepl("Units:.*m/s\\^2", out)))
})

test_that("summary shows no units when bursts are unitless", {
  out <- capture.output(print(summary(acc_example())))
  expect_true(any(grepl("Units:.*NULL", out)))
})

test_that("summary shows ranges and quantiles", {
  s <- summary(acc_example())
  expect_equal(s$freqs_rng, c(20, 20))
  expect_equal(s$freq_unit, "Hz")
  expect_equal(s$samples_rng, c(20, 30))
  expect_equal(s$durations_rng, c(1, 1.5))
  expect_equal(s$dur_unit, "s")
  expect_length(s$intervals_q, 5)
  expect_length(s$values_q, 5)
})

test_that("summary excludes NA frequencies from the frequency range", {
  a <- acc(
    list(
      cbind(X = 1, Y = 2, Z = 3),
      cbind(X = 4, Y = 5, Z = 6),
      cbind(X = 7, Y = 8, Z = 9)
    ),
    frequency = units::set_units(c(10, NA, 20), "Hz"),
    start = .as.POSIXct(c(0, 10, 20))
  )
  s <- summary(a)
  expect_equal(s$freqs_rng, c(10, 20))

  out <- capture.output(print(s))
  freq_line <- out[grepl("^Frequencies:", out)]
  expect_match(freq_line, "10 -- 20 [Hz]", fixed = TRUE)
  expect_false(grepl("NA", freq_line))
})

test_that("summary shows NA when every frequency is NA", {
  a <- acc(
    list(cbind(X = 1, Y = 2, Z = 3), cbind(X = 4, Y = 5, Z = 6)),
    frequency = units::set_units(c(NA, NA), "Hz"),
    start = .as.POSIXct(c(0, 10))
  )
  s <- summary(a)
  expect_null(s$freqs_rng)

  out <- capture.output(print(s))
  freq_line <- out[grepl("^Frequencies:", out)]
  expect_match(freq_line, "NA")
})

# Single-burst: always-present empty intervals --------------------------------

single_burst_acc <- function() {
  acc(
    list(cbind(X = 1.2, Y = 0.3, Z = 9.8)),
    frequency = units::set_units(20, "Hz"),
    start = as.POSIXct("2026-01-01", tz = "UTC")
  )
}

test_that("single-burst summary has no interval quantiles", {
  s <- summary(single_burst_acc())
  expect_null(s$intervals_q)
})

test_that("single-burst summary prints NA for intervals", {
  out <- capture.output(print(summary(single_burst_acc())))
  intervals_line <- out[grepl("^Intervals:", out)]
  expect_length(intervals_line, 1)
  expect_match(intervals_line, "NA")
})

# Mixed-unit path -------------------------------------------------------------

mixed_unit_acc <- function() {
  b1 <- cbind(X = c(1, 2, 3))
  units(b1) <- units::as_units("m/s^2")
  b2 <- cbind(X = c(2000, 2010, 2020)) # unitless
  acc(
    list(b1, b2),
    frequency = units::set_units(c(20, 20), "Hz"),
    start = as.POSIXct(
      c(
        "2026-01-01 00:00:00",
        "2026-01-01 00:00:10"
      ),
      tz = "UTC"
    )
  )
}

test_that("summary lists multiple unit groups in Units footer when mixed", {
  out <- capture.output(print(summary(mixed_unit_acc())))
  units_line <- out[grepl("^Units:", out)]
  expect_length(units_line, 1)
  expect_match(units_line, "m/s\\^2")
  expect_match(units_line, "NULL")
})

# format_range / format_quantiles helpers ------------------------------------

test_that("format_range renders", {
  expect_equal(format_range(c(1, 5), "s"), "1 -- 5 [s]")
  expect_equal(format_range(c(1, 5)), "1 -- 5")
})

test_that("format_range renders empty input as NA", {
  expect_equal(format_range(NULL), "NA")
  expect_equal(format_range(numeric(0), "s"), "NA")
})

test_that("format_quantiles renders", {
  with_unit <- format_quantiles(c(1, 2, 3, 4, 5), "s")
  expect_match(with_unit, "[ 1 / 2 / 3 / 4 / 5 ]", fixed = TRUE)
  expect_match(with_unit, "[s]", fixed = TRUE)
  expect_match(with_unit, "(min/Q1/med/Q3/max)", fixed = TRUE)

  no_unit <- format_quantiles(c(1, 2, 3, 4, 5))
  expect_match(no_unit, "[ 1 / 2 / 3 / 4 / 5 ]", fixed = TRUE)
  expect_false(grepl("[s]", no_unit, fixed = TRUE))
})

test_that("format_quantiles renders empty input as NA", {
  expect_equal(format_quantiles(NULL), "NA")
  expect_equal(format_quantiles(NULL, "s"), "NA")
  expect_equal(format_quantiles(numeric(0)), "NA")
  expect_equal(format_quantiles(numeric(0), "s"), "NA")
})

test_that(".range / .quantile precompute summaries or NULL", {
  expect_equal(.range(c(3, 1, 2)), c(1, 3))
  expect_null(.range(numeric(0)))

  # NAs are excluded rather than propagated into the range
  expect_equal(.range(c(3, NA, 1, 2)), c(1, 3))
  expect_null(.range(c(NA_real_, NA_real_)))

  expect_equal(.quantile(0:4), c(0, 1, 2, 3, 4))
  expect_equal(.quantile(c(0:4, NA)), c(0, 1, 2, 3, 4))
  expect_null(.quantile(numeric(0)))
  expect_null(.quantile(c(NA_real_, NA_real_)))
})

# Other sensor types ---------------------------------------------------------

test_that("summary dispatches and prints correctly for mag and gyro", {
  m <- mag(
    list(cbind(X = 1, Y = 2, Z = 3)),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct("2026-01-01", tz = "UTC")
  )
  s_m <- summary(m)
  expect_s3_class(s_m, "imu_summary")
  expect_equal(s_m$sensor, "mag")
  expect_match(capture.output(print(s_m))[1], "mag bursts")

  g <- gyro(
    list(cbind(X = 1, Y = 2, Z = 3)),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct("2026-01-01", tz = "UTC")
  )
  s_g <- summary(g)
  expect_s3_class(s_g, "imu_summary")
  expect_equal(s_g$sensor, "gyro")
  expect_match(capture.output(print(s_g))[1], "gyro bursts")
})
