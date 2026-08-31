skip_if_not_installed("dygraphs")

test_that("plot_imu_trace", {
  expect_silent(
    graph <- plot_imu_trace(acc_example())
  )
  expect_s3_class(graph, "dygraphs")
})

test_that("plot_imu_trace handles missing start times", {
  a <- acc_example()
  starts(a) <- .as.POSIXct(c(1, NA))

  expect_warning(
    g <- plot_imu_trace(a),
    "Omitting 1 burst.* no start timestamp"
  )
  expect_s3_class(g, "dygraphs")

  # All starts NA: errors.
  starts(a) <- .as.POSIXct(c(NA, NA))
  expect_error(plot_imu_trace(a), "start timestamps and sampling frequencies")
})

test_that("plot_imu_trace omits bursts with a missing frequency", {
  a <- acc_example()
  # Second burst loses its frequency (as a single-sample burst would)
  freqs(a) <- units::set_units(c(20, NA), "Hz")

  expect_warning(
    g <- plot_imu_trace(a),
    "Omitting 1 burst.* no sampling frequency"
  )
  expect_s3_class(g, "dygraphs")

  # All frequencies missing: errors rather than producing NA timestamps
  freqs(a) <- units::set_units(c(NA, NA), "Hz")
  expect_error(plot_imu_trace(a), "start timestamps and sampling frequencies")
})

test_that("plot_imu_trace uses seconds regardless of frequency unit", {
  # Equivalent frequencies: 20 Hz and 1200/min.
  burst <- matrix(seq_len(20), ncol = 1, dimnames = list(NULL, "X"))
  start <- as.POSIXct("2026-01-01", tz = "UTC")

  a_hz <- acc(list(burst), units::set_units(20, "Hz"), start = start)
  a_min <- acc(list(burst), units::set_units(1200, "1/min"), start = start)

  g_hz <- plot_imu_trace(a_hz)
  g_min <- plot_imu_trace(a_min)

  # At 20Hz, 21st sample starts a 1 second. Confirm offsets were added correctly
  expect_identical(
    g_hz$x$data[[1]][c(1, 21)],
    c("2026-01-01T00:00:00.000Z", "2026-01-01T00:00:01.000Z")
  )

  # The dygraph time series should be identical — i.e. the dt offsets
  # were correctly normalized to seconds before adding to the start time.
  expect_equal(g_min$x$data[[1]], g_hz$x$data[[1]])
})

test_that("plot_imu_trace unions the axes of bursts with different columns", {
  # Bursts need not share axes. The plotted frame is the union of their
  # columns, with the missing axis filled in as NA for the shorter burst.
  a <- acc(
    list(cbind(X = 1:5, Y = 1:5, Z = 1:5), cbind(X = 6:10, Z = 6:10)),
    frequency = units::set_units(c(10, 10), "Hz"),
    start = .as.POSIXct(c(0, 10))
  )

  g <- plot_imu_trace(a)

  expect_identical(g$x$attrs$labels, c("second", "X", "Y", "Z"))
})
