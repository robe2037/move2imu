# Pipeline smoke tests for the gyro side of the tabular -> imu converter.
# Detailed pipeline behavior (expanded/compact parsing, min_freq, merge, multi-colset
# coalescing, drop = FALSE, etc.) is covered by `test-as_acc.R`; these tests
# confirm that the gyro dispatch wires through correctly and produces `gyro`
# vectors with gyro-specific error messages.

test_that("as_gyro() builds a gyro vector from expanded-format gyro data", {
  r <- as_gyro_df(gyro_example_expanded_df())

  expect_true(is_gyro(r))
  expect_false(is_acc(r))
  expect_false(is_mag(r))
  # Two bursts separated by the time gap in the fixture
  expect_length(r, 10)
  # Each burst retains XYZ axis structure
  expect_identical(colnames(bursts(r)[[1]]), c("X", "Y", "Z"))
})

test_that("as_gyro() builds a gyro vector from compact-format gyro data", {
  r <- as_gyro_df(gyro_example_compact_df())

  expect_true(is_gyro(r))
  expect_length(r, 2)
  expect_identical(as.numeric(freqs(r)), c(10, 10))
  expect_identical(colnames(bursts(r)[[1]]), c("X", "Y", "Z"))
})

test_that("active_gyro_colsets() detects the expanded-format gyro colset", {
  expect_identical(
    active_gyro_colsets(gyro_example_expanded_df()),
    list(xyz = gyro_colset_xyz())
  )
})

test_that("active_gyro_colsets() detects the compact-format gyro colset", {
  expect_identical(
    active_gyro_colsets(gyro_example_compact_df()),
    list(raw = gyro_colset_raw())
  )
})

test_that("active_gyro_colsets() errors when no gyro colset is present", {
  g <- gyro_example_expanded_df()
  g$angular_velocity_x <- NULL
  g$angular_velocity_y <- NULL
  g$angular_velocity_z <- NULL

  expect_error(active_gyro_colsets(g), "Could not identify a full")
})

test_that("duplicated_gyro_rows() detects overlap across colsets", {
  # Stack an expanded-format gyro fixture with a compact-format one, then inject
  # expanded-format values into a row that already carries burst data.
  g <- vctrs::vec_rbind(gyro_example_expanded_df(), gyro_example_compact_df())
  burst_rows <- which(!is.na(g$angular_velocities_raw))
  g$angular_velocity_x[burst_rows[1]] <- 1
  g$angular_velocity_y[burst_rows[1]] <- 1
  g$angular_velocity_z[burst_rows[1]] <- 1

  expected <- logical(nrow(g))
  expected[burst_rows[1]] <- TRUE

  expect_identical(duplicated_gyro_rows(g), expected)
})

test_that("as_gyro() errors on overlapping gyro rows with a gyro-specific message", {
  g <- vctrs::vec_rbind(gyro_example_expanded_df(), gyro_example_compact_df())
  burst_rows <- which(!is.na(g$angular_velocities_raw))
  g$angular_velocity_x[burst_rows[1]] <- 1
  g$angular_velocity_y[burst_rows[1]] <- 1
  g$angular_velocity_z[burst_rows[1]] <- 1

  expect_error(
    suppressWarnings(as_gyro_df(g)),
    "multiple sources of gyro data"
  )
})

test_that("as_gyro() rejects a non-gyro colset argument", {
  expect_error(
    as_gyro_df(gyro_example_expanded_df(), colset = "foobar"),
    "must be an <imu_colset>"
  )
})

test_that("as_gyro() accepts a user-supplied gyro_colset", {
  g <- gyro_example_expanded_df()

  expect_identical(as_gyro_df(g), as_gyro_df(g, colset = gyro_colset_xyz()))
})

test_that("as_gyro() errors when the requested colset columns are missing", {
  expect_error(
    as_gyro_df(gyro_example_expanded_df(), colset = gyro_colset_raw()),
    "Missing columns"
  )
})

test_that("as_gyro() agrees between the move2 and data.frame entry points", {
  skip_if_not_installed("move2")

  expect_identical(
    as_gyro(gyro_example_expanded()),
    as_gyro_df(gyro_example_expanded_df())
  )
  expect_identical(
    as_gyro(gyro_example_compact()),
    as_gyro_df(gyro_example_compact_df())
  )
})

test_that("as_gyro() rejects timestamp and track_id for move2 input", {
  skip_if_not_installed("move2")

  g <- gyro_example_expanded()

  expect_error(
    as_gyro(g, timestamp = move2::mt_time(g)),
    "`timestamp` must not be supplied when"
  )
  expect_error(as_gyro(g, track_id = 1), "`track_id` must not be supplied when")
})
