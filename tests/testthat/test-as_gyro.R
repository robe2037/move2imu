# Pipeline smoke tests for the gyro side of the move2 -> imu converter.
# Detailed pipeline behavior (expanded/compact parsing, min_freq, merge, multi-colset
# coalescing, drop = FALSE, etc.) is covered by `test-as_acc.R`; these tests
# confirm that the gyro dispatch wires through correctly and produces `gyro`
# vectors with gyro-specific error messages.

skip_if_not_installed("move2")

test_that("as_gyro() builds a gyro vector from expanded-format gyro data", {
  g <- gyro_example_expanded()

  r <- as_gyro(g)

  expect_true(is_gyro(r))
  expect_false(is_acc(r))
  expect_false(is_mag(r))
  # Two bursts separated by the time gap in the fixture
  expect_length(r, 10)
  # Each burst retains XYZ axis structure
  expect_identical(colnames(bursts(r)[[1]]), c("X", "Y", "Z"))
})

test_that("as_gyro() builds a gyro vector from compact-format gyro data", {
  g <- gyro_example_compact()

  r <- as_gyro(g)

  expect_true(is_gyro(r))
  expect_length(r, 2)
  expect_identical(as.numeric(freqs(r)), c(10, 10))
  expect_identical(colnames(bursts(r)[[1]]), c("X", "Y", "Z"))
})

test_that("active_gyro_colsets() detects the expanded-format gyro colset", {
  expect_identical(
    active_gyro_colsets(gyro_example_expanded()),
    list(xyz = gyro_colset_xyz())
  )
})

test_that("active_gyro_colsets() detects the compact-format gyro colset", {
  expect_identical(
    active_gyro_colsets(gyro_example_compact()),
    list(raw = gyro_colset_raw())
  )
})

test_that("active_gyro_colsets() errors when no gyro colset is present", {
  g <- gyro_example_expanded()
  g$angular_velocity_x <- NULL
  g$angular_velocity_y <- NULL
  g$angular_velocity_z <- NULL

  expect_error(active_gyro_colsets(g), "Could not identify a full")
})

test_that("duplicated_gyro_rows() detects overlap across colsets", {
  g <- move2::mt_stack(gyro_example_expanded(), gyro_example_compact())
  burst_rows <- which(!is.na(g$angular_velocities_raw))
  g$angular_velocity_x[burst_rows[1]] <- 1
  g$angular_velocity_y[burst_rows[1]] <- 1
  g$angular_velocity_z[burst_rows[1]] <- 1

  expected <- logical(nrow(g))
  expected[burst_rows[1]] <- TRUE
  
  expect_identical(duplicated_gyro_rows(g), expected)
})

test_that("as_gyro() errors on overlapping gyro rows with a gyro-specific message", {
  g <- move2::mt_stack(gyro_example_expanded(), gyro_example_compact())
  burst_rows <- which(!is.na(g$angular_velocities_raw))
  g$angular_velocity_x[burst_rows[1]] <- 1
  g$angular_velocity_y[burst_rows[1]] <- 1
  g$angular_velocity_z[burst_rows[1]] <- 1

  expect_error(
    suppressWarnings(as_gyro(g)),
    "multiple sources of gyro data"
  )
})

test_that("as_gyro() rejects a non-gyro colset argument", {
  expect_error(
    as_gyro(gyro_example_expanded(), colset = "foobar"),
    "must be an <imu_colset>"
  )
})

test_that("as_gyro() accepts a user-supplied gyro_colset", {
  g <- gyro_example_expanded()

  r_default <- as_gyro(g)
  r_explicit <- as_gyro(g, colset = gyro_colset_xyz())

  expect_identical(r_default, r_explicit)
})

test_that("as_gyro() errors when the requested colset columns are missing", {
  expect_error(
    as_gyro(gyro_example_expanded(), colset = gyro_colset_raw()),
    "Missing columns"
  )
})

test_that("as_gyro() dispatches on data.frame", {
  g <- gyro_example_expanded()
  g_df <- as.data.frame(g)

  expect_identical(
    as_gyro(
      g_df,
      timestamp = g_df[[move2::mt_time_column(g)]],
      track_id = g_df[[move2::mt_track_id_column(g)]]
    ),
    as_gyro(g)
  )
})
