# Pipeline smoke tests for the mag side of the tabular -> imu converter.
# Detailed pipeline behavior (expanded/compact parsing, min_freq, merge, multi-colset
# coalescing, drop = FALSE, etc.) is covered by `test-as_acc.R`; these tests
# confirm that the mag dispatch wires through correctly and produces `mag`
# vectors with mag-specific error messages.

test_that("as_mag() builds a mag vector from expanded-format mag data", {
  r <- as_mag_df(mag_example_expanded())

  expect_true(is_mag(r))
  expect_false(is_acc(r))
  expect_false(is_gyro(r))
  # Two bursts separated by the time gap in the fixture
  expect_length(r, 10)
  # Each burst retains XYZ axis structure
  expect_identical(colnames(bursts(r)[[1]]), c("X", "Y", "Z"))
})

test_that("as_mag() builds a mag vector from compact-format mag data", {
  r <- as_mag_df(mag_example_compact())

  expect_true(is_mag(r))
  expect_length(r, 2)
  expect_identical(as.numeric(freqs(r)), c(10, 10))
  expect_identical(colnames(bursts(r)[[1]]), c("X", "Y", "Z"))
})

test_that("active_mag_colsets() detects the expanded-format mag colset", {
  expect_identical(
    active_mag_colsets(mag_example_expanded()),
    list(xyz = mag_colset_xyz())
  )
})

test_that("active_mag_colsets() detects the compact-format mag colset", {
  expect_identical(
    active_mag_colsets(mag_example_compact()),
    list(raw = mag_colset_raw())
  )
})

test_that("active_mag_colsets() detects the raw expanded-format mag colset", {
  # `mag_colset_raw_xyz()`: the same axes under `magnetic_field_raw_*` names
  m <- mag_example_expanded()
  names(m) <- sub("^magnetic_field_", "magnetic_field_raw_", names(m))

  expect_identical(active_mag_colsets(m), list(raw_xyz = mag_colset_raw_xyz()))

  # Only the column names differ, so the parsed vector is unchanged
  expect_identical(as_mag_df(m), as_mag_df(mag_example_expanded()))
})

test_that("active_mag_colsets() errors when no mag colset is present", {
  m <- mag_example_expanded()
  m$magnetic_field_x <- NULL
  m$magnetic_field_y <- NULL
  m$magnetic_field_z <- NULL

  expect_error(active_mag_colsets(m), "Could not identify a full")
})

test_that("duplicated_mag_rows() detects overlap across colsets", {
  # Stack an expanded-format mag fixture with a compact-format one, then inject
  # expanded-format values into a row that already carries burst data.
  m <- vctrs::vec_rbind(mag_example_expanded(), mag_example_compact())
  burst_rows <- which(!is.na(m$magnetic_fields_raw))
  m$magnetic_field_x[burst_rows[1]] <- 1
  m$magnetic_field_y[burst_rows[1]] <- 1
  m$magnetic_field_z[burst_rows[1]] <- 1

  expected <- logical(nrow(m))
  expected[burst_rows[1]] <- TRUE

  expect_identical(duplicated_mag_rows(m), expected)
})

test_that("as_mag() errors on overlapping mag rows with a mag-specific message", {
  m <- vctrs::vec_rbind(mag_example_expanded(), mag_example_compact())
  burst_rows <- which(!is.na(m$magnetic_fields_raw))
  m$magnetic_field_x[burst_rows[1]] <- 1
  m$magnetic_field_y[burst_rows[1]] <- 1
  m$magnetic_field_z[burst_rows[1]] <- 1

  expect_error(
    suppressWarnings(as_mag_df(m)),
    "multiple sources of mag data"
  )
})

test_that("as_mag() rejects a non-mag colset argument", {
  expect_error(
    as_mag_df(mag_example_expanded(), colset = "foobar"),
    "must be an <imu_colset>"
  )
})

test_that("as_mag() accepts a user-supplied mag_colset", {
  m <- mag_example_expanded()

  expect_identical(as_mag_df(m), as_mag_df(m, colset = mag_colset_xyz()))
})

test_that("as_mag() errors when the requested colset columns are missing", {
  # Expanded-format fixture doesn't have compact-format mag columns
  expect_error(
    as_mag_df(mag_example_expanded(), colset = mag_colset_raw()),
    "Missing columns"
  )
})

test_that("as_mag() agrees between the move2 and data.frame entry points", {
  skip_if_not_installed("move2")

  expect_identical(
    as_mag(df_to_move2(mag_example_expanded())),
    as_mag_df(mag_example_expanded())
  )
  expect_identical(
    as_mag(df_to_move2(mag_example_compact())),
    as_mag_df(mag_example_compact())
  )
})

test_that("as_mag() rejects timestamp and track_id for move2 input", {
  skip_if_not_installed("move2")

  m <- df_to_move2(mag_example_expanded())

  expect_error(
    as_mag(m, timestamp = move2::mt_time(m)),
    "`timestamp` must not be supplied when"
  )
  expect_error(as_mag(m, track_id = 1), "`track_id` must not be supplied when")
})
