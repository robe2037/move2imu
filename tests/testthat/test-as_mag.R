# Pipeline smoke tests for the mag side of the move2 -> imu converter.
# Detailed pipeline behavior (expanded/compact parsing, min_freq, merge, multi-colset
# coalescing, drop = FALSE, etc.) is covered by `test-as_acc.R`; these tests
# confirm that the mag dispatch wires through correctly and produces `mag`
# vectors with mag-specific error messages.

skip_if_not_installed("move2")

test_that("as_mag() builds a mag vector from expanded-format mag data", {
  m <- mag_example_expanded()

  r <- as_mag(m)

  expect_true(is_mag(r))
  expect_false(is_acc(r))
  # Two bursts separated by the time gap in the fixture
  expect_length(r, 10)
  # Each burst retains XYZ axis structure
  expect_identical(colnames(bursts(r)[[1]]), c("X", "Y", "Z"))
})

test_that("as_mag() builds a mag vector from compact-format mag data", {
  m <- mag_example_compact()

  r <- as_mag(m)

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

test_that("active_mag_colsets() errors when no mag colset is present", {
  m <- mag_example_expanded()
  m$magnetic_field_x <- NULL
  m$magnetic_field_y <- NULL
  m$magnetic_field_z <- NULL

  expect_error(active_mag_colsets(m), "Could not identify a full")
})

test_that("duplicated_mag_rows() detects overlap across colsets", {
  # Stack a expanded-format mag fixture with a compact-format one, then inject
  # expanded-format values into rows that already carry burst data.
  m <- move2::mt_stack(mag_example_expanded(), mag_example_compact())
  burst_rows <- which(!is.na(m$magnetic_fields_raw))
  m$magnetic_field_x[burst_rows[1]] <- 1
  m$magnetic_field_y[burst_rows[1]] <- 1
  m$magnetic_field_z[burst_rows[1]] <- 1

  expected <- logical(nrow(m))
  expected[burst_rows[1]] <- TRUE

  expect_identical(duplicated_mag_rows(m), expected)
})

test_that("as_mag() errors on overlapping mag rows with a mag-specific message", {
  m <- move2::mt_stack(mag_example_expanded(), mag_example_compact())
  burst_rows <- which(!is.na(m$magnetic_fields_raw))
  m$magnetic_field_x[burst_rows[1]] <- 1
  m$magnetic_field_y[burst_rows[1]] <- 1
  m$magnetic_field_z[burst_rows[1]] <- 1

  expect_error(
    suppressWarnings(as_mag(m)),
    "multiple sources of mag data"
  )
})

test_that("as_mag() rejects a non-mag colset argument", {
  expect_error(
    as_mag(mag_example_expanded(), colset = "foobar"),
    "must be an <imu_colset>"
  )
})

test_that("as_mag() accepts a user-supplied mag_colset", {
  m <- mag_example_expanded()

  r_default <- as_mag(m)
  r_explicit <- as_mag(m, colset = mag_colset_xyz())

  expect_identical(r_default, r_explicit)
})

test_that("as_mag() errors when the requested colset columns are missing", {
  # Expanded-format fixture doesn't have compact-format mag columns
  expect_error(
    as_mag(mag_example_expanded(), colset = mag_colset_raw()),
    "Missing columns"
  )
})

test_that("as_mag() dispatches on data.frame", {
  m <- mag_example_expanded()
  m_df <- as.data.frame(m)

  expect_identical(
    as_mag(
      m_df,
      timestamp = m_df[[move2::mt_time_column(m)]],
      track_id = m_df[[move2::mt_track_id_column(m)]]
    ),
    as_mag(m)
  )
})
