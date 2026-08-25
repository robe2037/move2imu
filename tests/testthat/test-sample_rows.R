test_that("acc_sample_rows flags rows with raw acc data (compact format)", {
  # For compact-format data (one row per burst), every row that contributes
  # data also stores a non-NA burst in the as_acc() output
  alb <- albatrosses_df()
  expect_identical(acc_sample_rows(alb), !is.na(as_acc_df(alb)))
})

test_that("acc_sample_rows flags rows with raw acc data (expanded format)", {
  gul <- gulls_df()

  h <- acc_sample_rows(gul)

  expect_identical(h, !is.na(gul$acceleration_raw_x))
  expect_equal(sum(h), sum(n_samples(as_acc_df(gul, drop = TRUE))))
})

test_that("acc_sample_rows returns a logical vector parallel to nrow(x)", {
  alb <- albatrosses_df()

  h <- acc_sample_rows(alb)

  expect_type(h, "logical")
  expect_length(h, nrow(alb))
  expect_false(anyNA(h))
})

test_that("acc_sample_rows respects an explicit colset", {
  gul <- gulls_df()

  gul$acc_x <- gul$acceleration_raw_x
  gul$acc_y <- gul$acceleration_raw_y
  gul$acc_z <- gul$acceleration_raw_z

  h_default <- acc_sample_rows(gul)

  gul$acceleration_raw_x <- NULL
  gul$acceleration_raw_y <- NULL
  gul$acceleration_raw_z <- NULL

  h_explicit <- acc_sample_rows(
    gul,
    colset = imu_colset(x = "acc_x", y = "acc_y", z = "acc_z")
  )

  expect_identical(h_default, h_explicit)
})

test_that("*_sample_rows() returns all-FALSE when no active colset is detected", {
  alb <- albatrosses_df()

  h_mag <- mag_sample_rows(alb)
  h_gyro <- gyro_sample_rows(alb)

  expect_length(h_mag, nrow(alb))
  expect_length(h_gyro, nrow(alb))
  expect_false(any(h_mag))
  expect_false(any(h_gyro))
})

test_that("acc_sample_rows returns TRUE for rows where multiple colsets overlap", {
  gul <- gulls_df()
  gul$acceleration_x <- gul$acceleration_raw_x
  gul$acceleration_y <- gul$acceleration_raw_y
  gul$acceleration_z <- gul$acceleration_raw_z

  expect_error(suppressWarnings(as_acc_df(gul)), "multiple sources")

  h <- acc_sample_rows(gul)

  expect_no_warning(acc_sample_rows(gul))
  expect_length(h, nrow(gul))
  expect_identical(h, !is.na(gul$acceleration_raw_x))
})

test_that("acc_sample_rows returns the union when colsets cover disjoint rows", {
  # Partition the acc data in gulls_df() into two disjoint colsets.
  # acc_sample_rows() should identify TRUE when either colset contains acc data
  gul <- gulls_df()

  acc_rows <- which(!is.na(gul$acceleration_raw_x))
  to_xyz <- acc_rows[seq_along(acc_rows) %% 2 == 0]

  gul$acceleration_x <- NA_real_
  gul$acceleration_y <- NA_real_
  gul$acceleration_z <- NA_real_

  gul$acceleration_x[to_xyz] <- gul$acceleration_raw_x[to_xyz]
  gul$acceleration_y[to_xyz] <- gul$acceleration_raw_y[to_xyz]
  gul$acceleration_z[to_xyz] <- gul$acceleration_raw_z[to_xyz]

  gul$acceleration_raw_x[to_xyz] <- NA_real_
  gul$acceleration_raw_y[to_xyz] <- NA_real_
  gul$acceleration_raw_z[to_xyz] <- NA_real_

  h <- acc_sample_rows(gul)
  expected <- !is.na(gul$acceleration_raw_x) | !is.na(gul$acceleration_x)

  expect_identical(h, expected)
  expect_equal(sum(h), length(acc_rows))
})

test_that("acc_sample_rows() accepts a plain data.frame", {
  # No `move2` involved: `*_sample_rows()` is documented for `data.frame` too
  df <- data.frame(
    acceleration_raw_x = c(1, NA, 1),
    acceleration_raw_y = c(2, NA, 2),
    acceleration_raw_z = c(3, NA, 3),
    eobs_accelerations_raw = c(NA, "1 2 3", "1 2 3"),
    eobs_acceleration_axes = c(NA, "XYZ", "XYZ"),
    eobs_acceleration_sampling_frequency_per_axis = c(NA, 10, 10)
  )

  expect_identical(acc_sample_rows(df), c(TRUE, TRUE, TRUE))
  expect_identical(
    acc_sample_rows(df, colset = acc_colset_eobs()),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("mag_sample_rows() and gyro_sample_rows() accept a plain data.frame", {
  m <- mag_example_compact()
  g <- gyro_example_compact()

  expect_identical(mag_sample_rows(m), c(TRUE, TRUE))
  expect_identical(gyro_sample_rows(g), c(TRUE, TRUE))

  # A sensor with no active colset in `x` flags no rows at all
  expect_identical(gyro_sample_rows(m), c(FALSE, FALSE))
  expect_identical(acc_sample_rows(m), c(FALSE, FALSE))
})

test_that("acc_sample_rows handles a zero-row input", {
  h <- acc_sample_rows(data.frame())
  expect_type(h, "logical")
  expect_length(h, 0)
})
