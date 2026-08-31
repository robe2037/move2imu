# Tests ACC extraction from data.frame inputs `gulls_df()` and `albatrosses_df()`
# which are derived from real move2 inputs. data.frame pipeline ensures
# core tests run even if suggested move2 package is not installed.

test_that("Can get acc from compact-format acc data", {
  alb_data <- albatrosses_df()

  acc <- as_acc_df(alb_data, drop = TRUE)

  non_na <- which(!is.na(alb_data$eobs_acceleration_axes))

  expect_s3_class(acc, "acc")
  expect_length(acc, nrow(alb_data[non_na, ]))
  expect_true(is_uniform(acc))
  expect_identical(
    purrr::map_chr(
      bursts(acc),
      function(x) paste0(colnames(x), collapse = "")
    ),
    as.character(alb_data[non_na, ]$eobs_acceleration_axes)
  )
  expect_identical(
    purrr::map_chr(
      bursts(acc),
      function(x) paste(t(x), collapse = " ")
    ),
    alb_data[non_na, ]$eobs_accelerations_raw
  )
  expect_identical(
    freqs(acc),
    alb_data[non_na, ]$eobs_acceleration_sampling_frequency_per_axis
  )
  expect_identical(
    starts(acc),
    alb_data[non_na, ]$timestamp
  )
})

test_that("Can get acc from expanded-format acc data", {
  gulls_data <- gulls_df()

  acc_i <- which(gulls_data$sensor_type_id == 2365683)
  non_na <- which(!is.na(gulls_data$acceleration_raw_x))

  # Identify time series gap points
  gap_i <- which(c(TRUE, diff(gulls_data$timestamp[non_na]) > 0.5))

  acc <- as_acc_df(gulls_data, colset = acc_colset_raw_xyz(), drop = TRUE)

  expect_s3_class(acc, "acc")
  expect_length(acc, length(gap_i))
  expect_true(is_uniform(acc))
  expect_identical(
    unique(purrr::map(bursts(acc), colnames))[[1]],
    c("X", "Y", "Z")
  )
  expect_identical(
    unlist(purrr::map(bursts(acc), ~ .x[, "X"])),
    gulls_data[non_na, ]$acceleration_raw_x
  )
  expect_identical(
    unlist(purrr::map(bursts(acc), ~ .x[, "Y"])),
    gulls_data[non_na, ]$acceleration_raw_y
  )
  expect_identical(
    unlist(purrr::map(bursts(acc), ~ .x[, "Z"])),
    gulls_data[non_na, ]$acceleration_raw_z
  )
  expect_identical(
    unique(freqs(acc)),
    units::set_units(20, "Hz")
  )
  expect_identical(
    starts(acc),
    sort(gulls_data[non_na, ][gap_i, ]$timestamp)
  )
})

test_that("Can manually specify acc columns to use for parsing", {
  cols <- acc_colset_raw_xyz()

  a <- as_acc_df(gulls_df(), colset = cols)
  i <- which_imu_vals(gulls_df(), colset = cols)

  expect_equal(
    unlist(lapply(bursts(a), function(b) b[, 1])),
    gulls_df()[[cols[[1]]]][i]
  )
  expect_equal(
    unlist(lapply(bursts(a), function(b) b[, 2])),
    gulls_df()[[cols[[2]]]][i]
  )
  expect_equal(
    unlist(lapply(bursts(a), function(b) b[, 3])),
    gulls_df()[[cols[[3]]]][i]
  )
})

test_that("Can manually specify a subset of expanded-format cols", {
  col <- imu_colset(y = "acceleration_raw_y")

  a <- as_acc_df(gulls_df(), colset = col)
  i <- which_imu_vals(gulls_df(), colset = col)

  expect_equal(unlist(lapply(bursts(a), function(b) b[, 1])), gulls_df()[[as.character(col)]][i])
})

test_that("Can manually specify acc columns in mixed acc type data", {
  d <- vctrs::vec_rbind(albatrosses_df(), gulls_df())

  expect_identical(
    as_acc_df(albatrosses_df(), drop = TRUE),
    as_acc_df(d, colset = acc_colset_eobs(), drop = TRUE)
  )
  expect_identical(
    as_acc_df(gulls_df(), colset = acc_colset_raw_xyz(), drop = TRUE),
    as_acc_df(d, colset = acc_colset_raw_xyz(), drop = TRUE)
  )
  expect_identical(
    suppressWarnings(as_acc_df(d, drop = TRUE)),
    as_acc_df(d, colset = list(acc_colset_eobs(), acc_colset_raw_xyz()), drop = TRUE)
  )
  expect_identical(
    suppressWarnings(as_acc_df(d, drop = TRUE)),
    as_acc_df(d, colset = list(acc_colset_raw_xyz(), acc_colset_eobs()), drop = TRUE)
  )
})

test_that("Error on duplicate acc rows across colsets", {
  m <- vctrs::vec_rbind(gulls_df(), albatrosses_df())

  # Add raw xyz values to rows that already have eobs data
  eobs_rows <- which(!is.na(m$eobs_accelerations_raw))
  m$acceleration_raw_x[eobs_rows[1:3]] <- 1
  m$acceleration_raw_y[eobs_rows[1:3]] <- 1
  m$acceleration_raw_z[eobs_rows[1:3]] <- 1

  expect_error(
    suppressWarnings(as_acc_df(m)),
    "multiple sources of acc data"
  )
})

test_that("Automatically get all available colsets", {
  m <- vctrs::vec_rbind(gulls_df(), albatrosses_df())

  expect_warning(a <- as_acc_df(m, drop = TRUE), "Detected multiple")
  a2 <- c(as_acc_df(gulls_df(), drop = TRUE), as_acc_df(albatrosses_df(), drop = TRUE))

  # Ensure same ordering on comparison
  expect_identical(a2[order(starts(a2))], a[order(starts(a))])
})

test_that("Multi-colset coalesce preserves index alignment with drop = FALSE", {
  m <- vctrs::vec_rbind(gulls_df(), albatrosses_df())
  suppressWarnings(a <- as_acc_df(m, drop = FALSE))

  expect_length(a, nrow(m))
})

test_that("Multi-colset coalesce places values at correct indices", {
  m <- vctrs::vec_rbind(gulls_df(), albatrosses_df())

  a_eobs <- as_acc_df(m, colset = acc_colset_eobs(), drop = FALSE)
  a_raw <- as_acc_df(m, colset = acc_colset_raw_xyz(), drop = FALSE)
  suppressWarnings(a_all <- as_acc_df(m, drop = FALSE))

  eobs_idx <- which(!is.na(a_eobs))
  raw_idx <- which(!is.na(a_raw))

  # No overlap between colsets
  expect_length(intersect(eobs_idx, raw_idx), 0)

  # Combined non-NA count matches sum of individual colsets
  expect_equal(sum(!is.na(a_all)), length(eobs_idx) + length(raw_idx))

  # Values at each colset's indices match
  expect_identical(a_all[eobs_idx], a_eobs[eobs_idx])
  expect_identical(a_all[raw_idx], a_raw[raw_idx])
})

test_that("Multi-colset drop = TRUE is subset of drop = FALSE", {
  m <- vctrs::vec_rbind(gulls_df(), albatrosses_df())

  suppressWarnings(a_drop <- as_acc_df(m, drop = TRUE))
  suppressWarnings(a_no_drop <- as_acc_df(m, drop = FALSE))

  expect_identical(a_drop, a_no_drop[!is.na(a_no_drop)])
})

test_that("Correctly error on bad colset specifications", {
  expect_error(as_acc_df(gulls_df(), colset = acc_colset_eobs()), "Missing columns")
  expect_error(as_acc_df(gulls_df(), colset = "foobar"), "must be an <imu_colset>")
})

test_that("Error on a user-supplied colset whose columns are present but empty", {
  g <- gulls_df()
  g$acceleration_raw_x <- NA_real_
  g$acceleration_raw_y <- NA_real_
  g$acceleration_raw_z <- NA_real_

  # Columns exist (so they pass the "missing columns" check) but hold no data.
  # Without the guard this would fail later with a cryptic `round()` error.
  expect_error(
    as_acc_df(g, colset = acc_colset_raw_xyz()),
    "contain no data"
  )
})

test_that("Can split expanded-format data into bursts by inferred frequency", {
  m1 <- acc_example_expanded(
    c(seq(1, 3, by = 0.5), 4, 5.5, seq(6, 10, by = 0.5), seq(10.5, 50, by = 0.75))
  )

  a <- as_acc_df(m1, drop = TRUE)

  expect_length(a, 4)
  expect_equal(purrr::map_int(bursts(a), nrow), c(5, 1, 11, 52))
  expect_equal(as.numeric(freqs(a)), c(2, NA, 2, signif(4 / 3, 6)))
})

test_that("Can use `min_freq` to avoid building bursts below freq thresh", {
  m1 <- acc_example_expanded(
    c(seq(1, 3, by = 0.5), 4, 5.5, seq(6, 10, by = 0.5), seq(10.5, 50, by = 0.75))
  )

  a1 <- as_acc_df(m1, min_freq = 1, drop = TRUE)
  a2 <- as_acc_df(m1, min_freq = 2, drop = TRUE)

  # First bursts should be identical, but final burst should be split
  # fully into length-1 "bursts"
  expect_identical(a2[1:3], a1[1:3])
  expect_length(a2, length(a1) - 1 + nrow(bursts(a1)[[4]]))
  expect_identical(do.call(rbind, bursts(a2)[4:length(a2)]), bursts(a1)[[4]])
  expect_true(all(is.na(freqs(a2)[4:length(a2)])))

  expect_length(as_acc_df(m1, min_freq = Inf, drop = TRUE), nrow(m1))
  expect_identical(a1, as_acc_df(m1, min_freq = 0, drop = TRUE))

  # A units-valued `min_freq` is accepted and matches the bare-numeric (Hz) form
  expect_identical(as_acc_df(m1, min_freq = units::set_units(2, "Hz"), drop = TRUE), a2)

  # If `drop = FALSE`, partitioned bursts should fill indices that were
  # previously empty, and overall vector length should stay the same.
  expect_length(
    as_acc_df(gulls_df(), min_freq = 40, drop = FALSE),
    nrow(gulls_df())
  )
})

test_that("Can drop missing acc values", {
  gulls_data <- gulls_df()

  # Provide cols explicitly below to avoid irrelevant multi-col warnings
  cols <- acc_colset_raw_xyz()

  acc <- as_acc_df(gulls_data, colset = cols, drop = FALSE)

  expect_identical(as_acc_df(gulls_data, colset = cols, drop = TRUE), acc[!is.na(acc)])
  expect_length(acc, nrow(gulls_data))

  # Each burst is attached at its first row index; every other row is NA.
  first_i <- sapply(
    parse_bursts(
      gulls_data,
      colset = cols,
      timestamp = gulls_data$timestamp,
      track_id = as.character(gulls_data$id)
    )$bursts,
    function(x) x[1]
  )
  expect_equal(which(!is.na(acc)), sort(first_i))

  acc <- as_acc_df(albatrosses_df(), drop = FALSE)
  expect_identical(as_acc_df(albatrosses_df(), drop = TRUE), acc[!is.na(acc)])
})

test_that("Retain burst dimensions when missing data in some axes", {
  g <- gulls_df()

  g[["acceleration_raw_x"]][1:100] <- NA

  a <- as_acc_df(g, colset = acc_colset_raw_xyz(), drop = TRUE)
  expect_true(all(purrr::map(bursts(a), ncol) == 3))
})

test_that("Preserve time zone", {
  g <- gulls_df()
  a <- albatrosses_df()

  expect_equal(attr(starts(acc()), "tzone"), "UTC")
  expect_equal(attr(starts(acc(acc_burst_example(), 1)), "tzone"), "UTC")

  expect_equal(attr(starts(as_acc_df(albatrosses_df())), "tzone"), "UTC")

  a$timestamp <- 1:nrow(a)
  expect_equal(attr(starts(as_acc_df(a)), "tzone"), "UTC")

  a$timestamp <- .as.POSIXct(a$timestamp, "CET")
  # Set the attribute directly: `as.POSIXct(<POSIXct>, tz = )` ignores `tz`
  # before R 4.3, so this line was a silent no-op there.
  attr(g$timestamp, "tzone") <- "CET"
  expect_equal(attr(starts(as_acc_df(a)), "tzone"), "CET")
  expect_equal(attr(starts(as_acc_df(g)), "tzone"), "CET")
})

test_that("Equivalent data in burst and expanded format produce same acc", {
  t1 <- data.frame(
    id = 1,
    acceleration_x = as.numeric(1:10),
    acceleration_y = as.numeric(1:10),
    acceleration_z = as.numeric(1:10),
    timestamp = .as.POSIXct(seq(1, 1.9, by = 0.1)),
    x = 1,
    y = 1
  )

  t2 <- data.frame(
    id = 1,
    acceleration_axes = "XYZ",
    acceleration_sampling_frequency_per_axis = 10,
    accelerations_raw = c(
      paste0(rep(1:5, each = 3), collapse = " "),
      paste0(rep(6:10, each = 3), collapse = " ")
    ),
    timestamp = .as.POSIXct(c(1, 1.5)),
    x = 1,
    y = 1
  )

  expect_identical(as_acc_df(t1, drop = TRUE), as_acc_df(t2, drop = TRUE))
})

test_that("as_acc() checks expanded-format coltypes", {
  g <- gulls_df()
  g[["acceleration_raw_x"]] <- "foobar"

  expect_error(
    as_acc_df(g, colset = acc_colset_raw_xyz()),
    "Detected non-numeric column"
  )
  expect_silent(as_acc_df(g, colset = imu_colset(y = "acceleration_raw_y")))
})

test_that("Expanded bursts inherit the storage mode of their input columns", {
  t <- data.frame(
    acceleration_x = 1:10,
    acceleration_y = 1:10,
    acceleration_z = 1:10,
    ts = .as.POSIXct(seq(1, 1.9, by = 0.1))
  )

  a_int <- as_acc(t, timestamp = t$ts, track_id = NULL, drop = TRUE)

  expect_identical(storage.mode(bursts(a_int)[[1]]), "integer")
  expect_identical(unlist(bursts(a_int)), rep(1:10, 3))

  t[1:3] <- lapply(t[1:3], as.double)

  a_dbl <- as_acc(t, timestamp = t$ts, track_id = NULL, drop = TRUE)

  expect_identical(storage.mode(bursts(a_dbl)[[1]]), "double")
  expect_identical(unlist(bursts(a_dbl)), rep(as.numeric(1:10), 3))
})

test_that("as_acc() errors when expanded-format columns have mismatched units", {
  g <- gulls_df()

  g$acceleration_raw_x <- units::set_units(
    as.numeric(g$acceleration_raw_x),
    "m/s^2"
  )

  expect_error(
    as_acc_df(g, colset = acc_colset_raw_xyz()),
    "Multiple units detected"
  )
})

test_that("as_acc() uses column units as burst units for expanded data", {
  cols <- as.character(acc_colset_raw_xyz())

  g_plain <- gulls_df()
  g_units <- gulls_df()

  for (col in cols) {
    g_plain[[col]] <- as.numeric(g_plain[[col]])
    g_units[[col]] <- units::set_units(as.numeric(g_units[[col]]), "m/s^2")
  }

  a <- as_acc_df(g_units, colset = acc_colset_raw_xyz(), drop = TRUE)

  expect_s3_class(bursts(a)[[1]], "units")
  expect_identical(units::deparse_unit(bursts(a)[[1]]), "m s-2")

  # Units aside, the parse is unchanged
  expect_identical(
    drop_imu_units(a),
    as_acc_df(g_plain, colset = acc_colset_raw_xyz(), drop = TRUE)
  )
})

test_that("as_acc() errors on swapped burst column types", {
  a <- albatrosses_df()

  # Swap bursts and frequency columns
  expect_error(
    as_acc_df(
      a,
      colset = imu_colset(
        bursts = "eobs_acceleration_sampling_frequency_per_axis",
        axes = "eobs_acceleration_axes",
        frequency = "eobs_accelerations_raw"
      )
    ),
    "must be character"
  )

  # The bursts check fires first, so `axes` and `frequency` need their own
  # inputs to be reached at all.
  a$scratch_num <- 1
  a$scratch_chr <- "XYZ"

  expect_error(
    as_acc_df(
      a,
      colset = imu_colset(
        bursts = "eobs_accelerations_raw",
        axes = "scratch_num",
        frequency = "eobs_acceleration_sampling_frequency_per_axis"
      )
    ),
    "must be character"
  )

  expect_error(
    as_acc_df(
      a,
      colset = imu_colset(
        bursts = "eobs_accelerations_raw",
        axes = "eobs_acceleration_axes",
        frequency = "scratch_chr"
      )
    ),
    "must be numeric"
  )
})

test_that("Custom compact-format colset works end-to-end", {
  alb <- albatrosses_df()

  a <- as_acc_df(alb, colset = acc_colset_eobs())

  colnames(alb)[colnames(alb) == "eobs_acceleration_axes"] <- "my_axes"
  colnames(alb)[colnames(alb) == "eobs_acceleration_sampling_frequency_per_axis"] <- "my_freq"
  colnames(alb)[colnames(alb) == "eobs_accelerations_raw"] <- "my_bursts"

  # Use the eobs columns via a custom colset (equivalent to acc_colset_eobs())
  custom <- imu_colset(
    bursts = "my_bursts",
    axes = "my_axes",
    frequency = "my_freq"
  )

  expect_identical(as_acc_df(alb, colset = custom), a)
})

test_that("Custom expanded-format colset works end-to-end", {
  gul <- gulls_df()

  a <- as_acc_df(gul, colset = acc_colset_raw_xyz())

  colnames(gul)[colnames(gul) == "acceleration_raw_x"] <- "acc_x"
  colnames(gul)[colnames(gul) == "acceleration_raw_y"] <- "acc_y"
  colnames(gul)[colnames(gul) == "acceleration_raw_z"] <- "acc_z"

  # Use the raw xyz columns via a custom colset (equivalent to
  # acc_colset_raw_xyz())
  custom <- imu_colset(x = "acc_x", y = "acc_y", z = "acc_z")

  expect_identical(as_acc_df(gul, colset = custom), a)
})

test_that("freq_tol and min_freq control burst parsing (gap_tol does not)", {
  # 1 Hz data with a single 1.001 s hiccup
  ts <- cumsum(c(0, rep(1, 26), 1.001, 0.999, 1, 1))
  m <- acc_example_expanded(ts)

  # Hiccup is absorbed by default `freq_tol`
  expect_length(as_acc_df(m, drop = TRUE), 1)

  # A `freq_tol` tighter than the magnitude of the glitch triggers a split
  expect_equal(length(as_acc_df(m, freq_tol = 1e-5, drop = TRUE)), 3)

  expect_length(as_acc_df(m, min_freq = 1, drop = TRUE), 1)

  # Genuinely slow data (0.5 Hz) fall below the floor and are exploded into
  # individual (length-1) bursts; with no floor they stay a single burst.
  slow <- acc_example_expanded(cumsum(c(0, rep(2, 9))))
  expect_length(as_acc_df(slow, min_freq = 1, drop = TRUE), 10)
  expect_length(as_acc_df(slow, min_freq = 0, drop = TRUE), 1)

  # `gap_tol` only acts when merging, not parsing:
  two <- acc_example_expanded(c(0:9, 10.5 + 0:9))
  expect_length(as_acc_df(two, merge_continuous = FALSE, drop = TRUE), 2)
  expect_identical(
    as_acc_df(two, gap_tol = 1e-6, merge_continuous = FALSE, drop = TRUE),
    as_acc_df(two, gap_tol = 0.5, merge_continuous = FALSE, drop = TRUE)
  )
  expect_length(as_acc_df(two, gap_tol = 0.5, drop = TRUE), 1)
  expect_length(as_acc_df(two, drop = TRUE), 2)
})

test_that("as_acc() validates tolerances", {
  compact <- data.frame(
    acceleration_axes = "XYZ",
    acceleration_sampling_frequency_per_axis = 10,
    accelerations_raw = "1 2 3 4 5 6",
    ts = .as.POSIXct(1)
  )

  expect_error(
    as_acc(compact, timestamp = compact$ts, track_id = NULL, min_freq = -1),
    "`min_freq` must be greater than or equal to 0"
  )
  expect_error(
    as_acc(
      compact,
      timestamp = compact$ts,
      track_id = NULL,
      freq_tol = -1,
      merge_continuous = FALSE
    ),
    "`freq_tol` must be greater than or equal to 0"
  )
  expect_error(
    as_acc(
      compact,
      timestamp = compact$ts,
      track_id = NULL,
      gap_tol = -1,
      merge_continuous = FALSE
    ),
    "`gap_tol` must be greater than or equal to 0"
  )
  expect_error(
    as_acc(
      compact,
      timestamp = compact$ts,
      track_id = NULL,
      freq_tol = units::set_units(1e-2, "Hz")
    ),
    "`freq_tol` must be a bare numeric value"
  )

  # An input with no rows to parse is checked the same way
  expect_error(
    as_acc(compact[0, ], timestamp = compact$ts[0], track_id = NULL, gap_tol = -1),
    "`gap_tol` must be greater than or equal to 0"
  )
})

test_that("duplicate timestamps within a track are rejected", {
  expect_error(
    as_acc_df(acc_example_expanded(c(0, 0, 0, 1, 2, 3))),
    "strictly increasing"
  )

  # Duplicates shared across different tracks should be fine
  two_tracks <- acc_example_expanded(c(0, 0), id = c("a", "b"))
  expect_no_error(as_acc_df(two_tracks))
})

test_that("out-of-order timestamps within a track are rejected", {
  # Put problem timestamp in second track to also ensure all tracks are checked
  expect_error(
    as_acc_df(acc_example_expanded(
      c(0, 0.05, 0.10, 0, 0.10, 0.05),
      id = c("a", "a", "a", "b", "b", "b")
    )),
    "strictly increasing"
  )
})

test_that("NA timestamps on IMU records are rejected", {
  expect_error(
    as_acc_df(acc_example_expanded(c(0, 0.05, NA, 0.10))),
    "must be non-NA"
  )

  # NA values outside of IMU data rows don't error
  d <- data.frame(
    id = 1,
    acceleration_x = c(1, 2, NA, 3),
    acceleration_y = c(1, 2, NA, 3),
    acceleration_z = c(1, 2, NA, 3),
    timestamp = as.POSIXct(c(0, 0.05, NA, 0.10), tz = "UTC", origin = "2020-01-01"),
    x = 1, y = 1
  )
  expect_no_error(as_acc_df(d, drop = TRUE))
})

test_that("Only IMU records are considered when checking data ordering", {
  d <- data.frame(
    id = 1,
    acceleration_raw_x = c(1, NA, 2, NA, 3),
    acceleration_raw_y = c(4, NA, 5, NA, 6),
    acceleration_raw_z = c(7, NA, 8, NA, 9),
    timestamp = as.POSIXct(
      c(0, 0.05, 0.05, 0.02, 0.10),
      tz = "UTC", origin = "2020-01-01"
    ),
    x = 1, y = 1
  )

  # Across all records the timestamps are neither ordered nor unique
  expect_false(times_ordered(d$timestamp, d$id))

  # But the acc records alone are strictly increasing
  expect_no_error(a <- as_acc_df(d, drop = TRUE))
  expect_length(a, 1)
  expect_identical(n_samples(a), 3L)
  expect_equal(as.numeric(freqs(a)), 20)
  expect_identical(bursts(a)[[1]][, "X"], c(1, 2, 3))
})

test_that("compact bursts must also be ordered and unique within a track", {
  expect_error(as_acc_df(acc_example_compact(.as.POSIXct(c(2, 0)))), "strictly increasing")
  expect_error(as_acc_df(acc_example_compact(.as.POSIXct(c(0, 0)))), "strictly increasing")
  expect_no_error(as_acc_df(acc_example_compact(.as.POSIXct(c(0, 2)))))

  comp <- acc_example_compact(.as.POSIXct(c(0, 0)), id = c(1, 2))
  expect_no_error(as_acc_df(comp))
})

test_that("tracks that are not grouped together are rejected", {
  d <- acc_example_expanded(c(1, 2, 3, 4), id = c("a", "b", "a", "b"))

  expect_error(as_acc_df(d), "Not all tracks are grouped")

  d$id <- c("a", "a", "b", "b")
  expect_s3_class(as_acc_df(d), "acc")
})

test_that("as_acc() treats NULL track_id as a single track", {
  # Two tracks that are contiguous in time, so the only thing keeping their
  # samples in separate bursts is the track ID.
  d <- acc_example_expanded(seq(0, 0.5, by = 0.1), id = rep(c("a", "b"), each = 3))

  per_track <- as_acc_df(d, drop = TRUE)
  one_track <- as_acc(d, timestamp = d$timestamp, track_id = NULL, drop = TRUE)

  # One burst per track, versus a single burst spanning every sample
  expect_length(per_track, 2)
  expect_identical(purrr::map_int(bursts(per_track), nrow), c(3L, 3L))

  expect_length(one_track, 1)
  expect_identical(nrow(bursts(one_track)[[1]]), 6L)

  # A single supplied track ID is equivalent to NULL
  expect_identical(
    as_acc(d, timestamp = d$timestamp, track_id = rep("a", nrow(d)), drop = TRUE),
    one_track
  )
})

test_that("a single IMU record is parsed without ordering checks", {
  # One row has nothing to compare against, so the ordering checks short-circuit.
  # Expanded data cannot derive a frequency from a lone sample.
  e <- acc_example_expanded(0)

  a <- as_acc_df(e, drop = TRUE)

  expect_length(a, 1)
  expect_identical(n_samples(a), 1L)
  expect_true(is.na(freqs(a)))

  # A compact row carries its own frequency, so it parses fully
  c1 <- as_acc_df(acc_example_compact(.as.POSIXct(0)), drop = TRUE)

  expect_length(c1, 1)
  expect_identical(n_samples(c1), 20L)
})

test_that("track IDs are compared by observed value, not declared levels", {
  # Subsetting a factor track_id leaves its unused levels behind. Those must not
  # be counted as tracks that the data failed to group.
  d <- acc_example_expanded(c(1, 2, 3, 4), id = c("a", "a", "b", "b"))
  d$id <- factor(d$id, levels = c("a", "b", "unused"))

  expect_identical(
    as_acc_df(d, drop = TRUE),
    as_acc_df(acc_example_expanded(c(1, 2, 3, 4), id = c("a", "a", "b", "b")), drop = TRUE)
  )
})

test_that("an empty input returns an empty vector", {
  empty <- gulls_df()[0, ]
  a <- as_acc_df(empty)

  expect_s3_class(a, "acc")
  expect_length(a, 0)
})

test_that("burst frequency is span-based (unbiased) for non-uniform spacing", {
  # Uniform spacing: span frequency equals the mean instantaneous frequency.
  m_uniform <- acc_example_expanded(c(0, 1, 2))
  expect_equal(as.numeric(freqs(as_acc_df(m_uniform, drop = TRUE))), 1)

  # Non-uniform spacing normally splits bursts, but a freq_tol covering the
  # 40% change from the 1 s to the 1.4 s interval incorporates them together.
  # The derived frequency is biased with (mean(1/diff)); instead use (n-1)/span.
  m_jitter <- acc_example_expanded(c(0, 1, 2.4))
  a <- as_acc_df(m_jitter, freq_tol = 0.5, drop = TRUE)

  expect_length(a, 1)
  expect_equal(as.numeric(freqs(a)), signif(2 / 2.4, 6))
})

test_that("as_acc() validates timestamp and track_id for data.frame input", {
  # Unlike the move2 method, which reads both from the object's metadata, the
  # tabular method takes them as arguments and has to check them itself.
  d <- acc_example_expanded(c(1, 2, 3, 4), id = c("a", "a", "b", "b"))

  expect_error(as_acc(d), 'argument "timestamp" is missing')
  expect_error(
    as_acc(d, timestamp = d$timestamp),
    'argument "track_id" is missing'
  )
  expect_no_error(as_acc(d, timestamp = as.numeric(d$timestamp), track_id = NULL))
  # Rejected by the shared `timestamp_to_POSIXct()`, so the wording matches the
  # one used by `acc()`/`starts<-` rather than being specific to this entry point
  expect_error(
    as_acc(d, timestamp = as.character(d$timestamp), track_id = NULL),
    "must be a timestamp, not <character>"
  )

  # `units` vectors are numeric, but their unit would be silently ignored, so we
  # don't accept them
  expect_error(
    as_acc(d, timestamp = units::set_units(as.numeric(d$timestamp), "s"), track_id = NULL),
    "must be a timestamp, not <units>"
  )
  expect_error(
    as_acc(d, timestamp = d$timestamp[1:2], track_id = NULL),
    "must be the same length"
  )
  expect_error(
    as_acc(d, timestamp = d$timestamp, track_id = d$id[1:2]),
    "must be the same length"
  )
  expect_error(
    as_acc(d, timestamp = d$timestamp, track_id = c("a", NA, "b", "b")),
    "must not contain missing values"
  )
})

test_that("as_acc() rejects unused arguments for either burst format", {
  df <- data.frame(
    acceleration_x = as.numeric(1:4),
    acceleration_y = as.numeric(1:4),
    acceleration_z = as.numeric(1:4),
    ts = .as.POSIXct(1:4)
  )

  expect_error(as_acc_df(albatrosses_df(), typo = 1), "`\\.\\.\\.` must be empty")
  expect_error(as_acc_df(gulls_df(), typo = 1), "`\\.\\.\\.` must be empty")
  expect_error(
    as_acc(df, timestamp = df$ts, track_id = NULL, typo = 1),
    "`\\.\\.\\.` must be empty"
  )
})

test_that("Timestamps that are not POSIXct are normalized", {
  # move2 permits a `numeric`, `Date`, or `POSIXt` time column, so the tabular
  # method accepts the same range through its `timestamp` argument.
  d <- as_acc_df(acc_example_compact(as.Date(c("2020-01-01", "2020-01-02"))))
  expect_identical(starts(d), as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"))

  # Numeric timestamps are seconds since the epoch
  n <- as_acc_df(acc_example_compact(c(0, 10)))
  expect_identical(starts(n), .as.POSIXct(c(0, 10)))

  e <- acc_example_expanded(seq(0, 0.9, by = 0.1))
  a <- as_acc_df(e, drop = TRUE)

  expect_identical(
    as_acc(e, timestamp = as.POSIXlt(e$timestamp), track_id = e$id, drop = TRUE),
    a
  )
  expect_identical(
    as_acc(e, timestamp = as.numeric(e$timestamp), track_id = e$id, drop = TRUE),
    a
  )

  # Burst frequencies are derived from the timestamps for expanded data, so a
  # `Date` column must be seconds there too: a daily series is 1/86400 Hz.
  e$timestamp <- as.Date("2020-01-01") + seq_len(nrow(e)) - 1

  daily <- as_acc_df(e, drop = TRUE)

  expect_equal(as.numeric(freqs(daily)), signif(1 / 86400, 6))
  expect_identical(starts(daily), as.POSIXct("2020-01-01", tz = "UTC"))
})
