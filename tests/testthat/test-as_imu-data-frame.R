# This file tests data.frame methods without gating on availability of move2

test_that("ids_cleaved() detects non-contiguous tracks", {
  expect_true(ids_cleaved(c("a", "a", "b", "b")))
  expect_false(ids_cleaved(c("a", "b", "a")))

  expect_true(ids_cleaved(character(0)))
  expect_true(ids_cleaved("a"))

  f <- factor(c("a", "a", "b"), levels = c("a", "b", "unused"))
  expect_true(ids_cleaved(f))
})

test_that("times_ordered() requires strictly increasing time within a track", {
  t <- .as.POSIXct(c(1, 2, 3, 1, 2))
  id <- c("a", "a", "a", "b", "b")

  # Time may step backwards only where the track changes
  expect_true(times_ordered(t, id))
  expect_false(times_ordered(t, rep("a", 5)))

  # Duplicate timestamps within a track are not strictly increasing
  expect_false(times_ordered(.as.POSIXct(c(1, 1)), c("a", "a")))
  expect_false(times_ordered(.as.POSIXct(c(2, 1)), c("a", "a")))

  expect_true(times_ordered(.as.POSIXct(numeric(0)), character(0)))
  expect_true(times_ordered(.as.POSIXct(1), "a"))

  # Sub-second intervals must not be flattened by difftime unit selection
  expect_true(times_ordered(.as.POSIXct(c(0, 1e-3, 5)), rep("a", 3)))
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

test_that("as_acc() rejects unordered and non-cleaved data.frame input", {
  df <- data.frame(
    acceleration_x = as.numeric(1:4),
    acceleration_y = as.numeric(1:4),
    acceleration_z = as.numeric(1:4),
    ts = .as.POSIXct(c(1, 2, 3, 4)),
    id = c("a", "a", "b", "b")
  )

  unordered <- df
  unordered$ts <- .as.POSIXct(c(2, 1, 3, 4))
  expect_error(
    as_acc(unordered, timestamp = unordered$ts, track_id = unordered$id),
    "strictly increasing"
  )

  non_cleaved <- df
  non_cleaved$id <- c("a", "b", "a", "b")
  expect_error(
    as_acc(non_cleaved, timestamp = non_cleaved$ts, track_id = non_cleaved$id),
    "Not all tracks are grouped"
  )

  expect_s3_class(as_acc(df, timestamp = df$ts, track_id = df$id), "acc")
})

test_that("as_acc() validates timestamp and track_id for data.frame input", {
  df <- data.frame(
    acceleration_x = as.numeric(1:4),
    acceleration_y = as.numeric(1:4),
    acceleration_z = as.numeric(1:4),
    ts = .as.POSIXct(1:4),
    id = c("a", "a", "b", "b")
  )

  expect_error(as_acc(df), 'argument "timestamp" is missing')
  expect_error(
    as_acc(df, timestamp = df$ts),
    'argument "track_id" is missing'
  )
  expect_no_error(as_acc(df, timestamp = as.numeric(df$ts), track_id = NULL))
  # Rejected by the shared `timestamp_to_POSIXct()`, so the wording matches the
  # one used by `acc()`/`starts<-` rather than being specific to this entry point
  expect_error(
    as_acc(df, timestamp = as.character(df$ts), track_id = NULL),
    "must be a timestamp, not <character>"
  )

  # `units` vectors are numeric, but their unit would be silently ignored
  expect_error(
    as_acc(df, timestamp = units::set_units(as.numeric(df$ts), "s"), track_id = NULL),
    "must not carry"
  )
  expect_error(
    as_acc(df, timestamp = df$ts[1:2], track_id = NULL),
    "must be the same length"
  )
  expect_error(
    as_acc(df, timestamp = df$ts, track_id = df$id[1:2]),
    "must be the same length"
  )
  expect_error(
    as_acc(df, timestamp = df$ts, track_id = c("a", NA, "b", "b")),
    "must not contain missing values"
  )
})

test_that("as_acc() accepts other date-time representations", {
  # These time formats are less useful, but they are supported in move2 time
  # columns, so we should at least pass them through coherently.
  df <- data.frame(
    acceleration_x = as.numeric(1:4),
    acceleration_y = as.numeric(1:4),
    acceleration_z = as.numeric(1:4),
    ts = .as.POSIXct(seq(1, 1.3, by = 0.1))
  )

  a <- as_acc(df, timestamp = df$ts, track_id = NULL, drop = TRUE)

  expect_identical(
    as_acc(df, timestamp = as.POSIXlt(df$ts), track_id = NULL, drop = TRUE),
    a
  )

  # Numeric timestamps are seconds since the epoch
  expect_identical(
    as_acc(df, timestamp = as.numeric(df$ts), track_id = NULL, drop = TRUE),
    a
  )

  # Date timestamps are read as UTC midnight, so a daily series is 1/86400 Hz
  df$ts <- as.Date("2024-01-01") + 0:3

  a <- as_acc(df, timestamp = df$ts, track_id = NULL, drop = TRUE)

  expect_equal(as.numeric(freqs(a)), signif(1 / 86400, 6))
  expect_identical(starts(a), as.POSIXct("2024-01-01", tz = "UTC"))
})

test_that("as_acc() treats NULL track_id as a single track", {
  # Two tracks that are contiguous in time, so the only thing keeping their
  # samples in separate bursts is the track ID.
  df <- data.frame(
    acceleration_x = as.numeric(1:6),
    acceleration_y = as.numeric(1:6),
    acceleration_z = as.numeric(1:6),
    ts = .as.POSIXct(seq(1, 1.5, by = 0.1)),
    id = rep(c("a", "b"), each = 3)
  )

  per_track <- as_acc(df, timestamp = df$ts, track_id = df$id, drop = TRUE)
  one_track <- as_acc(df, timestamp = df$ts, track_id = NULL, drop = TRUE)

  # One burst per track, versus a single burst spanning every sample
  expect_length(per_track, 2)
  expect_identical(purrr::map_int(bursts(per_track), nrow), c(3L, 3L))

  expect_length(one_track, 1)
  expect_identical(nrow(bursts(one_track)[[1]]), 6L)

  # A single supplied track ID is equivalent to NULL
  expect_identical(
    as_acc(df, timestamp = df$ts, track_id = rep("a", nrow(df)), drop = TRUE),
    one_track
  )
})
