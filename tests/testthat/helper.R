# `as.POSIXct()` only gained a default `origin` in R 4.3, and this package
# supports R >= 4.1. Calls must have an explicit origin to successfully pass
# tests on old R releases.
.as.POSIXct <- function(x, tz = "UTC") {
  as.POSIXct(x, origin = "1970-01-01", tz = tz)
}

# Apply a single calibration to a single burst. Improves test legibility.
transform_burst <- function(burst, calibration, ...) {
  burst_transformer(calibration, ...)(burst)
}

acc_burst_example <- function(x = NULL, y = NULL, z = NULL) {
  vctrs::vec_size_common(x, y, z)
  new_burst_list(list(do.call(cbind, list(X = x, Y = y, Z = z))), "acc")
}

# Call a sensor's `data.frame` method on a fixture, supplying the `timestamp`
# and `track_id` it requires from the fixture's own columns. Improves
# legibility where the data.frame entry point is the thing under test.
as_mag_df <- function(d, ...) {
  as_mag(d, timestamp = d$timestamp, track_id = d$id, ...)
}

as_gyro_df <- function(d, ...) {
  as_gyro(d, timestamp = d$timestamp, track_id = d$id, ...)
}

# Fabricated mag/gyro fixtures. Each sensor has an expanded- and a
# compact-format variant, built as a plain data.frame so it can be pushed
# through the `data.frame` entry point without move2 installed. The `move2`
# counterpart of each is derived from the same data.frame, so both entry points
# are exercised against one definition of the fixture.

# Uses the column names expected by `mag_colset_xyz()` (i.e.
# `magnetic_field_{x,y,z}`). Two bursts at 10 Hz, separated by a gap so
# `as_mag()` splits them.
mag_example_expanded_df <- function(id = "expanded") {
  data.frame(
    id = id,
    magnetic_field_x = as.numeric(1:10),
    magnetic_field_y = as.numeric(11:20),
    magnetic_field_z = as.numeric(21:30),
    timestamp = .as.POSIXct(c(seq(1, 1.4, by = 0.1), seq(3, 3.4, by = 0.1))),
    x = 1,
    y = 1
  )
}

# Uses the column names expected by `mag_colset_raw()`. Two XYZ bursts at 10 Hz,
# separated by a gap so that `merge_imu` does not collapse them.
mag_example_compact_df <- function(id = "compact") {
  data.frame(
    id = id,
    magnetic_field_axes = "XYZ",
    magnetic_field_sampling_frequency_per_axis = 10,
    magnetic_fields_raw = c(
      paste0(rep(1:5, each = 3), collapse = " "),
      paste0(rep(6:10, each = 3), collapse = " ")
    ),
    timestamp = .as.POSIXct(c(10, 30)),
    x = 1,
    y = 1
  )
}

# Uses the column names expected by `gyro_colset_xyz()` (i.e.
# `angular_velocity_{x,y,z}`). Two bursts at 10 Hz, separated by a gap so
# `as_gyro()` splits them.
gyro_example_expanded_df <- function(id = "expanded") {
  data.frame(
    id = id,
    angular_velocity_x = as.numeric(1:10),
    angular_velocity_y = as.numeric(11:20),
    angular_velocity_z = as.numeric(21:30),
    timestamp = .as.POSIXct(c(seq(1, 1.4, by = 0.1), seq(3, 3.4, by = 0.1))),
    x = 1,
    y = 1
  )
}

# Uses the column names expected by `gyro_colset_raw()`. Two XYZ bursts at
# 10 Hz, separated by a gap so that `merge_imu` does not collapse them.
gyro_example_compact_df <- function(id = "compact") {
  data.frame(
    id = id,
    gyroscope_axes = "XYZ",
    gyroscope_sampling_frequency_per_axis = 10,
    angular_velocities_raw = c(
      paste0(rep(1:5, each = 3), collapse = " "),
      paste0(rep(6:10, each = 3), collapse = " ")
    ),
    timestamp = .as.POSIXct(c(10, 30)),
    x = 1,
    y = 1
  )
}

mag_example_expanded <- function() {
  move2::mt_as_move2(
    mag_example_expanded_df(),
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
}

mag_example_compact <- function(id = "compact") {
  move2::mt_as_move2(
    mag_example_compact_df(),
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
}

gyro_example_expanded <- function(id = "expanded") {
  move2::mt_as_move2(
    gyro_example_expanded_df(),
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
}

gyro_example_compact <- function(id = "compact") {
  move2::mt_as_move2(
    gyro_example_compact_df(),
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
}

# Fabricated expanded-format acc move2 built from an arbitrary vector of
# timestamps (seconds relative to `origin`). Used to exercise burst splitting,
# the tolerance, and span-based frequency in `as_acc()`.
expanded_acc <- function(ts, id = 1) {
  t <- data.frame(
    id = id,
    acceleration_x = seq_along(ts),
    acceleration_y = seq_along(ts),
    acceleration_z = seq_along(ts),
    timestamp = as.POSIXct(ts, tz = "UTC", origin = "2020-01-01"),
    x = 1, y = 1
  )
  move2::mt_as_move2(
    t,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
}

# Build sample data source to simulate case where compact-format data is actually
# continuous, as the bursts are adjacent in time.
albatrosses_messy <- function() {
  d <- albatrosses()
  d <- d[d$sensor_type_id == 2365683, ]

  # Fake time series with some records that represent continuous data along
  # with some longer gaps
  ts1 <- seq(
    min(move2::mt_time(d)),
    by = "12 s",
    length.out = 10
  )

  ts2 <- seq(
    ts1[length(ts1)] + units::as_difftime(units::set_units(5, "min")),
    by = "5 min",
    length.out = 2
  )

  ts3 <- seq(
    ts2[length(ts2)] + units::as_difftime(units::set_units(5, "min")),
    by = "12 s",
    length.out = nrow(d) - (length(ts1) + length(ts2))
  )

  move2::mt_time(d) <- c(ts1, ts2, ts3)

  # Should not collapse continuous data across different axes
  levels(d$eobs_acceleration_axes) <- c("XY", "XZ", "XYZ")
  d[c(4, 5, 6), "eobs_acceleration_axes"] <- "XYZ"

  d[c(44, 45), "eobs_acceleration_axes"] <- "XZ"

  # Adjust so that the duration of the burst is the same:
  d[c(4, 5, 6), "eobs_accelerations_raw"] <- paste0(
    d[c(4, 5, 6), ][["eobs_accelerations_raw"]], " ", paste0(rep(1, 60), collapse = " ")
  )

  # Should not collapse continuous data across different frequencies
  d[c(31, 32), "eobs_acceleration_sampling_frequency_per_axis"] <- units::set_units(10, "Hz")
  d[c(31, 32), "eobs_accelerations_raw"] <- paste0(
    d[c(31, 32), ][["eobs_accelerations_raw"]], " ", paste0(rep(1, 120), collapse = " ")
  )

  d
}

# Fabricated compact-format acc move2 built from an arbitrary timestamp vector.
#
# `timestamp` becomes the time column verbatim -- callers wanting POSIXct pass
# POSIXct. That is deliberate rather than a convenience left undone: the
# boundary-normalization tests have to hand `as_acc()` a raw `numeric` or `Date`
# column, both of which move2 permits.
#
# Each row holds one 20-sample XYZ burst. The default `frequency` leaves a gap
# between rows, while a frequency that makes each burst span the gap exercises
# merging.
compact_acc <- function(timestamp, frequency = 2000, id = "compact") {
  t <- data.frame(
    id = id,
    eobs_acceleration_axes = "XYZ",
    eobs_acceleration_sampling_frequency_per_axis = frequency,
    eobs_accelerations_raw = paste0(rep(1:20, each = 3), collapse = " "),
    timestamp = timestamp,
    x = 1, y = 1
  )

  move2::mt_as_move2(
    t,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
}
