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

# aliases for as_*() for data.frame inputs. Simplifies code in the test suite
# by not needing to specify `timestamp` and `track_id` at all call sites.
as_mag_df <- function(d, ...) {
  as_mag(d, timestamp = d$timestamp, track_id = d$id, ...)
}

as_gyro_df <- function(d, ...) {
  as_gyro(d, timestamp = d$timestamp, track_id = d$id, ...)
}

as_acc_df <- function(d, ...) {
  as_acc(d, timestamp = d$timestamp, track_id = d$id, ...)
}

# Convert albatrosses() and gulls() move2 to data.frame counterparts.
#
# This removes move2 class, sf attributes, integer64 and sfc columns (both
# handled by move2) and renames the track_id column for simplicity in tests.
move2_to_df <- function(x) {
  y <- unclass(x)

  # integer64 columns deserialize, but require bit64 (which comes with move2) to
  # be compared correctly between move2 and data.frame outputs. We remove them.
  y <- y[!vapply(y, inherits, logical(1), "integer64")]
  y <- y[!vapply(y, inherits, logical(1), "sfc")]

  # Set explicit names for ID cols for consistent access
  names(y)[names(y) == attr(x, "time_column")] <- "timestamp"
  names(y)[names(y) == attr(x, "track_id_column")] <- "id"

  # Drop the move2 attributes (e.g. `track_data`)
  attributes(y) <- list(
    names = names(y),
    row.names = .set_row_names(length(y[[1]])),
    class = "data.frame"
  )

  y
}

# Convert a data.frame fixture into the move2 it stands in for. Every fixture is
# defined as a data.frame, so this is the single place that conversion happens --
# only tests actually about the move2 entry point need it.
df_to_move2 <- function(d) {
  move2::mt_as_move2(
    d,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
}

albatrosses_df <- function() {
  move2_to_df(read_example("albatrosses"))
}

gulls_df <- function() {
  move2_to_df(read_example("gulls"))
}

# Fabricated mag/gyro fixtures. Each sensor has an expanded- and a
# compact-format variant, built as a plain data.frame so it can be pushed
# through the `data.frame` entry point without move2 installed. The `move2`
# counterpart of each is derived from the same data.frame, so both entry points
# are exercised against one definition of the fixture.
#
# Uses the column names expected by `mag_colset_xyz()` (i.e.
# `magnetic_field_{x,y,z}`). Two bursts at 10 Hz, separated by a gap so
# `as_mag()` splits them.
mag_example_expanded <- function() {
  data.frame(
    id = "exp",
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
mag_example_compact <- function() {
  data.frame(
    id = "comp",
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
gyro_example_expanded <- function() {
  data.frame(
    id = "exp",
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
gyro_example_compact <- function() {
  data.frame(
    id = "comp",
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

# Example expanded-format acc data built from an arbitrary vector of
# timestamps (seconds relative to `origin`). Used to exercise burst splitting,
# the tolerance, and span-based frequency in `as_acc()`.
acc_example_expanded <- function(ts, id = 1) {
  data.frame(
    id = id,
    acceleration_x = seq_along(ts),
    acceleration_y = seq_along(ts),
    acceleration_z = seq_along(ts),
    timestamp = as.POSIXct(ts, tz = "UTC", origin = "2020-01-01"),
    x = 1,
    y = 1
  )
}

# Example compact-format acc data built from an arbitrary timestamp vector.
#
# Each row holds one 20-sample XYZ burst spanning a hundredth of a second, so
# rows stay separate bursts at any realistic spacing. Merging of adjacent
# compact bursts is covered in `test-split_merge.R`.
acc_example_compact <- function(ts, id = "comp") {
  data.frame(
    id = id,
    eobs_acceleration_axes = "XYZ",
    eobs_acceleration_sampling_frequency_per_axis = 2000,
    eobs_accelerations_raw = paste0(rep(1:20, each = 3), collapse = " "),
    timestamp = ts,
    x = 1,
    y = 1
  )
}
