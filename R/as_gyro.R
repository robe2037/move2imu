#' Convert an object to a `gyro` vector
#'
#' @description
#' Extract gyroscope data from a `move2` or `data.frame` and convert to a
#' `gyro` vector.
#'
#' Data are extracted from the object's [active_gyro_colsets()].
#'
#' @inheritParams as_acc
#' @param x A `move2` or `data.frame` containing gyroscope data. A `move2`
#'   will typically be loaded from disk with [move2::mt_read()] or downloaded
#'   using [move2::movebank_download_study()].
#' @param colset An `imu_colset` object or list of `imu_colset` objects
#'   specifying the columns of `x` that contain gyroscope data. By default,
#'   constructs bursts for all column sets that are detected in `x` that also
#'   contain data (see [active_gyro_colsets()]).
#'
#'   Several common colsets are listed under [movebank_gyro_colsets()]. To
#'   specify a custom set of columns, use [imu_colset()].
#'
#' @inherit as_acc details
#'
#' @return An object of class `gyro` inheriting from class `imu`.
#'
#' @seealso [movebank_gyro_colsets()] for supported gyroscope column sets
#'   in Movebank.
#'
#' @export
#'
#' @examples
#' # Example gyroscope data, with one sample per row
#' g <- data.frame(
#'   angular_velocity_x = c(1, 2, 3, 4),
#'   angular_velocity_y = c(5, 6, 7, 8),
#'   angular_velocity_z = c(9, 10, 11, 12),
#'   timestamp = as.POSIXct("2024-01-01", tz = "UTC") + seq(0, 0.3, by = 0.1),
#'   id = "tag_1"
#' )
#'
#' # Samples are combined into a 10 Hz burst, placed at the row where it starts
#' gyro <- as_gyro(g, timestamp = g$timestamp, track_id = g$id)
#'
#' # Each burst holds a column of samples per recorded axis
#' bursts(gyro)[[1]]
#'
#' # Output is index-matched to the input so the result can be easily attached:
#' g$gyro <- gyro
#'
#' # Samples can also be stored as space-delimited strings with associated
#' # axis and frequency metadata:
#' g_compact <- data.frame(
#'   gyro_axes = "XYZ",
#'   gyro_freq = 10,
#'   gyro_raw = c(
#'     "1 5 9 2 6 10 3 7 11 4 8 12",
#'     "2 6 10 3 7 11 4 8 12 5 9 13"
#'   ),
#'   timestamp = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 60),
#'   id = "tag_1"
#' )
#'
#' # If column names are not automatically recognized, provide your own
#' # column set:
#' as_gyro(
#'   g_compact,
#'   colset = imu_colset(
#'     bursts = "gyro_raw",
#'     axes = "gyro_axes",
#'     frequency = "gyro_freq"
#'   ),
#'   timestamp = g_compact$timestamp,
#'   track_id = g_compact$id
#' )
#'
#' @examplesIf rlang::is_installed(c("move2", "sf"))
#' # For a `move2`, timestamps and track IDs come from the object's metadata.
#' # Build a sample move2 with empty geometries:
#' g2 <- move2::mt_as_move2(
#'   sf::st_sf(g, geometry = sf::st_sfc(rep(list(sf::st_point()), nrow(g)))),
#'   time_column = "timestamp",
#'   track_id_column = "id"
#' )
#'
#' as_gyro(g2)
as_gyro <- function(x, ...) {
  UseMethod("as_gyro")
}

#' @rdname as_gyro
#' @export
as_gyro.default <- function(x, ...) {
  vctrs::vec_cast(x, new_imu("gyro"))
}

#' @rdname as_gyro
#' @export
as_gyro.move2 <- function(x,
                          colset = NULL,
                          min_freq = 0,
                          freq_tol = 1e-2,
                          gap_tol = 1e-6,
                          merge_continuous = TRUE,
                          drop = FALSE,
                          ...) {
  check_move2_dots(...)
  rlang::check_dots_empty()

  as_imu(
    x,
    sensor = "gyro",
    colset = colset,
    min_freq = min_freq,
    freq_tol = freq_tol,
    gap_tol = gap_tol,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}
#' @rdname as_gyro
#' @export
as_gyro.data.frame <- function(x,
                               timestamp,
                               track_id,
                               colset = NULL,
                               min_freq = 0,
                               freq_tol = 1e-2,
                               gap_tol = 1e-6,
                               merge_continuous = TRUE,
                               drop = FALSE,
                               ...) {
  rlang::check_dots_empty()
  as_imu(
    x,
    sensor = "gyro",
    colset = colset,
    min_freq = min_freq,
    freq_tol = freq_tol,
    gap_tol = gap_tol,
    merge_continuous = merge_continuous,
    drop = drop,
    timestamp = timestamp,
    track_id = track_id,
    ...
  )
}
