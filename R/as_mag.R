#' Convert an object to a `mag` vector
#'
#' @description
#' Extract magnetometer data from a `move2` or `data.frame` and convert to a
#' `mag` vector.
#'
#' Data are extracted from the object's [active_mag_colsets()].
#'
#' @inheritParams as_acc
#' @param x A `move2` or `data.frame` containing magnetometer data. A `move2`
#'   will typically be loaded from disk with [move2::mt_read()] or downloaded
#'   using [move2::movebank_download_study()].
#' @param colset An `imu_colset` object or list of `imu_colset` objects
#'   specifying the columns of `x` that contain magnetometer data. By default,
#'   constructs bursts for all column sets that are detected in `x` that also
#'   contain data (see [active_mag_colsets()]).
#'
#'   Several common colsets are listed under [movebank_mag_colsets()]. To
#'   specify a custom set of columns, use [imu_colset()].
#'
#' @inherit as_acc details
#'
#' @return An object of class `mag` inheriting from class `imu`.
#'
#' @seealso [movebank_mag_colsets()] for supported magnetometer column sets
#'   in Movebank.
#'
#' @export
#'
#' @examples
#' # Example magnetometer data, with each burst stored as a single string
#' m <- data.frame(
#'   magnetic_field_axes = "XYZ",
#'   magnetic_field_sampling_frequency_per_axis = 10,
#'   magnetic_fields_raw = c(
#'     "1 5 9 2 6 10 3 7 11 4 8 12",
#'     "2 6 10 3 7 11 4 8 12 5 9 13"
#'   ),
#'   timestamp = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 60),
#'   id = "tag_1"
#' )
#'
#' mag <- as_mag(m, timestamp = m$timestamp, track_id = m$id)
#'
#' mag
#'
#' # Each burst holds a column of samples per recorded axis
#' bursts(mag)[[1]]
#'
#' # Output is index-matched to the input so the result can be easily attached:
#' m$mag <- mag
#'
#' # Data can also be provided with one sample per row:
#' m_expanded <- data.frame(
#'   mag_x = c(1, 2, 3, 4),
#'   mag_y = c(5, 6, 7, 8),
#'   mag_z = c(9, 10, 11, 12),
#'   timestamp = as.POSIXct("2024-01-01", tz = "UTC") + seq(0, 0.3, by = 0.1),
#'   id = "tag_1"
#' )
#'
#' # If column names are not identified automatically, specify your
#' # own column set:
#' as_mag(
#'   m_expanded,
#'   colset = imu_colset(x = "mag_x", y = "mag_y", z = "mag_z"),
#'   timestamp = m_expanded$timestamp,
#'   track_id = m_expanded$id
#' )
#'
#' @examplesIf rlang::is_installed(c("move2", "sf"))
#' # For a `move2`, timestamps and track IDs come from the object's metadata.
#' # Build a sample move2 with empty geometries:
#' m2 <- move2::mt_as_move2(
#'   sf::st_sf(m, geometry = sf::st_sfc(rep(list(sf::st_point()), nrow(m)))),
#'   time_column = "timestamp",
#'   track_id_column = "id"
#' )
#'
#' as_mag(m2)
as_mag <- function(x, ...) {
  UseMethod("as_mag")
}

#' @rdname as_mag
#' @export
as_mag.default <- function(x, ...) {
  vctrs::vec_cast(x, new_imu("mag"))
}

#' @rdname as_mag
#' @export
as_mag.move2 <- function(x,
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
    sensor = "mag",
    colset = colset,
    min_freq = min_freq,
    freq_tol = freq_tol,
    gap_tol = gap_tol,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}
#' @rdname as_mag
#' @export
as_mag.data.frame <- function(x,
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
    sensor = "mag",
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
