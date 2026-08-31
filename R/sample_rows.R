#' Identify rows containing IMU data
#'
#' @description
#' These functions return a logical vector flagging the rows of an input
#' `move2` or `data.frame` that contain sample data for the specified sensor.
#' These are the rows that will be used to build IMU bursts when calling
#' `as_acc()`, `as_mag()`, or `as_gyro()`.
#'
#' @details
#' If `x` has data in more than one active IMU column set, `*_sample_rows()`
#' will return `TRUE`. However, these rows cannot be parsed by [as_acc()],
#' [as_mag()], etc. as they contain duplicated IMU data. To ensure that
#' `*_sample_rows()` only considers certain column sets, use the `colset`
#' argument.
#'
#' If no active colset is detected (e.g. data with only GPS records),
#' `*_sample_rows()` returns `FALSE` for all rows.
#'
#' For expanded-format data (where multiple rows compose a single burst)
#' all rows that contain IMU data are flagged `TRUE`. However, the output
#' of `as_*()` will not necessarily return bursts at each of these locations,
#' as multiple of these rows will be combined into a single burst.
#'
#' @param x A `move2` or `data.frame`.
#' @param colset An `imu_colset` object or list of `imu_colset` objects
#'   specifying the columns to check for IMU data. By default, all active
#'   colsets detected in `x` are considered (see [active_acc_colsets()]).
#'
#' @returns A logical vector the same length as `nrow(x)`. `TRUE` values
#'   indicate rows where IMU data is present under at least one active colset.
#'
#' @seealso [as_acc()], [as_mag()], [as_gyro()] to extract IMU data from a
#'   `move2` or `data.frame`.
#'
#'   [active_acc_colsets()], [active_mag_colsets()], [active_gyro_colsets()]
#'   to inspect which colsets are detected in `x`.
#'
#' @name sample_rows
#'
#' @examplesIf rlang::is_installed("move2")
#' alb <- albatrosses()
#'
#' head(acc_sample_rows(alb))
#'
#' # Filter to rows with acc data without building bursts
#' nrow(alb[acc_sample_rows(alb), ])
NULL

#' @rdname sample_rows
#' @export
acc_sample_rows <- function(x, colset = NULL) {
  imu_sample_rows(x, sensor = "acc", colset = colset)
}

#' @rdname sample_rows
#' @export
mag_sample_rows <- function(x, colset = NULL) {
  imu_sample_rows(x, sensor = "mag", colset = colset)
}

#' @rdname sample_rows
#' @export
gyro_sample_rows <- function(x, colset = NULL) {
  imu_sample_rows(x, sensor = "gyro", colset = colset)
}

# Flag each row that has IMU data for the given sensor and colset.
# Returns TRUE if any valid colset has data. Does not check for duplicate
# IMU data in a given row. Returns FALSE if no valid colsets are found.
imu_sample_rows <- function(x, sensor, colset = NULL) {
  colsets <- tryCatch(
    parse_colsets(x, colset, sensor, quiet = TRUE),
    move2imu_no_active_colset = function(e) NULL
  )

  out <- logical(nrow(x))

  if (is.null(colsets)) {
    return(out)
  }

  vals <- unique(unlist(
    lapply(colsets, function(cs) which_imu_vals(x, colset = cs))
  ))

  out[vals] <- TRUE
  out
}
