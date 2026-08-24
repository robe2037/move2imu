#' Get or set names of columns that contain IMU data in a `move2`
#'
#' @description
#' Designate columns in a `move2` object that contain a given class of IMU
#' data. This allows you to flexibly store IMU data produced by `as_acc()`,
#' `as_mag()`, or `as_gyro()` in a column in a `move2` and access it
#' programmatically, without needing to hard-code column names.
#' 
#' - `mt_*_column()` returns the name of the column in the input `move2` that
#'   contains data from the indicated sensor.
#' - `mt_set_*_column()` sets the column that contains data from the indicated
#'   sensor.
#' 
#' @details
#' These functions behave similarly to move2's native helpers (e.g.
#' [move2::mt_time_column()]). They work by storing a character attribute
#' in the input `move2` object naming the relevant column of IMU data.
#' 
#' Be careful when modifying these columns with other methods. Operations 
#' that rename a column without updating the attribute (e.g.
#' [dplyr::rename()]) can create a mismatch between the stored column name
#' and the name of the column in the data themselves. The column name will
#' then need to be updated accordingly using `mt_set_*_column()`.
#' 
#' Further, some operations may drop these attributes, including
#' [dplyr::distinct()], [dplyr::summarize()], and [move2::mt_stack()].
#' 
#' @param x A `move2` object with an IMU vector column (for instance, produced
#'   by [as_acc()] or [as_mag()]).
#' @param value A string giving the name of the column in `x` that contains
#'   the extracted IMU data. The column must contain a vector of the
#'   corresponding sensor type (e.g. `acc`). 
#'
#' @returns
#' `mt_*_column()` returns a length-one character vector naming the designated
#' column.
#' 
#' `mt_set_*_column()` returns `x` with the corresponding attribute set to
#' `value`
#' 
#' @seealso [mt_acc()], [mt_mag()], [mt_gyro()] to extract the column values.
#' 
#'   [as_acc()], [as_mag()], [as_gyro()] to build IMU columns.
#'
#' @name mt_imu_column
#'
#' @examplesIf rlang::is_installed("move2")
#' alb <- albatrosses()
#' alb$acc <- as_acc(alb)
#' alb <- mt_set_acc_column(alb, "acc")
#' 
#' mt_acc_column(alb)
#' 
#' mt_acc(alb)[1:10]
NULL

#' @rdname mt_imu_column
#' @export
mt_acc_column <- function(x) {
  mt_imu_column(x, "acc")
}

#' @rdname mt_imu_column
#' @export
mt_mag_column <- function(x) {
  mt_imu_column(x, "mag")
}

#' @rdname mt_imu_column
#' @export
mt_gyro_column <- function(x) {
  mt_imu_column(x, "gyro")
}

#' @rdname mt_imu_column
#' @export
mt_set_acc_column <- function(x, value) {
  mt_set_imu_column(x, "acc", value)
}

#' @rdname mt_imu_column
#' @export
mt_set_mag_column <- function(x, value) {
  mt_set_imu_column(x, "mag", value)
}

#' @rdname mt_imu_column
#' @export
mt_set_gyro_column <- function(x, value) {
  mt_set_imu_column(x, "gyro", value)
}

#' Access parsed IMU data stored in a `move2`
#'
#' @description
#' Return the values contained in the currently designated column storing
#' data for the indicated sensor.
#' 
#' To set a column that contains data from a given sensor, use the corresponding
#' setting function. For instance, [mt_set_acc_column()].
#'
#' @details
#' The data contained in the designated IMU column for the given sensor must
#' be in the corresponding class. For instance, the `acc_column` set by
#' [mt_set_acc_column()] must contain an `acc` vector. `mt_acc()` validates
#' the data class before returning.
#'
#' @inheritParams mt_imu_column
#'
#' @returns 
#' An object inheriting from class `imu` containing the data in the indicated
#' column of `x`.
#' 
#' @seealso [mt_acc_column()], [mt_mag_column()], [mt_gyro_column()] to get or
#'   set the column that contains parsed data for a given sensor.
#'
#' @name mt_imu
#'
#' @examplesIf rlang::is_installed("move2")
#' alb <- albatrosses()
#' alb$acc <- as_acc(alb)
#' alb <- mt_set_acc_column(alb, "acc")
#'
#' mt_acc(alb)
NULL

#' @rdname mt_imu
#' @export
mt_acc <- function(x) {
  mt_imu(x, "acc")
}

#' @rdname mt_imu
#' @export
mt_mag <- function(x) {
  mt_imu(x, "mag")
}

#' @rdname mt_imu
#' @export
mt_gyro <- function(x) {
  mt_imu(x, "gyro")
}

# Get the name of an IMU column as stored in a move2's attribute for a given
# IMU sensor type.
mt_imu_column <- function(x, sensor, call = rlang::caller_env()) {
  assert_move2(x, call = call)
  
  nm <- imu_column_attr(sensor)
  col <- attr(x, nm, exact = TRUE)

  if (is.null(col)) {
    setter <- paste0("mt_set_", sensor, "_column")
    cli::cli_abort(
      c(
        "No {.field {nm}} detected.",
        i = "Set one with {.help [{.fn {setter}}](move2imu::{setter})}."
      ),
      call = call
    )
  }

  col
}

# Designate a new IMU column. Sets an attribute on the input move2 that 
# defines the name of the column containing a particular class of IMU data.
# Validates that the column exists and contains the correct IMU data class.
mt_set_imu_column <- function(x, sensor, value, call = rlang::caller_env()) {
  assert_move2(x, call = call)

  if (!rlang::is_string(value)) {
    cli::cli_abort(
      "{.arg value} must be a length-1 character.",
      call = call
    )
  }

  if (!rlang::has_name(x, value)) {
    cli::cli_abort(
      "{.arg value} must be the name of a column in {.arg x}.",
      call = call
    )
  }

  col <- x[[value]]

  if (!inherits(col, sensor)) {
    cli::cli_abort(
      "Column {.val {value}} must be of class {.cls {sensor}}, not {.cls {class(col)[1]}}.",
      call = call
    )
  }

  attr(x, imu_column_attr(sensor)) <- value
  
  x
}

# Access the values stored in a designated IMU column.
# Confirms that the attribute column name matches the existing colnames and
# contains expected class of IMU object.
mt_imu <- function(x, sensor, call = rlang::caller_env()) {
  col <- mt_imu_column(x, sensor, call = call)
  nm <- imu_column_attr(sensor)

  if (!rlang::has_name(x, col)) {
    cli::cli_abort(
      "{.field {nm}} {.val {col}} does not exist in {.arg x}.",
      call = call
    )
  }

  vals <- x[[col]]

  if (!inherits(vals, sensor)) {
    cli::cli_abort(
      "Column {.val {col}} must be of class {.cls {sensor}}, not {.cls {class(vals)[1]}}.",
      call = call
    )
  }

  vals
}

# Attribute name storing the column that holds a given IMU data type in a
# move2 object. Follows naming convention used by move2's track ID and time
# columns.
imu_column_attr <- function(sensor) {
  paste0(sensor, "_column")
}

# accessors for IMU columns is restricted to use in move2 objects
# for consistency with similar columns (time, track ID)
assert_move2 <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "move2")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls move2} object.",
      call = call
    )
  }
  invisible(x)
}
