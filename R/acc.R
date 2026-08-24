#' Create an IMU vector
#'
#' @description
#' Construct an `acc`, `mag`, or `gyro` vector from a list of burst
#' matrices and the sampling frequency at which they were recorded.
#'
#' The three constructors share the same signature and semantics —
#' they differ only in the sensor type recorded by the resulting
#' vector and in the units typically attached to its samples
#' (acceleration, magnetic field strength, angular velocity).
#'
#' @param bursts A list of matrices, one per burst. Each matrix has one
#'   row per sample and named columns drawn from `"X"`, `"Y"`, and
#'   `"Z"` indicating which axes were recorded. Entries can be `NULL`
#'   to represent missing bursts.
#' @param frequency Sampling frequency of the recordings in `bursts`,
#'   either length 1 (recycled to all bursts) or the same length as
#'   `bursts`. Frequencies with compatible [units][units::units] are converted
#'   to Hz internally. If no units are specified, the frequency is assumed to
#'   be in Hz.
#' @param start Optional; burst start times. Either length 1 (recycled), the
#'   same length as `bursts`, or `NULL` if start times are unknown.
#'
#'   Accepts `POSIXct`, `POSIXlt`, `Date`, or numeric values. `Date` objects
#'   are treated as being recorded at midnight, UTC. Numeric values are
#'   interpreted as the number of seconds since `1970-01-01 00:00:00 UTC`.
#'   Inputs are all converted to `POSIXct`. For inputs with a specified time
#'   zone, it is retained.
#'
#' @returns An IMU vector of the corresponding sensor type (`acc`,
#'   `mag`, or `gyro`).
#'
#' @name imu_constructors
#' @examples
#' # Accelerometer: a single burst on X, Y, Z at 40 Hz
#' acc(
#'   bursts = list(cbind(
#'     X = sin(1:30 / 10),
#'     Y = cos(1:30 / 10),
#'     Z = 1:30 / 10
#'   )),
#'   frequency = units::as_units(40, "Hz")
#' )
#'
#' # Multiple bursts with different axes — combined into one vector
#' acc(
#'   bursts = list(
#'     cbind(X = sin(1:20 / 10), Y = cos(1:20 / 10)),
#'     cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3))
#'   ),
#'   frequency = units::as_units(30, "Hz")
#' )
#'
#' # The same API for magnetometer and gyroscope data:
#' mag(
#'   bursts = list(cbind(X = 1:10, Y = 11:20, Z = 21:30)),
#'   frequency = units::as_units(10, "Hz")
#' )
#'
#' gyro(
#'   bursts = list(cbind(X = sin(1:20 / 10), Y = cos(1:20 / 10))),
#'   frequency = units::as_units(50, "Hz")
#' )
NULL

#' @rdname imu_constructors
#' @order 1
#' @export
acc <- function(bursts = list(),
                frequency = units::set_units(double(), "Hz"),
                start = NULL) {
  imu("acc", bursts = bursts, frequency = frequency, start = start)
}

#' @export
#' @rdname imu-predicates
#' @order 1
is_acc <- function(x) {
  inherits(x, "acc")
}

#' @export
vec_ptype2.acc.acc <- function(x, y, ...) imu_ptype2(x, y, ...)

#' @export
vec_cast.acc.acc <- function(x, to, ...) imu_cast(x, to, ...)
