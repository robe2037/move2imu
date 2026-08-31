#' Example `acc` vector
#'
#' A simple `acc` vector for use in examples.
#'
#' @returns An `acc` vector with two bursts.
#'
#' @keywords internal
#' @export
acc_example <- function() {
  acc(
    list(
      cbind(X = sin(1:30 / 10), Y = cos(1:30 / 10), Z = 1),
      cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3), Z = 1)
    ),
    frequency = units::set_units(c(20, 20), "Hz"),
    start = as.POSIXct(c(0, 10), origin = "1970-01-01", tz = "UTC")
  )
}

# Coerce a timestamp vector to POSIXct. The single conversion point for every
# timestamp a user or a `move2` hands the package, so that interior code and
# `imu` storage only ever see POSIXct and a time zone is always well defined.
#
# The accepted set is everything that already denotes an instant:
#
#   POSIXct  kept as-is
#   POSIXlt  the same instant and zone, in the non-canonical layout
#   Date     midnight UTC
#   numeric  seconds since 1970-01-01, UTC (also what move2 means by a numeric
#            time column)
#   NA       R's missing marker, carried through as a missing instant
#
# `Date` is converted by hand rather than with `as.POSIXct()`: that method only
# began setting a "UTC" time zone in R 4.3, and silently ignores a `tz` argument
# before that, so relying on it would yield local-time output on older R.
#
# returns TRUE for anything `timestamp_to_POSIXct()` accepts.
#
# `inherits()` rather than a bare `is.numeric()`: difftime and units objects are
# numeric underneath, and reading their value as seconds would drop the unit.
is_timestamp <- function(x) {
  inherits(x, c("POSIXct", "POSIXlt", "Date")) ||
    (is.logical(x) && all(is.na(x))) ||
    (is.numeric(x) && !inherits(x, c("difftime", "units")))
}

# `freq_tol` is a relative tolerance, so it should not have units. But alongside
# other tolerance arguments that do take units this may be a source of user
# error. Simple guard to avoid this potential confusion.
check_freq_tol <- function(freq_tol, call = rlang::caller_env()) {
  if (inherits(freq_tol, "units")) {
    cli::cli_abort(
      "{.arg freq_tol} must be a bare numeric value, not a {.cls units} object.",
      call = call
    )
  }

  if (as.numeric(freq_tol) < 0) {
    cli::cli_abort(
      "{.arg freq_tol} must be greater than or equal to 0.",
      call = call
    )
  }

  invisible(NULL)
}

timestamp_to_POSIXct <- function(x,
                                 arg = rlang::caller_arg(x),
                                 call = rlang::caller_env()) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }

  if (inherits(x, "POSIXlt")) {
    return(as.POSIXct(x))
  }

  if (inherits(x, "Date")) {
    return(as.POSIXct(unclass(x) * 86400, origin = "1970-01-01", tz = "UTC"))
  }

  # A bare `NA` is unclassed; base `as.POSIXct()` special-cases it the same way
  if (is.logical(x) && all(is.na(x))) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (!is_timestamp(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a timestamp, not {.cls {class(x)[1]}}.",
        "i" = "Supply a {.cls POSIXct}, {.cls POSIXlt}, {.cls Date}, or a bare numeric value."
      ),
      call = call
    )
  }

  as.POSIXct(as.double(x), origin = "1970-01-01", tz = "UTC")
}

# Helper to snap a computed frequency to a stable precision.
#
# Both parsing (expanded-format data) and merging derive a burst's frequency
# from its timestamp span: (n_samples - 1) / (last_time - first_time). POSIXct
# stores time as seconds since 1970, but floating point representation
# can only resolve so much precision for these large numbers. This leads to
# small irregularities in frequencies after derivation.
#
# This noise also scales with the sampling frequency. We use signif() to
# avoid applying the uniform correction of round(), which would not account
# for this fact. 6 significant figures clears the noise floor for bursts in
# normal frequency ranges (up to a few hundred Hz). Users can otherwise
# do their own normalization post-hoc if this is not sufficient to make
# their bursts uniform in frequency due to noise.
snap_freq <- function(x, digits = 6) {
  signif(x, digits = digits)
}

# Parsed form of the canonical frequency unit. Parsing a unit string is the
# dominant cost of a units operation, so the target unit is parsed once here
# and reused rather than re-parsed from "Hz" on every conversion.
HZ_UNIT <- units(units::as_units("Hz"))

# Coerce a frequency to Hz. Used so interior code can easily coerce values to Hz
# for operations that are units-sensitive. Hz is the canonical storage format
# for IMU vector objects.
as_hz <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!inherits(x, "units")) {
    return(units::set_units(x, HZ_UNIT, mode = "standard"))
  }

  # Return input if identical to avoid unnecessary units calls
  if (identical(units(x), HZ_UNIT)) {
    return(x)
  }

  if (!units::ud_are_convertible(units::deparse_unit(x), "Hz")) {
    cli::cli_abort(
      "{.arg {arg}} must be convertible to a frequency unit (Hz).",
      call = call
    )
  }

  units::set_units(x, HZ_UNIT, mode = "standard")
}

# Absolute floating-point noise floor for POSIXct-derived time differences, in
# seconds. POSIXct is a double count of seconds since 1970; one ULP at a
# contemporary epoch (~1.77e9 s) is ~4e-7 s, so a difference of two timestamps
# carries ~sub-microsecond noise regardless of the sampling frequency. Relative
# frequency comparisons (which divide by the sample period) inflate this noise
# at high sampling frequencies, so those comparisons are backstopped with this
# floor: a deviation must exceed both the relative `freq_tol` AND this absolute
# floor to count as a real frequency change. This keeps sub-microsecond
# timestamp jitter from being mistaken for a frequency change on fast (e.g.
# >1 kHz) data.
#
# The floor (1e-6 s) sits a few times above the raw ULP (~4e-7 s) to absorb
# noise accumulated across a burst's timestamps. The trade-off is that a genuine
# frequency change between two adjacent, gap-free regimes whose sample periods
# differ by less than the floor will not be split (it merges into one burst with
# a blended frequency). This is only an issue when `freq_tol / f` drops below the
# floor -- i.e. above roughly `freq_tol * 1e6` Hz -- well beyond normal IMU
# sampling rates, and near the resolution POSIXct can represent anyway.
fp_time_floor <- 1e-6

# From dplyr
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

# Check if a scalar value is NULL or NA
null_or_na <- function(x) {
  is.null(x) || rlang::is_na(x)
}

# Return the first scalar value in `...` that is not NULL or NA.
# Need to handle NA because NA values may be passed via data.frame col vals in
# as_acc_calibration()
first_valid <- function(...) {
  for (v in list(...)) {
    if (!is.null(v) && !rlang::is_na(v)) {
      return(v)
    }
  }
  NULL
}
