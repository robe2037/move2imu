#' Summarize an IMU vector
#'
#' @description
#' Provides a diagnostic overview of an IMU vector (`acc`, `mag`, or `gyro`).
#' 
#' Includes information about data time range, axes, sampling frequencies, 
#' burst lengths, inter-burst intervals, and sample values.
#' 
#' @details
#' The intervals shown are the gaps between consecutive bursts (end of one to 
#' the start of the next), computed in vector order (see [burst_intervals()]). 
#' If bursts come from different sources (e.g. different tags), there may be 
#' noticeable interval artifacts between some bursts where sources change.
#'
#' Note that the sample-value quantiles consider all axes and units
#' simultaneously.
#'
#' @param object An `imu` object.
#' @param ... Ignored.
#'
#' @returns An `imu_summary` object
#'
#' @name imu_summary
#' @examples
#' a <- acc_example()
#' summary(a)
NULL

#' @rdname imu_summary
#' @export
summary.imu <- function(object, ...) {
  x <- object[!is.na(object)]

  out <- list(
    sensor = class(object)[1],
    n = length(object),
    n_na = sum(is.na(object))
  )

  if (length(x) == 0) {
    return(new_imu_summary(out))
  }

  br <- bursts(x)

  # Axis combos (e.g. "XYZ", "XY")
  axis_combos <- vapply(br, function(b) paste(colnames(b), collapse = ""), "")
  out$axes <- sort(table(axis_combos), decreasing = TRUE)

  # Frequencies
  f <- freqs(x)
  out$freq_unit <- units::deparse_unit(f)
  out$freqs_rng <- .range(as.numeric(f))

  # Samples per burst
  sm <- n_samples(x)
  out$samples_rng <- .range(sm)

  # Freq unit is constant. Output is in seconds. Get the seconds per freq unit
  # to avoid per-element units processing.
  sec_per_period <- as.numeric(
    units::set_units(1 / units::set_units(1, units(f), mode = "standard"), "s")
  )

  # Durations. Don't use burst_dur() to avoid recomputing samples, etc.
  dur_s <- sm / as.numeric(f) * sec_per_period
  out$dur_unit <- "s"
  out$durations_rng <- .range(dur_s)

  st <- starts(x)
  st_valid <- st[!is.na(st)]

  if (length(st_valid) > 0) {
    out$start_range <- range(st_valid)
  } else {
    out$start_range <- NULL
  }

  out$start_tz <- attr(st_valid, "tzone") %||% ""

  # Inter-burst intervals. Don't use burst_intervals() to avoid recomputing
  # components. If bursts come from different sources some intervals will be
  # artifacts, but if generated from a move2 these should be limited as the
  # tracks should already be ordered.
  stn <- as.numeric(st)
  gap <- stn - c(NA_real_, utils::head(stn, -1L))
  gap <- gap - c(NA_real_, utils::head(dur_s, -1L))
  out$intervals_q <- .quantile(gap[!is.na(gap)])

  # Units
  unit_strs <- imu_units(x)
  out$imu_units <- unique(stats::na.omit(unit_strs))
  out$has_unitless <- any(is.na(unit_strs))

  # Sample values, pooled across axes and units (units stripped; mixed-unit
  # case flagged via footer)
  vals <- unlist(
    lapply(br, function(b) if (length(b) == 0) NULL else as.numeric(b)),
    use.names = FALSE
  )
  out$values_q <- .quantile(vals)

  new_imu_summary(out)
}

#' @export
print.imu_summary <- function(x, ...) {
  # Header
  if (x$n_na > 0) {
    na_note <- paste0(" (", format_count(x$n_na), " NA)")
  } else {
    na_note <- ""
  }

  cat(format_count(x$n), " ", x$sensor, " bursts", na_note, "\n", sep = "")

  if (is.null(x$axes)) {
    return(invisible(x))
  }

  # Time range
  if (!is.null(x$start_range)) {
    if (nzchar(x$start_tz)) {
      tz_label <- paste0(" ", x$start_tz)
    } else {
      tz_label <- ""
    }

    cat(paste0("from ", format(x$start_range[1]), " to ", format(x$start_range[2]), tz_label), "\n")
  }

  cat("\n")

  # Axes
  axis_parts <- paste0(
    names(x$axes), " (", format_count(as.integer(x$axes)), ")"
  )
  cat("Axes:", paste(axis_parts, collapse = ", "), "\n")

  # Frequency
  cat("Frequencies:", format_range(x$freqs_rng, x$freq_unit), "\n")

  # Samples
  cat("Samples per burst:", format_range(x$samples_rng), "\n")

  # Duration
  cat("Durations:", format_range(x$durations_rng, x$dur_unit), "\n")

  # Intervals
  cat("Intervals:", format_quantiles(x$intervals_q, "s"), "\n")

  # Values + Units
  cat("\n")
  cat("Values: ", format_quantiles(x$values_q), "\n")
  labels <- character(0)
  if (length(x$imu_units) > 0) labels <- paste0("[", x$imu_units, "]")
  if (isTRUE(x$has_unitless)) labels <- c(labels, "NULL")
  if (length(labels) == 0) labels <- "NULL"
  cat("Units:  ", paste(labels, collapse = ", "), "\n")

  invisible(x)
}

new_imu_summary <- function(x) {
  structure(x, class = c("imu_summary", class(x)))
}

format_count <- function(x) {
  format(x, big.mark = ",", trim = TRUE)
}

format_num <- function(x) {
  format(round(x, 2), trim = TRUE, nsmall = 0)
}

# Safe range function for storage in summary object
.range <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NULL)
  }
  range(x)
}

# Safe quantile function for storage in summary object
.quantile <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NULL)
  }
  unname(stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1)))
}

format_range <- function(rng, unit = NULL) {
  if (length(rng) == 0) {
    return("NA")
  }
  mn <- format_num(rng[1])
  mx <- format_num(rng[2])
  if (!is.null(unit)) {
    paste0(mn, " -- ", mx, " [", unit, "]")
  } else {
    paste0(mn, " -- ", mx)
  }
}

format_quantiles <- function(q, unit = NULL) {
  if (length(q) == 0) {
    return("NA")
  }
  parts <- paste(vapply(q, format_num, ""), collapse = " / ")
  out <- paste0("[ ", parts, " ]")
  if (!is.null(unit)) {
    out <- paste0(out, " [", unit, "]")
  }
  paste0(out, "  (min/Q1/med/Q3/max)")
}
