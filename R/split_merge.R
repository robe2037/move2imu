#' Merge adjacent bursts in an IMU vector
#'
#' @description
#' For a given IMU vector, identify temporally adjacent bursts and
#' merge them into a single burst. Bursts whose end time coincides with the
#' start time of the next burst (within `gap_tol`) and whose frequencies
#' agree (within the relative `freq_tol`) are considered adjacent. Bursts
#' with different frequencies, axes, or burst data units will not be merged.
#'
#' To merge bursts with differing units, convert them to a common
#' unit first with [set_imu_units()].
#'
#' @details
#' A burst's end is taken as one sample period after its last sample.
#' A burst of `n` samples at frequency `f` therefore ends `n / f` seconds after
#' its start. The next burst is adjacent when it starts within `gap_tol` of
#' that point.
#'
#' After merging, the burst's frequency is recomputed from its new
#' sample count and overall time span. The gap between the bursts therefore
#' impacts the derived output frequency of the new burst. If the `gap_tol` is
#' set to allow any timestamp noise, the gap between the two bursts may not
#' precisely correspond with the sampling periods of the bursts being merged.
#' In these cases, the recorded output frequency of the merged burst will
#' vary slightly from the values of its component bursts.
#'
#' This approach preserves overall burst time span at the expense of preserving
#' a consistent burst frequency. If you instead prefer to preserve frequency,
#' you will need to manually adjust the frequency of the
#' output burst (see [freqs()]) or correct timestamps in the input data.
#'
#' Bursts with missing frequencies (e.g. a burst with only one sample)
#' are not merged. To merge such bursts, you must assign them a sampling
#' frequency (see [freqs()]).
#'
#' Note that because the burst duration incorporates the elapsed time of the
#' period after the last recorded sample, timestamp noise can make a
#' subsequent burst appear to start slightly "before" the previous burst
#' ends (a small negative time gap). This jitter will also be incorporated into
#' `gap_tol`.
#'
#' @inheritParams n_axis
#' @param ids Vector indicating groups to which the elements in `x` belong.
#'   If provided, bursts in `x` will not be merged across different values of
#'   this vector, even if their timestamps and frequencies align.
#' @param gap_tol Absolute tolerance (in seconds) to use when determining
#'   whether two bursts are adjacent in time and can be merged. Two bursts are
#'   adjacent when the gap between the first burst's end and the second burst's
#'   start is within `gap_tol`.
#'
#'   For example, setting `gap_tol = 0.02` would allow a burst that starts up
#'   to 0.02 seconds after the end of the previous burst to be merged. See
#'   details.
#' @param freq_tol Bare numeric value representing the relative tolerance to use
#'   when determining whether two bursts share a sampling frequency. Bursts can
#'   only be merged if their frequencies are consistent, within `freq_tol`.
#'   Bursts can be merged when the faster frequency is at most `(1 + freq_tol)`
#'   times the slower. For example, `freq_tol = 0.01` merges bursts whose
#'   frequencies are within 1% of each other.
#' @param drop Logical indicating whether to drop entries that have been merged
#'   into other bursts. If `drop = FALSE` (default), the output will have the
#'   same length as the input `x`, with `NA` values at positions where bursts
#'   were merged into a preceding burst. This is useful for retaining index
#'   matching between the input and output vectors.
#'
#' @returns A vector of the same class as `x`.
#' @export
#'
#' @examples
#' a <- acc(
#'   list(cbind(X = 1:60, Y = 1:60), cbind(X = 61:100, Y = 61:100), cbind(X = 101:140)),
#'   frequency = units::set_units(20, "Hz"),
#'   start = as.POSIXct(c(0, 3, 5), origin = "1970-01-01", tz = "UTC")
#' )
#'
#' merge_imu(a)
merge_imu <- function(x,
                      ids = NULL,
                      gap_tol = 1e-6,
                      freq_tol = 1e-2,
                      drop = FALSE) {
  n <- vec_size(x)

  if (!is.null(ids) && length(ids) != n) {
    cli::cli_abort("{.arg ids} must be the same length as {.arg x}.")
  }

  # `gap_tol` is an absolute time; normalize to seconds once here. Every time
  # quantity below is then a plain count of seconds, so the comparisons need no
  # further unit handling.
  gap_tol <- as.numeric(units::set_units(gap_tol, "s"))

  if (gap_tol < 0) {
    cli::cli_abort("{.arg gap_tol} must be greater than or equal to 0.")
  }

  check_freq_tol(freq_tol)

  # Work only with non-NA entries; track their original positions
  valid <- which(!is.na(x))

  if (length(valid) <= 1) {
    if (drop) {
      return(x[valid])
    }
    return(x)
  }

  burst_starts <- starts(x)

  xv <- x[valid]
  sv <- burst_starts[valid]
  nv <- length(valid)

  # Frequencies are stored in Hz, so `n_samples / fq` is a burst's duration and
  # `1 / fq` its sample period, both already in seconds. Deriving the times
  # below from these numerics avoids converting the same quantities through
  # `units` (and through `difftime`, whose unit is chosen by magnitude) once per
  # burst-sized vector.
  fq <- as.numeric(freqs(xv))
  period_s <- 1 / fq
  ns <- n_samples(xv)
  sv_s <- as.numeric(sv)

  # Collapsible bursts must end at the start time of the subsequent burst,
  # within the absolute `gap_tol`.
  end_s <- sv_s + ns / fq
  is_adjacent_burst <- abs(sv_s[-1] - end_s[-nv]) <= gap_tol

  # If no adjacent bursts, no need to proceed
  if (!any(is_adjacent_burst, na.rm = TRUE)) {
    if (drop) {
      return(xv)
    }
    return(x)
  }

  # Collapsible bursts must share a sampling frequency, within `(1 + freq_tol)`:
  # the faster is at most that many times the slower.
  prev_freq <- fq[-nv]
  next_freq <- fq[-1]
  ratio_dev <- pmax(prev_freq, next_freq) / pmin(prev_freq, next_freq) - 1

  # `fp_time_floor` backstops the relative test against sub-microsecond timestamp
  # jitter. That noise is a time-domain quantity, so the backstop is evaluated on
  # the implied sample period (1/freq) in seconds, not on the frequency itself.
  period_dev <- abs(period_s[-1] - period_s[-nv])
  is_same_freq <- !((ratio_dev > freq_tol) & (period_dev > fp_time_floor))

  bv <- bursts(xv)

  # Collapsible bursts must have axis structure
  # Check both axis names and length to disambiguate possible name duplication
  # after collapsing to single string
  axes <- purrr::map_chr(bv, function(b) paste0(colnames(b), collapse = "_"))

  n_ax <- n_axis(xv)
  is_same_n_axis <- (axes[-1] == axes[-nv]) & (n_ax[-1] == n_ax[-nv])

  # Collapsible bursts must have identical units (or both be unitless)
  burst_units <- purrr::map_chr(
    bv,
    function(b) if (inherits(b, "units")) units::deparse_unit(b) else NA_character_
  )
  is_same_units <- (burst_units[-1] == burst_units[-nv]) |
    (is.na(burst_units[-1]) & is.na(burst_units[-nv]))
  is_same_units[is.na(is_same_units)] <- FALSE

  if (rlang::is_null(ids)) {
    is_same_id <- vctrs::vec_recycle(TRUE, nv - 1)
  } else {
    # Don't collapse bursts across different sources, if IDs provided
    ids_v <- ids[valid]
    is_same_id <- (ids_v[-1] == ids_v[-nv]) | (is.na(ids_v[-1]) & is.na(ids_v[-nv]))
  }

  to_bind <- c(FALSE, is_adjacent_burst & is_same_freq & is_same_n_axis & is_same_units & is_same_id)
  to_bind[is.na(to_bind)] <- FALSE

  # Split entries in the vector into groups that should be collapsed and
  # rbind burst matrices
  grp_idx <- unname(split(seq_along(to_bind), cumsum(!to_bind)))

  bursts_comb <- lapply(grp_idx, function(i) do.call(rbind, bv[i]))

  # Get first entry in each group. This defines the burst start time.
  merged_i <- purrr::map_int(grp_idx, function(x) x[1])

  # Recompute each merged burst's frequency from its new span so the merged
  # frequency reflects variations within the tolerance, rather than inheriting
  # the first burst's frequency arbitrarily.
  #
  # A burst's last sample falls one sample period before its end. Deriving this
  # for the whole vector up front keeps the per-group work below to arithmetic
  # on plain seconds.
  last_samp_s <- end_s - period_s

  merged_freq <- purrr::map_dbl(
    grp_idx,
    function(g) {
      if (length(g) == 1L) {
        return(fq[g])
      }

      merged_samps <- sum(ns[g])

      if (is.na(merged_samps) || merged_samps <= 1) {
        return(NA_real_)
      }

      merged_span <- last_samp_s[g[length(g)]] - sv_s[g[1]]

      if (is.na(merged_span) || merged_span <= 0) {
        return(NA_real_)
      }

      snap_freq((merged_samps - 1) / merged_span)
    }
  )

  merged <- imu(
    sensor = class(x)[1],
    bursts = bursts_comb,
    frequency = units::set_units(merged_freq, "Hz"),
    start = sv[merged_i]
  )

  # If retaining index matching, fill merged idx with NA entries.
  #
  # The placeholder's time zone has to match the input's: `vec_assign()` casts
  # the value to the type of the vector assigned into, so a placeholder built
  # with the default (UTC) start would silently re-tag the merged start times.
  if (!drop) {
    out <- vec_rep(
      imu(
        sensor = class(x)[1],
        bursts = list(NULL),
        frequency = units::set_units(NA, "Hz"),
        start = as.POSIXct(NA, tz = attr(burst_starts, "tzone") %||% "")
      ),
      n
    )
    out[valid[merged_i]] <- merged
    merged <- out
  }

  merged
}

#' Split an IMU vector at regular intervals
#'
#' Split the bursts in an IMU vector into bursts of a given time
#' duration. The result is a list of vectors of the same length as the input,
#' with the same class as `x`.
#'
#' @details
#' Bursts with `NA` frequency will not be split, as a burst duration can't
#' be derived. In these cases, the burst is returned unchanged.
#'
#' @inheritParams merge_imu
#' @param interval Numeric or [units][units::units] object defining the time
#'   intervals at which `x` will be split. If no units are provided, the
#'   interval is assumed to be in seconds.
#'
#' @returns A list of vectors (same class as `x`), the same length as `x`.
#'   Each element contains the split pieces of the corresponding input burst.
#' @export
#'
#' @examples
#' a <- acc(
#'   list(cbind(X = 1:60, Y = 1:60), cbind(X = 101:140)),
#'   frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
#'   start = as.POSIXct(c(0, 10), origin = "1970-01-01", tz = "UTC")
#' )
#'
#' x <- split_imu(a, units::set_units(1, "s"))
#' x
#'
#' # Flatten to a single vector
#' flat <- purrr::reduce(x, c)
#' flat
#'
#' # Start times are updated to match the start of each split component
#' starts(flat)
#'
#' # As the bursts immediately follow one another, they can be remerged:
#' m <- merge_imu(flat, drop = TRUE)
#' m
#'
#' identical(m, a)
#'
#' \dontrun{
#' # In a dataframe, split and unnest to retain index matching
#' library(dplyr)
#' library(tidyr)
#'
#' tbl <- tibble::tibble(id = c("a", "b"), burst = a)
#'
#' tbl <- tbl |>
#'   mutate(burst = split_imu(burst, units::set_units(1, "s"))) |>
#'   unnest(burst) |>
#'   mutate(timestamp = starts(burst))
#'
#' tbl
#'
#' # Use merge_imu() to recover original bursts
#' tbl |>
#'   mutate(burst = merge_imu(burst, ids = id, drop = FALSE))
#' }
split_imu <- function(x, interval) {
  if (!(as.numeric(interval) > 0)) {
    cli::cli_abort("{.arg interval} must be a positive number.")
  }

  sensor <- class(x)[1]

  x <- purrr::pmap(
    list(bursts(x), freqs(x), starts(x)),
    function(.br, .fq, .st) {
      if (rlang::is_empty(.br) || nrow(.br) < 1) {
        return(imu(sensor, list(NULL), .fq, .st))
      }

      # Return input burst if missing frequency
      if (is.na(.fq)) {
        return(imu(sensor, list(.br), .fq, .st))
      }

      # coerce user interval into units of (1 / frequency) which is what
      # is implied when we split burst records by index
      freq_units <- units::as_units(units(.fq), mode = "standard")
      period_units <- 1 / freq_units

      interval <- units::set_units(
        interval,
        units(period_units),
        mode = "standard"
      )

      # number of rows per chunk
      i <- units::drop_units((interval / period_units) * .fq)

      idx <- unname(split(seq_len(nrow(.br)), ceiling(seq_len(nrow(.br)) / i)))
      b_split <- lapply(idx, function(j) .br[j, , drop = FALSE])

      # Derive start times from sample index and frequency to keep starts
      # aligned with data when chunk sizes vary
      first_idx <- purrr::map_int(idx, 1L)
      offset_s <- (first_idx - 1) / as.numeric(.fq)

      imu(
        sensor = sensor,
        bursts = b_split,
        frequency = .fq,
        start = .st + offset_s
      )
    }
  )

  x
}
