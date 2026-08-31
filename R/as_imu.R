as_imu <- function(x, sensor, ...) {
  UseMethod("as_imu")
}

#' @export
as_imu.default <- function(x, sensor, ...) {
  vctrs::vec_cast(x, new_imu(sensor))
}

#' @export
as_imu.move2 <- function(x,
                         sensor,
                         colset = NULL,
                         min_freq = 0,
                         freq_tol = 1e-2,
                         gap_tol = 1e-6,
                         merge_continuous = TRUE,
                         drop = FALSE,
                         ...) {
  check_move2_dots(...)

  as_imu_table(
    x,
    sensor = sensor,
    colset = colset,
    min_freq = min_freq,
    freq_tol = freq_tol,
    gap_tol = gap_tol,
    merge_continuous = merge_continuous,
    drop = drop,
    timestamp = move2::mt_time(x),
    track_id = move2::mt_track_id(x),
    ...
  )
}

#' @export
as_imu.data.frame <- function(x,
                              sensor,
                              timestamp,
                              track_id,
                              colset = NULL,
                              min_freq = 0,
                              freq_tol = 1e-2,
                              gap_tol = 1e-6,
                              merge_continuous = TRUE,
                              drop = FALSE,
                              ...) {
  # Check lengths and NA track IDs for consistency with move2 inputs
  if (length(timestamp) != nrow(x)) {
    cli::cli_abort(
      "{.arg timestamp} must be the same length as {.arg x} ({nrow(x)}), not {length(timestamp)}."
    )
  }

  if (!is.null(track_id)) {
    if (length(track_id) != nrow(x)) {
      cli::cli_abort(
        "{.arg track_id} must be the same length as {.arg x} ({nrow(x)}), not {length(track_id)}."
      )
    }

    if (anyNA(track_id)) {
      cli::cli_abort("{.arg track_id} must not contain missing values.")
    }
  }

  as_imu_table(
    x,
    sensor = sensor,
    timestamp = timestamp,
    track_id = track_id,
    colset = colset,
    min_freq = min_freq,
    freq_tol = freq_tol,
    gap_tol = gap_tol,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}

# Implementation behind both `as_imu()` methods. Only difference is that .move2
# methods populate `timestamp` and `track_id` from the move2 metadata
as_imu_table <- function(x,
                         sensor,
                         timestamp,
                         track_id,
                         colset = NULL,
                         min_freq = 0,
                         freq_tol = 1e-2,
                         gap_tol = 1e-6,
                         merge_continuous = TRUE,
                         drop = FALSE) {
  timestamp <- timestamp_to_POSIXct(timestamp, arg = "timestamp")

  # Check input arg validity, including units matching and positive values
  if (as.numeric(as_hz(min_freq)) < 0) {
    cli::cli_abort("{.arg min_freq} must be greater than or equal to 0.")
  }

  check_freq_tol(freq_tol)

  if (as.numeric(units::set_units(gap_tol, "s")) < 0) {
    cli::cli_abort("{.arg gap_tol} must be greater than or equal to 0.")
  }

  if (nrow(x) == 0) {
    return(new_imu(sensor))
  }

  # If NULL, treat track_id as a single track
  track_id <- as.character(track_id %||% rep(1L, nrow(x)))

  colsets <- parse_colsets(x, colset, sensor)
  dup <- duplicated_imu_rows(x, colsets = colsets)

  if (any(dup)) {
    dup_fn <- paste0("duplicated_", sensor, "_rows")
    cli::cli_abort(c(
      "{.arg x} contains {sum(dup)} row{?s} with multiple sources of {sensor} data.",
      "i" = "Use {.help [{.fun {dup_fn}}](move2imu::{dup_fn})} to identify duplications."
    ))
  }

  # Use lapply as we don't need purrr's index errors here. User likely
  # will not realize we are iterating over colsets.
  out <- lapply(
    colsets,
    function(cols) {
      as_imu_(
        x,
        sensor = sensor,
        colset = cols,
        timestamp = timestamp,
        track_id = track_id,
        min_freq = min_freq,
        freq_tol = freq_tol,
        gap_tol = gap_tol,
        merge_continuous = merge_continuous,
        drop = FALSE
      )
    }
  )

  out <- purrr::reduce(out, coalesce_imu)

  if (drop) {
    out <- out[!is.na(out)]
  }

  out
}

as_imu_ <- function(x,
                    sensor,
                    colset,
                    timestamp,
                    track_id,
                    min_freq = 0,
                    freq_tol = 1e-2,
                    gap_tol = 1e-6,
                    merge_continuous = TRUE,
                    drop = FALSE) {
  check_colset(x, colset)

  imu_rows <- imu_sample_rows(x, sensor = sensor, colset = colset)

  if (any(imu_rows) && any(is.na(timestamp[imu_rows]))) {
    cli::cli_abort("All timestamps associated with IMU data must be non-NA.")
  }

  if (any(imu_rows)) {
    # Ensure timestamps in imu rows are ordered within tracks. We don't use
    # move2's native implementation as it only dispatches on `move2`
    check_time_order(
      timestamp[imu_rows],
      track_id[imu_rows],
      hint_move2 = inherits(x, "move2")
    )
  }

  type <- colset_type(colset)

  if (type == "expanded") {
    out <- as_imu_expanded(
      x,
      colset = colset,
      sensor = sensor,
      timestamp = timestamp,
      track_id = track_id,
      min_freq = min_freq,
      freq_tol = freq_tol
    )
  } else if (type == "compact") {
    out <- as_imu_compact(
      x[[colset[["bursts"]]]],
      x[[colset[["axes"]]]],
      x[[colset[["frequency"]]]],
      sensor = sensor,
      timestamp = timestamp
    )
  } else {
    abort_missing_colset(sensor)
  }

  if (merge_continuous) {
    out <- merge_imu(
      out,
      ids = track_id,
      gap_tol = gap_tol,
      freq_tol = freq_tol,
      drop = drop
    )
  }

  if (drop) {
    out <- out[!is.na(out)]
  }

  out
}

as_imu_compact <- function(x, axes, freq, sensor, timestamp) {
  colnms <- strsplit(as.character(axes), "")
  n_axis <- nchar(as.character(axes))
  vals_split <- strsplit(as.character(x), " ", fixed = TRUE)

  mlist <- purrr::map(vals_split, function(x) as.numeric(x))

  i <- !is.na(n_axis)

  mlist[!i] <- list(NULL)

  mlist[i] <- mapply(
    matrix,
    mlist[i],
    ncol = n_axis[i],
    MoreArgs = list(byrow = TRUE),
    SIMPLIFY = FALSE
  )

  mlist[i] <- mapply("colnames<-", mlist[i], colnms[i], SIMPLIFY = FALSE)

  imu(sensor = sensor, bursts = mlist, frequency = freq, start = timestamp)
}

as_imu_expanded <- function(x,
                            colset,
                            sensor,
                            timestamp,
                            track_id,
                            min_freq = 0,
                            freq_tol = 1e-2) {
  col_names <- as.character(colset)
  m <- as.matrix(as.data.frame(x)[, col_names])

  colnames(m) <- names(colset)

  # TODO: may want a safer way to handle units. Some columns will have units, others not
  if (inherits(x[[colset[[1]]]], "units")) {
    m <- m * units::as_units(units::deparse_unit(x[[colset[[1]]]]))
  }

  # Group samples into bursts. `parse_bursts()` returns, per burst, both its row
  # indices (`bursts`) and its derived sampling frequency (`freq`), as parallel
  # lists so the two stay aligned without a key.
  parsed <- parse_bursts(
    x,
    colset = colset,
    timestamp = timestamp,
    track_id = track_id,
    min_freq = min_freq,
    freq_tol = freq_tol
  )
  burst_idx <- parsed$bursts

  # Extract records for each burst into a separate matrix
  burst_lst <- lapply(burst_idx, function(i) {
    x <- m[i, , drop = FALSE]
    rownames(x) <- NULL # Standardize data.frame and tibble inputs
    x
  })

  # Attach bursts to index of the first record that belongs to that burst
  out <- vec_rep(
    imu(
      sensor,
      bursts = list(NULL),
      frequency = units::set_units(NA, "Hz"),
      start = as.POSIXct(NA, tz = attr(timestamp, "tzone") %||% "")
    ),
    nrow(x)
  )

  i <- purrr::map_int(burst_idx, 1L) # first index of each burst
  freq <- snap_freq(parsed$freq)

  if (length(i) > 0) {
    out[i] <- imu(sensor, bursts = burst_lst, frequency = units::as_units(freq, "Hz"), start = timestamp[i])
  }

  out
}

# Resolve user-supplied `colset` into a list of validated IMU colsets.
# Falls back to colsets detected in `x` when `colset` is NULL.
parse_colsets <- function(x, colset, sensor, quiet = FALSE) {
  if (!rlang::is_null(colset)) {
    if (is_imu_colset(colset)) {
      colsets <- colset
    } else if (rlang::is_list(colset) && all(purrr::map_lgl(colset, is_imu_colset))) {
      colsets <- colset
    } else {
      cli::cli_abort(c(
        "{.arg colset} must be an {.cls imu_colset} object or a list of {.cls imu_colset} objects.",
        "i" = "Use {.help [{.fun imu_colset}](move2imu::imu_colset)} to create an {.cls imu_colset} object."
      ))
    }
  } else {
    colsets <- active_colsets_(x, sensor = sensor)

    if (!quiet && length(colsets) > 1) {
      cli::cli_warn("Detected multiple valid {sensor} column sets.")
    }
  }

  # Standardize case where user supplied a single colset as a vector
  if (!rlang::is_list(colsets)) {
    colsets <- list(colsets)
  }

  colsets
}

which_imu_vals <- function(x, colset) {
  assert_all_cols_present(x, colset)

  x <- as.data.frame(x) # Drop sticky move2 columns

  type <- colset_type(colset)

  # Expanded-format columns only need at least one column to have data
  if (type == "expanded") {
    has_vals <- which(rowSums(!is.na(x[colset])) > 0)
  } else {
    has_vals <- which(rowSums(!is.na(x[colset])) == length(colset))
  }

  has_vals
}

#' Group expanded-format IMU samples into bursts
#'
#' @description
#' Based on the timestamps of the samples in expanded-format IMU
#' data, identify bursts based on the observed sampling frequency. Samples are
#' first grouped into runs of consistent sampling frequency; any run whose
#' overall frequency falls below `min_freq` is then split into individual
#' (length-1) bursts.
#'
#' @details
#' For continuous data, IMUs may dynamically update collection frequency.
#' However, a burst should not contain data from multiple collection
#' frequencies, so we must split these data into distinct bursts, despite the
#' fact that there may be no gap in collection.
#'
#' `min_freq` is evaluated on each run's derived frequency, not on the
#' individual inter-sample gaps. A single anomalous gap
#' therefore does not explode a run whose overall frequency is still consistent
#' with the input `min_freq`.
#'
#' For samples at the boundary of a frequency change, there is
#' a fundamental ambiguity as to whether these samples should be included in
#' the burst prior to or the burst after the boundary timestamp. See comments
#' to `freq_changes` for details on our approach.
#'
#' @inheritParams as_acc
#' @param x data.frame with expanded-format IMU data
#' @param timestamp Timestamp of each row of `x`, as a `POSIXct` vector.
#' @param track_id Track identifier of each row of `x`, as a `character` vector.
#'
#' @returns A list with elements `bursts` and `freq`. The former is a list
#'   whose elements indicate the row indices of `x` belonging to that burst.
#'   The latter is the derived sampling frequency of each burst. Elements of
#'   each match by index.
#' @noRd
parse_bursts <- function(x,
                         colset,
                         timestamp,
                         track_id,
                         min_freq = 0,
                         freq_tol = 1e-2) {
  # Coerce to Hz at the boundary so `1 / min_freq` below is in seconds,
  # matching the second-based sample intervals, regardless of the unit supplied.
  min_freq <- as_hz(min_freq)

  min_interval <- (1 / as.numeric(min_freq)) + fp_time_floor

  vals_i <- which_imu_vals(x, colset = colset)

  idx <- split(vals_i, track_id[vals_i])

  grps <- lapply(
    idx,
    function(i) {
      i <- unname(i)

      if (length(i) < 2) {
        return(list(bursts = list(i), freq = NA_real_))
      }

      samp_times <- as.numeric(timestamp[i])

      # Identify runs of consistent sampling frequency, within the tolerance
      is_freq_change <- freq_changes(diff(samp_times), freq_tol = freq_tol)
      run_id <- cumsum(is_freq_change)

      # Explode any run collected slower than `min_freq` by calculating
      # sample interval times and comparing to the `min_freq`-implied interval
      run_intervals <- unname(
        purrr::map_dbl(
          split(samp_times, run_id),
          function(z) {
            if (length(z) < 2) NA_real_ else (max(z) - min(z)) / (length(z) - 1)
          }
        )
      )

      too_slow <- !is.na(run_intervals) & (run_intervals > min_interval)
      run_start <- is_freq_change | too_slow[run_id]

      # Update `run_id` to split runs that were exploded by `min_freq` threshold
      run_id <- run_id[run_start]

      # Calculate frequencies for each run, assigning NA to length-1 runs
      freq <- 1 / run_intervals[run_id]
      freq[too_slow[run_id] | !is.finite(freq)] <- NA_real_

      # Return as list of burst locations and frequencies
      list(
        bursts = unname(split(i, cumsum(run_start))),
        freq = freq
      )
    }
  )

  list(
    bursts = unlist(purrr::map(grps, "bursts"), use.names = FALSE, recursive = FALSE),
    freq = unlist(purrr::map(grps, "freq"), use.names = FALSE)
  )
}

# Identify transition points from one frequency to another within a sequential
# time difference vector.
#
# Sequential IMU data may change frequency. This can occur either from
# legitimate burst gaps or from changes in collection frequency. In general,
# when a change of frequency is detected, we create a new group of IMU
# values. See `new_freq_regime()` for more on the logic of how split points
# are determined in ambiguous cases.
freq_changes <- function(x, freq_tol = 1e-2) {
  d <- as.numeric(x)

  # Absolute difference between each pair of consecutive sample intervals, in
  # seconds. Each interval is a local, one-sample estimate of the sampling
  # period, so this is the change in the implied frequency from one sample to
  # the next.
  interval_dev <- abs(diff(d))

  # `freq_tol` is the largest fractional gap allowed between two frequency
  # estimates: two intervals belong to the same regime when the faster
  # frequency is at most `(1 + freq_tol)` times the slower. Because the ratio of
  # the larger to the smaller is invariant under reciprocal, this is identical
  # whether taken on frequencies or on periods, so we take it directly on the
  # periods `d` we already have. `fp_time_floor` backstops the relative test
  # against sub-microsecond timestamp noise on short intervals; that noise is a
  # time-domain quantity, so the backstop stays on the period difference.
  prev_period <- d[-length(d)]
  next_period <- d[-1]
  ratio_dev <- pmax(prev_period, next_period) / pmin(prev_period, next_period) - 1
  is_change <- (ratio_dev > freq_tol) & (interval_dev > fp_time_floor)

  # Get runs of values within a given tolerance
  freq_within_tol <- cumsum(c(TRUE, is_change))
  r <- rle(freq_within_tol)

  # Adjust first run length to account for loss of initial value from `diff()`
  r$lengths[1] <- r$lengths[1] + 1

  runs <- list()
  runs[1] <- list(new_freq_regime(r$lengths[1]))

  # Length of subsequent run. Used when deciding which run to attach
  # ambiguous split points to
  n_next <- c(r$lengths[-1], 0)

  # Generate logical vector with TRUE values marking transition states to
  # new frequency regimes
  for (i in seq_len(length(r$lengths))[-1]) {
    runs[i] <- list(
      new_freq_regime(
        r$lengths[i],
        n_next = n_next[i],
        prev_run = runs[[i - 1]]
      )
    )
  }

  unlist(runs)
}

# Helper to build logical runs identifying sequences of frequency regimes
#
# In a sequence of time diffs, we identify the start of a new frequency
# regime where there is a change in frequency from one index to the next.
# The following time gap is established as the frequency of the next
# regime. This function generates a logical vector for each run of
# consistent frequency values. TRUE values mark start indexes of new
# frequency regimes. FALSE values mark indexes that will be grouped with the
# closest TRUE value that precedes them.
#
# Where multiple frequency changes happen in succession, there is ambiguity as
# to how values should be grouped, as no frequency regime can definitively be
# established for a series of length-1 sequences. That is, each of these single
# values could just as reasonably be grouped with the value prior to them or
# after them. In these cases, we group
# the record immediately following the initial frequency change (t + 1) with that
# initial frequency change (t), unless the subsequent run starting with record
# (t + 2) is longer than 1. In these cases, we consider
# the (t + 1) record to belong to the (t + 2) sequence and the (t) record becomes
# an isolated length-1 sequence.
new_freq_regime <- function(n, n_next = 0, prev_run = FALSE) {
  # If the previous run ends in FALSE, this run should start a new regime
  start <- !prev_run[length(prev_run)]

  # Force this record to join with next run if it is length-1 and that run is
  # longer than length-1. (This addresses cases where a length-1 value could
  # either be joined to its previous run or its subsequent run)
  if (n == 1 && n_next > 1) {
    start <- TRUE
  }

  c(start, rep(FALSE, n - 1))
}

# Implementation of `move2::mt_is_track_id_cleaved()` that dispatches on an
# ID vector. Differences from move2:
#   - Distinct IDs checked with `unique()`, not `nlevels()`. Avoids factor
#     with unused levels failing
#   - length-0 inputs are TRUE instead of FALSE
ids_cleaved <- function(id) {
  if (length(id) < 2L) {
    return(TRUE)
  }

  i <- as.integer(factor(id))

  sum(diff(i) != 0L) + 1L == length(unique(i))
}

# Implementation of `move2::mt_is_time_ordered(..., non_zero = TRUE)`
# that dispatches on timestamp and track ID vectors, not `move2` object.
times_ordered <- function(timestamp, id) {
  if (length(timestamp) < 2L) {
    return(TRUE)
  }

  new_track <- diff(as.integer(factor(id))) != 0L

  isTRUE(all(as.numeric(diff(timestamp)) > 0 | new_track))
}

# Error if tracks are not cleaved by ID and in increasing time order.
check_time_order <- function(timestamp,
                             track_id,
                             hint_move2 = FALSE,
                             call = rlang::caller_env()) {
  if (!ids_cleaved(track_id)) {
    cli::cli_abort(
      c(
        "Not all tracks are grouped in {.arg x}.",
        "i" = "Sort {.arg x} by track before building bursts."
      ),
      call = call
    )
  }

  if (!times_ordered(timestamp, track_id)) {
    cli::cli_abort(
      c(
        "Timestamps must be strictly increasing within each track.",
        "i" = if (hint_move2) {
          "Order data by track and time and remove duplicate timestamps. See `move2::mt_filter_unique()`."
        } else {
          "Order data by track and time and remove duplicate timestamps."
        }
      ),
      call = call
    )
  }

  invisible(NULL)
}

# `move2` methods take `timestamp` and `track_id` from the object's metadata,
# so a user-supplied value would collide with them in `as_imu_table()`. Catch
# that here rather than letting R report a duplicate formal match.
check_move2_dots <- function(..., call = rlang::caller_env()) {
  supplied <- intersect(c("timestamp", "track_id"), ...names())

  if (length(supplied) > 0) {
    cli::cli_abort(
      c(
        "{.arg {supplied}} must not be supplied when {.arg x} is a {.cls move2}.",
        "i" = "Timestamps and track IDs are taken from the {.cls move2} object's metadata.",
        "i" = "Use {.fn move2::mt_set_time_column} or {.fn move2::mt_set_track_id_column} to change them."
      ),
      call = call
    )
  }

  invisible(NULL)
}

# Combine two IMU vectors of the same sensor, filling missing bursts in `x`
# with index-matched bursts in `y`. Reimplementation of `dplyr::coalesce()`
# so we can avoid requiring dplyr for as_*.data.frame() calls.
coalesce_imu <- function(x, y) {
  i <- vec_detect_missing(x)
  vec_assign(x, i, vec_slice(y, i))
}

# Colset validation ------------------------------------------------------------

check_colset <- function(x, colset, call = rlang::caller_env()) {
  assert_all_cols_present(x, colset, call = call)
  assert_colset_has_data(x, colset, call = call)

  if (colset_type(colset) == "compact") {
    assert_compact_col_types(x, colset, call = call)
  } else {
    assert_matched_units(x, colset, call = call)
    assert_colset_numeric(x, colset, call = call)
  }
}

assert_colset_has_data <- function(x, colset, call = rlang::caller_env()) {
  if (all(cols_empty(x, colset))) {
    cli::cli_abort(
      c(
        "The provided {.arg colset} columns contain no data.",
        "x" = "Column{?s} {.val {colset}} {?is/are} empty."
      ),
      call = call
    )
  }
}

assert_matched_units <- function(x, cols, call = rlang::caller_env()) {
  unique_units <- unique(
    purrr::map(
      cols,
      function(col) {
        if (inherits(x[[col]], "units")) {
          units(x[[col]])
        } else {
          NA
        }
      }
    )
  )

  if (length(unique_units) != 1) {
    cli::cli_abort(
      c(
        "Multiple units detected across input columns.",
        "i" = "All columns must have consistent units."
      ),
      call = call
    )
  }
}

assert_colset_numeric <- function(x, colset, call = rlang::caller_env()) {
  cols_num <- purrr::map_lgl(colset, function(col) is.numeric(x[[col]]))

  if (any(!cols_num)) {
    non_numeric <- colset[!cols_num]
    cli::cli_abort(
      c(
        "Detected non-numeric column{?s}: {.val {non_numeric}}.",
        "i" = "Columns must contain numeric data."
      ),
      call = call
    )
  }
}

assert_compact_col_types <- function(x, colset, call = rlang::caller_env()) {
  bursts_col <- colset[["bursts"]]
  axes_col <- colset[["axes"]]
  freq_col <- colset[["frequency"]]

  if (!is.character(x[[bursts_col]]) && !is.factor(x[[bursts_col]])) {
    cli::cli_abort(
      "{.arg bursts} column {.val {bursts_col}} must be character, not {.cls {class(x[[bursts_col]])[1]}}.",
      call = call
    )
  }

  if (!is.character(x[[axes_col]]) && !is.factor(x[[axes_col]])) {
    cli::cli_abort(
      "{.arg axes} column {.val {axes_col}} must be character, not {.cls {class(x[[axes_col]])[1]}}.",
      call = call
    )
  }

  if (!is.numeric(x[[freq_col]])) {
    cli::cli_abort(
      "{.arg frequency} column {.val {freq_col}} must be numeric, not {.cls {class(x[[freq_col]])[1]}}.",
      call = call
    )
  }
}
