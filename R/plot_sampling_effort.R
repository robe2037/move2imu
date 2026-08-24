#' Visualize sample collection times from `imu` or timestamp vectors
#'
#' @description
#' Create a plot showing the sampling effort over time for a set of `imu`
#' or timestamp vectors. Use this to identify changes in sampling regimes over
#' the course of data collection.
#'
#' Sample collection times are grouped into bins spanning a given time range.
#' Each bin is shaded relative to the number of samples that falls within that
#' bin for each input vector. Bins are grouped by input vector and optionally
#' by a user-provided grouping factor (often identifying individual tracks).
#'
#' @details
#' Sampling effort is calculated by
#'
#' 1. splitting the plotted time range into equal-width bins. The binned grid
#'    starts at `from` if provided. Otherwise, it starts at the closest multiple
#'    of `bin_width` at or before the first sample.
#' 2. counting the number of samples each sensor recorded in each bin, separately
#'    for every track.
#' 3. dividing each count by the bin width, giving an effective sampling rate
#'    in Hz.
#' 4. normalizing those rates within each input vector, so that each sensor's
#'    sampling effort values are relative to the maximum sampling effort
#'    recorded in that vector, across all groups in `ids`.
#'
#' The shade of each bin is mapped to effort with a square-root transform and is
#' limited to a minimum alpha value of 0.28 to ensure sparse bursts remain
#' visible. A bin in which a sensor recorded nothing is left blank, so a gap in
#' a row reflects a period with no samples recorded.
#'
#' Note that because the shade of each bin is normalized within each input
#' vector provided to `...`, shade says nothing about the absolute sampling
#' rate of a vector, and shades cannot be compared across inputs.
#' Because normalization spans all groups in `ids`, panels can be compared
#' with one another, but a track that sampled less intensively than its peers
#' appears uniformly faint.
#'
#' The time axis is drawn in the time zone of the first vector passed to `...`.
#'
#' You can directly access the data produced by this calculation and passed to
#' the plot by calling [bin_samples()].
#'
#' ## Extending the plot
#'
#' The returned plot is a `ggplot2::ggplot` object and can therefore be modified
#' with further ggplot2 layers.
#'
#' Every aesthetic is set on the tile layer rather than on the plot, so added
#' layers do not inherit aesthetics and must supply their own mappings. For
#' more customization, build your own plot based on the data produced by
#' [bin_samples()].
#'
#' Note that the returned plot sets some default theme and scale
#' parameters, which may be overwritten if replacing certain layers or theme
#' elements (e.g., with a built-in ggplot2 theme).
#'
#' The default theme includes these theme parameters:
#'
#' ```
#' panel.grid.major.y = element_blank()
#' panel.grid.minor = element_blank()
#' panel.grid.major.x = element_line(linetype = "dashed", color = "gray80", linewidth = 0.3)
#' panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.4)
#' strip.text.y.left = element_text(angle = 0, hjust = 1)
#' strip.placement = "outside"
#' plot.caption = element_text(color = "gray20")
#' ```
#'
#' @inheritParams bin_samples
#'
#' @return A [`ggplot2::ggplot`] object, built from the data returned by
#'   [bin_samples()].
#'
#' @seealso [bin_samples()] for the underlying counts.
#'
#'   [plot_imu_trace()] to plot the data values recorded by an `imu` vector.
#'
#' @export
#'
#' @examplesIf rlang::is_installed(c("ggplot2", "move2", "sf"))
#' alb <- albatrosses()
#'
#' acc <- as_acc(alb)
#' tracks <- move2::mt_track_id(alb)
#'
#' # Plot `imu` vectors by passing them directly
#' plot_sampling_effort(acc, ids = tracks)
#'
#' # Adjust bin width to adjust "resolution" of the plot
#' plot_sampling_effort(
#'   acc,
#'   bin_width = units::set_units(20, "s"),
#'   ids = tracks
#' )
#'
#' # It is also possible to plot timestamp vectors.
#' plot_sampling_effort(
#'   move2::mt_time(alb),
#'   ids = tracks,
#'   bin_width = units::set_units(30, "s")
#' )
#'
#' # When plotting multiple sources of data, they must be the same length
#' # and aligned with `ids`, if provided.
#' #
#' # For instance, to extract GPS coordinates from a move2, mask out the non-GPS
#' # observations, but leave them as `NA`:
#' gps <- replace(move2::mt_time(alb), sf::st_is_empty(alb), NA)
#'
#' # This ensures GPS coordinates will be correctly grouped by `ids`:
#' plot_sampling_effort(
#'   acc,
#'   gps,
#'   ids = tracks,
#'   bin_width = units::set_units(30, "s")
#' )
#'
#' # Restrict the plot time axis with `from`/`to`
#' p <- plot_sampling_effort(
#'   acc,
#'   gps,
#'   ids = tracks,
#'   from = as.POSIXct("2008-07-27 00:00:00", tz = "UTC"),
#'   to = as.POSIXct("2008-07-27 00:02:00", tz = "UTC")
#' )
#'
#' p
#'
#' # The plot can be modified like another ggplot2 plot
#' # (Note that modifying some layers may overwrite plot defaults and produce a
#' # slightly different layout)
#' library(ggplot2)
#'
#' p +
#'   geom_vline(xintercept = as.POSIXct("2008-07-27 00:00:45", tz = "UTC")) +
#'   labs(title = "My IMU Data", x = "Time") +
#'   scale_x_datetime(date_breaks = "1 min", date_labels = "%H:%M") +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#'
#' # Modify label names by naming the input vectors
#' plot_sampling_effort(acceleration = acc, ids = tracks)
plot_sampling_effort <- function(...,
                                 ids = NULL,
                                 bin_width = NULL,
                                 from = NULL,
                                 to = NULL) {
  rlang::check_installed("ggplot2", "to plot sampling effort")

  bins <- bin_samples_(
    ...,
    ids = ids,
    bin_width = bin_width,
    from = from,
    to = to
  )

  bin_width <- attr(bins, "bin_width")

  from <- timestamp_to_sec(from)
  to <- timestamp_to_sec(to)

  p <- ggplot2::ggplot(bins) +
    ggplot2::geom_tile(
      ggplot2::aes(
        x = .data$time,
        y = .data$lane,
        fill = .data$lane,
        alpha = .data$effort
      ),
      width = bin_width,
      height = 0.85,
      position = ggplot2::position_nudge(x = bin_width / 2)
    ) +
    # y-axis runs bottom up, lanes run top-down. Reverse y-order here to
    # standardize
    ggplot2::scale_y_discrete(limits = rev, drop = FALSE) +
    ggplot2::scale_alpha_continuous(
      range = c(0.28, 1), # Set floor so no bins fully disappear
      transform = "sqrt", # Stretch the scale to accentuate low end
      guide = "none"
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      caption = paste0("Bin width: ", format_bin(bin_width))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        linetype = "dashed",
        color = "gray80",
        linewidth = 0.3
      ),
      panel.border = ggplot2::element_rect(
        color = "gray80",
        fill = NA,
        linewidth = 0.4
      ),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
      strip.placement = "outside", # Place facet labels outside lane labels
      plot.caption = ggplot2::element_text(color = "gray20")
    ) +
    ggplot2::guides(fill = "none")

  if (!is.null(ids)) {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$id),
        switch = "y",
        drop = FALSE
      )
  }

  # Ensure plot limits are consistent with `from`/`to`, even if they span
  # beyond the range of the data themselves.
  if (!is.null(from) || !is.null(to)) {
    p <- p + ggplot2::expand_limits(
      x = as.POSIXct(
        c(from, to),
        tz = attr(bins$time, "tzone"),
        origin = "1970-01-01"
      )
    )
  }

  p
}

#' Count samples within time bins for a set of sensors
#'
#' @description
#' Summarize sensor sampling regimes for a set of `imu` or timestamp vectors by
#' counting samples within fixed time bins. This produces the data that is
#' used by [plot_sampling_effort()], exposed for custom plots.
#'
#' @details
#' Sampling effort is calculated by
#'
#' 1. splitting the plotted time range into equal-width bins. The binned grid
#'    starts at `from` if provided. Otherwise, it starts at the closest multiple
#'    of `bin_width` at or before the first sample.
#' 2. counting the number of samples each sensor recorded in each bin, separately
#'    for every track.
#' 3. dividing each count by the bin width, giving an effective sampling rate
#'    in Hz.
#' 4. normalizing those rates within each lane, so that each lane's sampling
#'    effort values are relative to the maximum sampling effort in that lane,
#'    across all values of `ids`.
#'
#' @param ... Any number of `imu` or timestamp vectors. All vectors must be the
#'   same length. Timestamps can be in `POSIXct`, `POSIXlt`, `Date`, or a
#'   number of seconds since `1970-01-01 00:00:00 UTC`. `Date` objects are
#'   treated as being recorded at midnight, UTC.
#' @param ids Vector of IDs used to group the observations in `...`. All
#'   observations for each group will be included in a single panel. Must be
#'   the same length as each of the vectors in `...`.
#' @param bin_width Width of the time bins within which samples are counted.
#'   Provided as a [units][units::units] object, a [difftime] object, or a
#'   numeric value which will be interpreted as seconds. By default, the time
#'   range of the plot is divided into roughly 300 bins.
#'
#'   Decrease the `bin_width` to increase plot resolution, at the expense of
#'   legibility for sparsely collected data.
#' @param from,to Start and end timestamps defining the range within which
#'   samples will be counted. Accepts the same formats as timestamps in `...`.
#'   By default, the full temporal extent of the data is used.
#'
#' @return A data frame with one row per non-empty lane, id, and bin, containing
#'   the following columns:
#'
#'   - `lane`: The source vector in `...`, labeled with that
#'     argument's name or expression. Each lane is drawn as one
#'     row by [plot_sampling_effort()].
#'   - `id`: Grouping identifier, as a factor. Absent when `ids` is `NULL`.
#'   - `time`: Bin start, in the timezone of the first lane.
#'   - `n`: Samples recorded in the bin.
#'   - `rate`: Effective sampling rate, `n / bin_width`, in Hz.
#'   - `effort`: `rate` as a fraction of the highest rate in the same lane,
#'     taken across all values of `ids`.
#'
#'   The bin width in seconds is attached as the `"bin_width"` attribute.
#'
#' @seealso [plot_sampling_effort()] to plot the output of `bin_samples()`
#'
#' @keywords internal
#' @export
#'
#' @examplesIf rlang::is_installed(c("move2", "sf"))
#' alb <- albatrosses()
#'
#' bin_samples(acc = as_acc(alb), ids = move2::mt_track_id(alb))
bin_samples <- function(...,
                        ids = NULL,
                        bin_width = NULL,
                        from = NULL,
                        to = NULL) {
  bin_samples_(..., ids = ids, bin_width = bin_width, from = from, to = to)
}

# Count samples for a set of IMU or timestamp vectors within time bins.
# This drives `bin_samples()` and `plot_sampling_effort()`. Kept as a separate
# internal implementation to improve error tracebacks.
bin_samples_ <- function(...,
                         ids = NULL,
                         bin_width = NULL,
                         from = NULL,
                         to = NULL) {
  error_call <- rlang::caller_env()
  rlang::local_error_call(error_call)

  lanes <- rlang::dots_list(..., .named = TRUE, .homonyms = "error")

  if (length(lanes) == 0L) {
    cli::cli_abort("No data provided to {.arg ...}.")
  }

  lane_sizes <- unique(vctrs::list_sizes(lanes))

  if (length(lane_sizes) > 1) {
    cli::cli_abort(c(
      "All elements in `...` must be the same length.",
      x = "Got lengths {lane_sizes}"
    ))
  }

  if (!is.null(ids) && length(ids) != lane_sizes[[1]]) {
    cli::cli_abort(
      "Length of `ids` ({length(ids)}) must match length of each vector in `...` ({lane_sizes[[1]]})"
    )
  }

  bad_type <- !purrr::map_lgl(
    lanes,
    function(l) inherits(l, "imu") || is_timestamp(l)
  )

  if (any(bad_type)) {
    cli::cli_abort(c(
      "Every input to {.arg ...} must be an {.cls imu} or timestamp vector.",
      x = "Problems with {.arg {names(lanes)[bad_type]}}."
    ))
  }

  lane_labels <- names(lanes)

  # Standardize bin width in seconds
  bin_width <- bin_to_sec(bin_width)

  # Check plot time window validity
  from <- timestamp_to_sec(from)
  to <- timestamp_to_sec(to)

  if (!is.null(from) && !is.null(to) && to <= from) {
    cli::cli_abort("{.arg to} must be after {.arg from}.")
  }

  lane_timing <- purrr::map(lanes, burst_timing)

  # Get start and end times of each burst
  burst_bounds <- unlist(
    purrr::map(
      lane_timing,
      function(x) {
        has_start <- !is.na(x$start)
        c(
          x$start[has_start],
          x$start[has_start] +
            (x$n_samp[has_start] - 1) * x$samp_period[has_start]
        )
      }
    )
  )

  if (length(burst_bounds) == 0L) {
    cli::cli_abort("None of the data inputs have any timestamp information recorded.")
  }

  # Get default time window. If user provided `from`/`to`, they are used,
  # otherwise, data extent is used
  win_start <- from %||% min(burst_bounds, to %||% Inf)
  win_end <- to %||% max(burst_bounds, from %||% -Inf)

  # Set default bin width based on plot time window
  span <- win_end - win_start

  if (span > 0) {
    bin_width <- bin_width %||% (span / default_bins)
  } else {
    # Span not greater than 0 only in the single-sample case, in which
    # case the default bin_width is arbitrary, as it spans the entire plot. We
    # use 1s here in an attempt to indicate that the bin is not aggregated over
    # a long time range.
    bin_width <- bin_width %||% 1
  }

  # Anchor the bin grid on the provided `from`. If not provided, snap it to a
  # bin edge based on the bin width
  grid_origin <- from %||% (floor(win_start / bin_width) * bin_width)

  # Standardize time zone to the tz associated with the first lane
  tz <- attr(lane_timing[[1]], "tzone")

  out <- do.call(
    rbind,
    Map(
      function(timing, label) {
        has_start <- !is.na(timing$start)

        if (is.null(ids)) {
          burst_ids <- rep(1L, nrow(timing))
        } else {
          burst_ids <- as.character(ids)
        }

        counts <- count_in_bins(
          start = timing$start[has_start],
          samp_period = timing$samp_period[has_start],
          n_samp = timing$n_samp[has_start],
          ids = burst_ids[has_start],
          bin_width = bin_width,
          grid_origin = grid_origin,
          from = from %||% -Inf,
          to = to %||% Inf,
          call = error_call
        )

        if (nrow(counts) == 0L) {
          return(NULL)
        }

        data.frame(
          lane = label,
          id = counts$id,
          time = as.POSIXct(
            grid_origin + counts$bin_i * bin_width,
            tz = tz,
            origin = "1970-01-01"
          ),
          n = counts$n,
          rate = counts$n / bin_width
        )
      },
      lane_timing,
      names(lane_timing)
    )
  )

  if (is.null(out)) {
    cli::cli_abort("No samples fall in the requested time window.")
  }

  # `rbind()` derives row names from the lane labels, which duplicates the
  # `lane` column and gets unwieldy when a label is a deparsed expression
  rownames(out) <- NULL

  # Lane levels come from the arguments, not from the data, so a lane with no
  # data still renders as a labeled empty row.
  out$lane <- factor(out$lane, levels = lane_labels)

  if (is.null(ids)) {
    out$id <- NULL
  } else {
    # Ensure all tracks with entries in `ids` are represented as a panel,
    # even if they have only NA data. A NA track ID is treated as a level.
    out$id <- factor(
      out$id,
      levels = c(levels(factor(ids)), if (anyNA(out$id)) NA),
      exclude = NULL
    )
  }

  # Normalize sampling effort within each lane. This prevents low-frequency
  # lanes (e.g. GPS) from getting very low alpha if paired with a high-frequency
  # sensor
  out$effort <- stats::ave(out$rate, out$lane, FUN = function(r) r / max(r))

  attr(out, "bin_width") <- bin_width

  out
}

# Count samples per track/bin, using the burst and bin metadata rather than
# expanding samples.
#
# The sample count in a given bin is the number of samples before its end
# minus the number of samples before its start. We can use this to determine
# sample counts without generating individual sample timestamps.
#
# We need to intersect the bursts with the plot's regular bin grid as bursts
# and bin boundaries may overlap irregularly. This intersection allows us to
# tally samples within each individual burst/bin cell and then reaggregate to
# get totals in each bin for each track.
#
# We also intersect the lower and upper limits of the time range window
# as the provided `from` and `to` arguments may split bursts. We need to be
# able then to count only samples within the covered part of these clipped
# bursts.
count_in_bins <- function(start,
                          samp_period,
                          n_samp,
                          ids,
                          bin_width,
                          grid_origin,
                          from = -Inf,
                          to = Inf,
                          call = rlang::caller_env()) {
  if (length(start) == 0L) {
    return(data.frame(id = ids[0], bin_i = numeric(0), n = integer(0)))
  }

  # Identify start and end bins for each burst. If `from`/`to` are provided
  # then clamp to remove bins that fall entirely outside the window.
  start_bin <- pmax(
    floor((start - grid_origin) / bin_width),
    floor((from - grid_origin) / bin_width)
  )
  end_bin <- pmin(
    floor((start + (n_samp - 1) * samp_period - grid_origin) / bin_width),
    floor((to - grid_origin) / bin_width)
  )

  # Bursts that fall outside the clamped time window are removed
  in_window <- start_bin <= end_bin
  start <- start[in_window]
  samp_period <- samp_period[in_window]
  n_samp <- n_samp[in_window]
  ids <- ids[in_window]
  start_bin <- start_bin[in_window]
  end_bin <- end_bin[in_window]

  if (length(start) == 0L) {
    return(data.frame(id = ids[0], bin_i = numeric(0), n = integer(0)))
  }

  # Check that the provided bin width is not so fine as to produce
  # extremely large number of cells
  n_cells <- sum(end_bin - start_bin + 1)

  if (n_cells > max_cells) {
    n_bursts <- length(start)

    if (n_bursts < max_cells) {
      cli::cli_abort(
        c(
          "{.arg bin_width} is too fine for the bursts in these data.",
          i = "Increase {.arg bin_width} or restrict the time range with {.arg from}/{.arg to}."
        ),
        call = call
      )
    } else {
      cli::cli_abort(
        c(
          "Too many input bursts.",
          i = "Restrict the time range with {.arg from}/{.arg to}."
        ),
        call = call
      )
    }
  }

  # We now have all the bins that overlap our time window of interest.
  # The first and last bin may partially overlap the range, though.
  # We need to account for this when computing how many samples fall in each
  # time bin. Further work vectorizes at the (burst, bin) cell level to
  # address this.

  # `burst_i` gives the position of the burst each cell came from, `bin_i` the
  # position on the time grid. Together they are the cell's key.
  burst_i <- rep(seq_along(start), end_bin - start_bin + 1)
  bin_i <- unlist(purrr::map2(start_bin, end_bin, seq.int), use.names = FALSE)

  # Spread each burst's parameters out over its cells so the arithmetic below
  # is plain element-wise vector work.
  burst_start <- start[burst_i]
  burst_period <- samp_period[burst_i]
  burst_n_samp <- n_samp[burst_i]

  # Truncate bin edges for bins that overlap the `from`/`to` limits. We only
  # want to count samples within the window, not within the full bin.
  bin_start <- grid_origin + bin_i * bin_width
  counted_start <- pmax(bin_start, from)
  counted_end <- pmin(bin_start + bin_width, to)

  n_samp_before_end <- samples_before(
    counted_end, burst_start, burst_period, burst_n_samp
  )
  n_samp_before_start <- samples_before(
    counted_start, burst_start, burst_period, burst_n_samp
  )

  # Each row represents a burst/bin intersection and contains that burst's
  # contribution to that bin. At this point we no longer care about individual
  # bursts, just bins within tracks.
  cells <- data.frame(
    id = ids[burst_i],
    bin_i = bin_i,
    n = n_samp_before_end - n_samp_before_start
  )

  # Drop empty bins (this can happen for sparse bursts and fine bin size, where
  # bins are covered but don't actually have samples)
  cells <- cells[cells$n > 0L, , drop = FALSE]

  # Sum bin counts within track and bin position.
  # `rowsum()` returns totals keyed only by group number, so the keys are
  # recovered separately from the first row of each group
  grp_i <- vctrs::vec_group_id(cells[c("id", "bin_i")])
  is_first <- !duplicated(grp_i)
  grp_totals <- rowsum(cells$n, grp_i, reorder = FALSE)[, 1]

  data.frame(
    id = cells$id[is_first],
    bin_i = cells$bin_i[is_first],
    n = as.integer(grp_totals)
  )
}

# How many of a burst's samples fall strictly before the `cutoff` time,
# accounting for partially covered bursts that overlap `from`/`to` bounds.
# Allows us to identify the total number of samples in the `from`/`to` window
# by subtraction above.
samples_before <- function(cutoff, burst_start, burst_period, burst_n_samp) {
  # Get count of sampling intervals elapsed before the `cutoff` time
  n_elapsed <- (cutoff - burst_start) / burst_period
  n_elapsed <- ceiling(n_elapsed - 1e-9) # Handle floating point discrepancies

  # Clamp to 0--burst_n_samp
  pmin(pmax(n_elapsed, 0), burst_n_samp)
}

# Extract start times, sampling period, and number of samples from an input
# `imu` or datetime. This metadata is sufficient to identify the number of
# samples within each burst/bin intersection during plot construction.
burst_timing <- function(x) {
  # If `lane` is an `imu`, take the timing from its metadata. Otherwise `lane`
  # must be a date-like object and we can use the timestamps themselves
  if (inherits(x, "imu")) {
    has_data <- !is.na(x)

    with_data <- x[has_data]

    start <- rep(NA_real_, length(x))
    samp_period <- rep(NA_real_, length(x))
    n_samp <- rep(NA_integer_, length(x))

    start[has_data] <- as.numeric(starts(with_data))
    n_samp[has_data] <- n_samples(with_data)

    # Get time diff in seconds
    samp_period[has_data] <- 1 / as.numeric(freqs(with_data))

    # Single-sample burst has no defined time span. Set `samp_period` positive
    # to avoid filtering out these records
    samp_period[!is.na(n_samp) & n_samp <= 1L] <- 1

    # Remove bursts with no start time, samples, or finite positive duration
    no_timing <- has_data &
      (is.na(start) |
        is.na(n_samp) |
        !is.finite(samp_period) |
        samp_period <= 0)

    if (any(no_timing)) {
      cli::cli_warn(
        "Omitting {sum(no_timing)} burst{?s} with no start time or no usable
         sampling frequency."
      )
    }

    keep <- has_data & !no_timing
    tz <- attr(starts(x), "tzone") %||% ""
  } else if (is_timestamp(x)) {
    x <- timestamp_to_POSIXct(x)
    start <- as.numeric(x)
    n_samp <- rep(1L, length(start))
    samp_period <- rep(1, length(start))

    keep <- !is.na(start)
    tz <- attr(x, "tzone") %||% ""
  }

  out <- data.frame(
    start = ifelse(keep, start, NA_real_),
    samp_period = ifelse(keep, samp_period, NA_real_),
    n_samp = ifelse(keep, as.integer(n_samp), NA_integer_)
  )

  attr(out, "tzone") <- tz

  out
}

# Normalize a bin width to seconds
bin_to_sec <- function(bin_width,
                       arg = rlang::caller_arg(bin_width),
                       call = rlang::caller_env()) {
  if (is.null(bin_width)) {
    return(NULL)
  }

  # Needed to produce correct error message parsing if arg is provided
  # as an external variable
  force(arg)

  if (length(bin_width) != 1L) {
    cli::cli_abort("{.arg {arg}} must be length 1.", call = call)
  }

  # Guard the coercion below, which otherwise fails without argument context
  if (!is.numeric(bin_width) && !inherits(bin_width, c("units", "difftime"))) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls units} object, a {.cls difftime}, or a
       number of seconds, not {.cls {class(bin_width)[1]}}.",
      call = call
    )
  }

  if (inherits(bin_width, "difftime")) {
    bin_width <- units::as_units(bin_width)
  }

  bin_width <- as.numeric(units::set_units(bin_width, "s"))

  if (!is.finite(bin_width) || bin_width <= 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a finite number greater than 0.",
      call = call
    )
  }

  bin_width
}

# Validate timestamps passed to `from`/`to` arguments and convert to seconds
timestamp_to_sec <- function(x,
                             arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }

  # `x` is normalized below, so the lazy default for `arg` has to be resolved
  # while it still refers to what the caller passed
  force(arg)

  # Convert first: it rejects non-timestamps, and the checks below need a plain
  # double. `is.finite()` only gained a `POSIXlt` method in R 4.3 and errors on
  # the underlying list before that, so a `strptime()` result would otherwise
  # fail here rather than being validated.
  x <- timestamp_to_POSIXct(x, arg = arg, call = call)

  if (length(x) != 1L) {
    cli::cli_abort("{.arg {arg}} must be length 1.", call = call)
  }

  # Catches `NA` and an infinite datetime in one
  if (!is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a finite datetime.", call = call)
  }

  as.numeric(x)
}

# Format a bin width for the plot caption.
format_bin <- function(bin_width) {
  unit_secs <- c(1, 60, 3600, 86400)
  unit_names <- c("s", "min", "hour", "day")

  # Ensure we use "s" labels and appropriate digits for sub-second bins
  i <- max(1L, which(bin_width >= unit_secs))
  value <- bin_width / unit_secs[i]

  if (value < 1) {
    value <- signif(value, 2)
  } else {
    value <- round(value, 1)
  }

  paste(value, unit_names[i])
}

# Max number of burst/bin cell combinations we will build in a single call.
# `count_in_bins()` fails if an input bin width causes an exceedance of this
# value.
#
# This attempts to convert an out-of-memory crash into an actionable error when
# processing extreme numbers of bins. Building burst/bin intersect cells costs
# roughly 0.3 MB and 1.6 ms per 1000, so this cap allows a peak of about 1.5 GB
# and 8 s.
#
# There is ample headroom for real work. At the default 300 bins a plot builds
# roughly one cell per burst plus the bins each burst spans, so even a
# deployment with several hundred thousand bursts stays well under the cap.
max_cells <- 5e6

# Default number of bins to divide the time axis into when no `bin_width` is
# provided
default_bins <- 300
