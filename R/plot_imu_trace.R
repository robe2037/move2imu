#' Plot IMU traces over time
#'
#' Plot the trace of IMU values from an IMU vector with time on the x-axis.
#'
#' If the bursts in the input come from multiple sources, traces may be
#' combined incorrectly. See examples.
#'
#' @inheritParams n_axis
#' @param ... Passed to [dygraphs::dygraph()] (for instance, to label axes).
#'
#' @returns A [dygraphs::dygraph()] with one series per IMU axis.
#'
#' @seealso [plot_sampling_effort()] to plot sample collection times.
#'
#' @export
#'
#' @examplesIf rlang::is_installed(c("dygraphs", "move2"))
#' plot_imu_trace(acc_example())
#'
#' # If bursts come from multiple sources (in this case, deployments),
#' # then lines from different bursts may be incorrectly connected:
#' alb <- albatrosses()
#' a <- as_acc(alb)
#'
#' plot_imu_trace(a)
#'
#' # To avoid this issue, plot only a single deployment's values:
#' plot_imu_trace(a[move2::mt_track_id(alb) == "4261-2228"])
#'
#' # Label axes by passing arguments to `dygraph()`:
#' plot_imu_trace(acc_example(), main = "Wing beats", ylab = "Acceleration (g)")
#'
#' # Use other `dygraphs` layers to further modify the plot. For instance,
#' # to label the time axis in UTC instead of the browser time zone:
#' plot_imu_trace(acc_example()) |>
#'   dygraphs::dyOptions(labelsUTC = TRUE)
plot_imu_trace <- function(x, ...) {
  rlang::check_installed(c("dygraphs", "dplyr"))

  time <- starts(x)
  freq <- freqs(x)
  present <- !is.na(x)

  # Only plot bursts that have data, start time, and freq
  keep <- present & !is.na(time) & !is.na(freq)

  if (!any(keep)) {
    cli::cli_abort(
      "Can't plot bursts without start timestamps and sampling frequencies."
    )
  }

  n_no_start <- sum(present & is.na(time))
  if (n_no_start > 0) {
    cli::cli_warn("Omitting {n_no_start} burst{?s} with no start timestamp.")
  }

  n_no_freq <- sum(present & !is.na(time) & is.na(freq))
  if (n_no_freq > 0) {
    cli::cli_warn("Omitting {n_no_freq} burst{?s} with no sampling frequency.")
  }


  dt <- mapply(
    # Convert to seconds before stripping units — otherwise non-Hz
    # frequencies (e.g. stored as "1/min") would yield offsets in minutes
    # that POSIXct silently treats as seconds.
    function(x, n) {
      c(units::drop_units(
        units::set_units((c(0, seq_len(n))) / x, "s")
      ))
    },
    x = freqs(x)[keep],
    n = n_samples(x)[keep],
    SIMPLIFY = FALSE
  )

  df <- dplyr::bind_cols(
    time = do.call("c", mapply("+", time[keep], dt, SIMPLIFY = FALSE)),
    dplyr::bind_rows(
      lapply(bursts(x)[keep], function(x) rbind(data.frame(x), NA))
    )
  )

  dygraphs::dygraph(df, ...)
}
