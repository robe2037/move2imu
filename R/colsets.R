# Colset constructor -----------------------------------------------------------

#' Specify IMU data columns present in a tabular data source
#'
#' @description
#' Define which columns in a `move2` or `data.frame`
#' contain IMU data. Pass the result as the `colset` argument of [as_acc()],
#' [as_mag()], or [as_gyro()] to convert those columns into an IMU vector.
#'
#' IMU data are stored in two ways:
#'
#' - **Expanded-format** columns store each IMU sample (possibly for multiple axes)
#'   in its own row.
#'
#' - **Compact-format** columns store a burst of IMU samples as a space-delimited
#'   string. This string must be segmented into axis-specific values using
#'   an associated column that indicates the axes present in the burst.
#'   A further column provides the sampling frequency of the burst. All three
#'   of these columns must be present to form a valid compact-format column set.
#'
#' @param x,y,z (Expanded-format) Column name(s) for the X, Y, and/or Z axes.
#' @param bursts (Compact-format) Column name containing the raw burst strings.
#' @param axes (Compact-format) Column name containing the axis labels for
#'   each burst.
#' @param frequency (Compact-format) Column name containing the sampling
#'   frequency for each burst.
#'
#' @returns An `imu_colset` object of type `"expanded"` or `"compact"`.
#'
#' @seealso [as_acc()], [as_mag()], [as_gyro()] to extract IMU data from a
#'   tabular data source.
#'
#'   [active_acc_colsets()], [active_mag_colsets()], [active_gyro_colsets()] to
#'   identify IMU colsets present in a tabular data source.
#'
#'   [movebank_acc_colsets()], [movebank_mag_colsets()], [movebank_gyro_colsets()]
#'   to see column sets provided by Movebank.
#'
#' @export
#'
#' @examples
#' # Expanded-format: one or more axes
#' imu_colset(x = "my_x", y = "my_y", z = "my_z")
#' imu_colset(x = "my_x", y = "my_y")
#'
#' # Compact-format: all three columns required
#' imu_colset(bursts = "my_raw", axes = "my_axes", frequency = "my_freq")
#'
#' @examplesIf rlang::is_installed("move2")
#' # Use a colset to extract IMU data from those columns in a move2 object
#' as_acc(gulls(), colset = imu_colset(x = "acceleration_raw_x"))
imu_colset <- function(x = NULL,
                       y = NULL,
                       z = NULL,
                       bursts = NULL,
                       axes = NULL,
                       frequency = NULL) {
  expanded_args <- purrr::compact(list(X = x, Y = y, Z = z))
  compact_args <- purrr::compact(list(bursts = bursts, axes = axes, frequency = frequency))

  has_expanded <- length(expanded_args) > 0
  has_compact <- length(compact_args) > 0

  if (has_expanded && has_compact) {
    cli::cli_abort(c(
      "Cannot mix expanded-format and compact-format columns in a single imu_colset.",
      "i" = "Use either {.code x}/{.code y}/{.code z} (expanded-format) or {.code bursts}/{.code axes}/{.code frequency} (compact-format)."
    ))
  }

  if (!has_expanded && !has_compact) {
    cli::cli_abort("No IMU data columns specified.")
  }

  if (has_compact) {
    if (length(compact_args) != 3) {
      cli::cli_abort(
        "Compact-format {.fun imu_colset} requires {.code bursts}, {.code axes}, and {.code frequency} columns."
      )
    }

    cols <- unlist(compact_args)
    type <- "compact"
  } else {
    cols <- unlist(expanded_args)
    type <- "expanded"
  }

  new_imu_colset(cols = cols, type = type)
}

#' @export
print.imu_colset <- function(x, ...) {
  cat(paste0(
    "<imu_colset> [\n",
    paste0("  ", names(x), " = \"", unclass(x), "\"", collapse = ",\n"),
    "\n]\n"
  ))

  invisible(x)
}

# Default supported colsets ----------------------------------------------------

#' View standard Movebank IMU data column sets
#'
#' @description
#' Movebank has several standard ways to store data for each IMU sensor. These
#' functions show the recognized columns for each sensor that can be extracted
#' from a `move2` or `data.frame` by default.
#'
#' - `movebank_acc_colsets()` — standard column sets for [as_acc()].
#' - `movebank_mag_colsets()` — standard column sets for [as_mag()].
#' - `movebank_gyro_colsets()` — standard column sets for [as_gyro()].
#'
#' To extract IMU data from columns whose names don't correspond to
#' Movebank's conventions, provide a custom set of IMU columns with
#' [imu_colset()].
#'
#' @details
#' IMU data are stored in two ways:
#'
#' - **Expanded-format** columns store each IMU sample (possibly for multiple axes)
#'   in its own row.
#'
#' - **Compact-format** columns store a burst of IMU samples as a space-delimited
#'   string. This string must be segmented into axis-specific values using
#'   an associated column that indicates the axes present in the burst.
#'   A further column provides the sampling frequency of the burst. All three
#'   of these columns must be present to form a valid compact-format column set.
#'
#' ## Alternate column name separators
#'
#' Some column names may differ depending on how the data were downloaded.
#' The Movebank API (e.g. `move2::movebank_download_study()`) provides columns
#' with `_` separators, while manually downloaded data uses `:` and `-`
#' separators and occasionally includes additional prefixes. For full
#' compatibility, the `active_*_colsets()` functions recognize these alternate
#' spellings as additional column sets even though `movebank_*_colsets()` lists
#' only the standard API names.
#'
#' For future compatibility, consider converting data with
#' the manually-downloaded column names to use `_` separators. To use
#' a custom column set, provide the names explicitly with
#' [imu_colset()].
#'
#' @returns A named list of `imu_colset` objects.
#'
#' @seealso [active_acc_colsets()], [active_mag_colsets()], [active_gyro_colsets()]
#'   to identify column sets present in a tabular data source.
#'
#' @name movebank_colsets
#'
#' @examples
#' movebank_acc_colsets()
#'
#' movebank_mag_colsets()
#'
#' movebank_gyro_colsets()
NULL

#' @export
#' @rdname movebank_colsets
movebank_acc_colsets <- function() {
  list(
    eobs = acc_colset_eobs(),
    raw = acc_colset_raw(),
    acc = acc_colset_acc(),
    xyz = acc_colset_xyz(),
    raw_xyz = acc_colset_raw_xyz()
  )
}

#' @export
#' @rdname movebank_colsets
movebank_mag_colsets <- function() {
  list(
    raw = mag_colset_raw(),
    xyz = mag_colset_xyz(),
    raw_xyz = mag_colset_raw_xyz()
  )
}

#' @export
#' @rdname movebank_colsets
movebank_gyro_colsets <- function() {
  list(
    raw = gyro_colset_raw(),
    xyz = gyro_colset_xyz()
  )
}

# Generate alternately-spelled colset names, since manually-downloaded data from
# Movebank has different column names. These columns are also supported by
# active_*_colsets(), though not exposed officially through movebank_*_colsets()
#
# Only colsets with a genuinely distinct alternate spelling are returned, since
# a colset can have an alternate spelling that is identical to its standard
# spelling (if it has no separators). Retaining these would produce spurious
# "duplicate" colsets.
movebank_alt_colsets <- function(config) {
  alt <- purrr::map(config, to_alt_colset)

  differs <- purrr::map2_lgl(
    config,
    alt,
    function(cols, alt_cols) !identical(unclass(cols), unclass(alt_cols))
  )

  rlang::set_names(alt[differs], paste0(names(config)[differs], "_alt"))
}

# Active colsets in a move2 object ---------------------------------------------

#' Identify IMU columns present in a tabular data source
#'
#' @description
#' Determine the column sets that will be used by default when extracting IMU
#' data from a `move2` or `data.frame`. Column sets are processed
#' independently, but a single source may contain multiple active column sets
#' for one IMU sensor.
#'
#' - `active_acc_colsets()` — column sets used by [as_acc()].
#' - `active_mag_colsets()` — column sets used by [as_mag()].
#' - `active_gyro_colsets()` — column sets used by [as_gyro()].
#'
#' If no active colsets are found, you can use [imu_colset()] to specify
#' a custom set of columns that contain IMU data.
#'
#' @param x A `move2` or `data.frame`.
#'
#' @returns A list of `imu_colset` objects.
#'
#' @name active_colsets
#'
#' @inherit movebank_colsets details
#'
#' @seealso [movebank_acc_colsets()], [movebank_mag_colsets()],
#'   [movebank_gyro_colsets()] for the supported default colsets.
#'
#'   [as_acc()], [as_mag()], [as_gyro()] to extract IMU data from a
#'   tabular data source.
#'
#' @examplesIf rlang::is_installed("move2")
#' active_acc_colsets(albatrosses())
#'
#' # Multiple colsets may be available
#' active_acc_colsets(move2::mt_stack(albatrosses(), gulls()))
#'
#' # Missing expanded-format axes are not included in the set
#' g <- gulls()
#' g$acceleration_raw_x <- NULL
#' active_acc_colsets(g)
#'
#' # Columns with no data are also removed
#' g$acceleration_raw_y <- NA
#' active_acc_colsets(g)
#'
#' # Some column sets must be present in their entirety
#' alb <- albatrosses()
#' alb$eobs_acceleration_axes <- NULL
#'
#' \dontrun{
#' active_acc_colsets(alb)
#' }
NULL

#' @export
#' @rdname active_colsets
active_acc_colsets <- function(x) {
  active_colsets_(x, "acc")
}

#' @export
#' @rdname active_colsets
active_mag_colsets <- function(x) {
  active_colsets_(x, "mag")
}

#' @export
#' @rdname active_colsets
active_gyro_colsets <- function(x) {
  active_colsets_(x, "gyro")
}

active_colsets_ <- function(x, sensor) {
  force(x)

  config <- switch(sensor,
    acc = movebank_acc_colsets(),
    mag = movebank_mag_colsets(),
    gyro = movebank_gyro_colsets()
  )

  # Ensure that manually-downloaded column spellings (with `:`/`-`) are
  # recognized as valid "alternate" spellings. These are treated as separate
  # colsets alongside the ones officially supported via movebank_*_colsets()
  config <- c(config, movebank_alt_colsets(config))

  colsets <- purrr::compact(
    purrr::map(config, function(colset) colset_active(colset, x))
  )

  if (length(colsets) == 0) {
    abort_missing_colset(sensor)
  }

  colsets
}

#' Identify data rows with multiple sources of IMU data
#'
#' @description
#' Return a logical vector flagging rows of a `move2` or `data.frame` where more
#' than one column set for a given sensor contains data. Functions that extract
#' IMU data will error if a single row contains multiple sources of IMU data
#' for the same sensor.
#'
#' To resolve duplicated rows, pass a specific set of IMU columns to the
#' `colset` argument of `as_*()` or remove the duplicated data.
#'
#' - `duplicated_acc_rows()` — checks acceleration column sets used by [as_acc()].
#' - `duplicated_mag_rows()` — checks magnetometer column sets used by [as_mag()].
#' - `duplicated_gyro_rows()` — checks gyroscope column sets used by [as_gyro()].
#'
#' @param x A `move2` or `data.frame`.
#' @param colsets A list of `imu_colset` objects to check for overlap. Defaults
#'   to the column sets detected by the corresponding `active_*_colsets()`.
#'
#' @returns A logical vector of length `nrow(x)` with `TRUE` values indicating
#'   rows that contain multiple sources of IMU data across the indicated
#'   column sets.
#'
#' @name duplicated_rows
#'
#' @keywords internal
#'
#' @seealso [active_acc_colsets()], [active_mag_colsets()],
#'   [active_gyro_colsets()] to identify available column sets in a
#'   tabular data source.
#'
#'   [as_acc()], [as_mag()], [as_gyro()] to extract IMU data from a
#'   tabular data source.
NULL

#' @export
#' @rdname duplicated_rows
duplicated_acc_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_acc_colsets(x))
}

#' @export
#' @rdname duplicated_rows
duplicated_mag_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_mag_colsets(x))
}

#' @export
#' @rdname duplicated_rows
duplicated_gyro_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_gyro_colsets(x))
}

duplicated_imu_rows <- function(x, colsets = NULL) {
  # Standardize case where user supplied a single colset as a vector
  if (!rlang::is_list(colsets)) {
    colsets <- list(colsets)
  }

  rows <- unlist(
    purrr::map(
      colsets,
      function(cols) which_imu_vals(x, colset = cols)
    )
  )

  # A row is duplicated if more than one colset supplies data for it.
  tabulate(rows, nbins = nrow(x)) > 1
}

# Colset constructor helpers ---------------------------------------------------

# Internal constructor for `imu_colset` objects. Colsets are IMU-class-agnostic:
# the same colset can be passed to `as_acc()`, `as_mag()`, or `as_gyro()` -
# the IMU class is determined by which converter you call, not by the colset.
#
# The format ("expanded"/"compact") is stored in the `imu_colset_<type>`
# subclass. This enables S3 dispatch for behavior that differs across compact
# and expanded colsets (e.g. compact requires all cols, expanded allows a
# subset). Use `colset_type()` to recover the format as a string.
new_imu_colset <- function(cols, type) {
  type <- rlang::arg_match(type, c("expanded", "compact"))

  structure(
    cols,
    class = c(paste0("imu_colset_", type), "imu_colset", class(cols))
  )
}

is_imu_colset <- function(x) {
  inherits(x, "imu_colset")
}

# Recover a colset's format ("compact"/"expanded") from its subclass.
colset_type <- function(x) {
  if (inherits(x, "imu_colset_compact")) {
    "compact"
  } else if (inherits(x, "imu_colset_expanded")) {
    "expanded"
  } else {
    cli::cli_abort("{.arg x} must be an {.cls imu_colset}.")
  }
}

# Colset config ----------------------------------------------------------------

acc_colset_eobs <- function() {
  new_imu_colset(
    cols = c(
      bursts = "eobs_accelerations_raw",
      axes = "eobs_acceleration_axes",
      frequency = "eobs_acceleration_sampling_frequency_per_axis"
    ),
    type = "compact"
  )
}

acc_colset_raw <- function() {
  new_imu_colset(
    cols = c(
      bursts = "accelerations_raw",
      axes = "acceleration_axes",
      frequency = "acceleration_sampling_frequency_per_axis"
    ),
    type = "compact"
  )
}

acc_colset_acc <- function() {
  new_imu_colset(
    cols = c(
      bursts = "accelerations",
      axes = "acceleration_axes",
      frequency = "acceleration_sampling_frequency_per_axis"
    ),
    type = "compact"
  )
}

acc_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "acceleration_x",
      Y = "acceleration_y",
      Z = "acceleration_z"
    ),
    type = "expanded"
  )
}

acc_colset_raw_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "acceleration_raw_x",
      Y = "acceleration_raw_y",
      Z = "acceleration_raw_z"
    ),
    type = "expanded"
  )
}

mag_colset_raw <- function() {
  new_imu_colset(
    cols = c(
      bursts = "magnetic_fields_raw",
      axes = "magnetic_field_axes",
      frequency = "magnetic_field_sampling_frequency_per_axis"
    ),
    type = "compact"
  )
}

mag_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "magnetic_field_x",
      Y = "magnetic_field_y",
      Z = "magnetic_field_z"
    ),
    type = "expanded"
  )
}

mag_colset_raw_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "magnetic_field_raw_x",
      Y = "magnetic_field_raw_y",
      Z = "magnetic_field_raw_z"
    ),
    type = "expanded"
  )
}

gyro_colset_raw <- function() {
  new_imu_colset(
    cols = c(
      bursts = "angular_velocities_raw",
      axes = "gyroscope_axes",
      frequency = "gyroscope_sampling_frequency_per_axis"
    ),
    type = "compact"
  )
}

gyro_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "angular_velocity_x",
      Y = "angular_velocity_y",
      Z = "angular_velocity_z"
    ),
    type = "expanded"
  )
}

# Colset predicates (S3, dispatched on format subclass) ------------------------

# Determine whether a colset is "active" in `x`. Active colsets
# are present and contain data in all necessary columns. Compact colsets
# require all columns in the set to be present and contain data. Expanded
# colsets only require a subset of the columns to be present and have data.
colset_active <- function(colset, x) {
  UseMethod("colset_active")
}

#' @export
colset_active.imu_colset_compact <- function(colset, x) {
  # All compact cols must be present
  if (!all(colset %in% colnames(x))) {
    return(NULL)
  }

  # All compact cols must have data
  if (any(cols_empty(x, colset))) {
    return(NULL)
  }

  colset
}

#' @export
colset_active.imu_colset_expanded <- function(colset, x) {
  # Some axes may be present but not have data. Identify the axes that are
  # present and have data.
  present <- colset[colset %in% colnames(x)]
  present <- present[!cols_empty(x, present)]

  if (length(present) == 0) {
    return(NULL)
  }

  new_imu_colset(present, type = "expanded")
}

# General helpers --------------------------------------------------------------

# Convert Movebank API column names to their manual-download equivalent.
# This includes updating underscores to `-` and `:` where appropriate and
# reinstating the `mag:` prefix for magnetometer columns. This is the inverse
# of `to_download_names()` from move2, restricted to the IMU columns we support.
#
# This allows us to detect and parse manually-downloaded IMU cols alongside
# the standard Movebank API column names.
to_alt_names <- function(x) {
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- sub("^eobs-", "eobs:", x)
  sub("^magnetic-", "mag:magnetic-", x)
}

# Convert a supported colset to its alternately-named format
to_alt_colset <- function(colset) {
  new_imu_colset(
    rlang::set_names(to_alt_names(unclass(colset)), names(colset)),
    type = colset_type(colset)
  )
}

cols_empty <- function(x, cols) {
  assert_all_cols_present(x, cols)
  purrr::map_lgl(
    cols,
    function(col) all(is.na(unlist(x[[col]]))) || all(rlang::is_empty(unlist(x[[col]])))
  )
}

assert_all_cols_present <- function(x, cols, call = rlang::caller_env()) {
  if (!all(cols %in% colnames(x))) {
    cols <- cols[which(!cols %in% colnames(x))]

    cli::cli_abort(
      c(
        "Missing columns provided.",
        "x" = "Could not find column{?s} {.val {cols}}."
      ),
      call = call
    )
  }
}

abort_missing_colset <- function(sensor, call = rlang::caller_env()) {
  fn <- paste0("movebank_", sensor, "_colsets")
  cli::cli_abort(
    c(
      "Could not identify a full {sensor} column set in the input data.",
      "i" = "Use {.fn {fn}} to see supported {sensor} column sets."
    ),
    class = "move2imu_no_active_colset",
    call = call
  )
}
