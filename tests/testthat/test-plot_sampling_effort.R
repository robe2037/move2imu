# ---- plot_sampling_effort() -------------------------------------------------

test_that("plot_sampling_effort builds one tile layer faceted by IDs", {
  skip_if_not_installed("ggplot2")

  p <- plot_sampling_effort(acc = acc_example(), bin_width = 1)

  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$lane), "acc")

  expect_length(p$layers, 1L)
  expect_s3_class(p$layers[[1]]$geom, "GeomTile")

  # Tile layer is faceted only when `ids` is given
  expect_s3_class(p$facet, "FacetNull")
  expect_s3_class(
    plot_sampling_effort(acc_example(), ids = c("x", "y"), bin_width = 1)$facet,
    "FacetGrid"
  )
})

test_that("plot structure is based on the data returned by bin_samples", {
  skip_if_not_installed("ggplot2")

  a <- acc_example()
  ts <- .as.POSIXct(c(0, 5))

  p <- plot_sampling_effort(acc = a, gps = ts, bin_width = 1)

  expect_identical(p$data, bin_samples(acc = a, gps = ts, bin_width = 1))
  expect_equal(rlang::as_label(p$layers[[1]]$mapping$x), "time")
  expect_equal(rlang::as_label(p$layers[[1]]$mapping$y), "lane")

  # Tiles are nudged by half a bin width to ensure left alignment, while
  # the tile still starts at the bin boundary precisely
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_setequal(unique(built$x), unique(as.numeric(p$data$time) + 0.5))
  expect_equal(min(built$xmin), min(as.numeric(p$data$time)))
})

test_that("lanes stack in argument order and are colored in the same order", {
  skip_if_not_installed("ggplot2")

  a <- acc_example()
  ts <- .as.POSIXct(c(0, 5))

  p <- plot_sampling_effort(acc = a, gps = ts, bin_width = 1)

  expect_equal(levels(p$data$lane), c("acc", "gps"))
  expect_equal(
    p$scales$get_scales("y")$limits(levels(p$data$lane)), c("gps", "acc")
  )

  built <- ggplot2::ggplot_build(
    p + ggplot2::scale_fill_manual(values = c("red", "blue"))
  )$data[[1]]

  # `y` is a position along the reversed axis, so read the lane back off it.
  lanes <- rev(levels(p$data$lane))[built$y]

  expect_equal(unique(built$fill[lanes == "acc"]), "red")
  expect_equal(unique(built$fill[lanes == "gps"]), "blue")
})

test_that("a lane with no data keeps its place on the axis", {
  skip_if_not_installed("ggplot2")

  a <- acc(
    list(matrix(1:100, ncol = 1, dimnames = list(NULL, "X"))),
    units::set_units(1, "Hz"),
    start = .as.POSIXct(0)
  )[c(1L, NA, NA, NA)]

  g <- .as.POSIXct(c(NA, 200, 250, 300))

  # Helper to get the y limits used in the plot
  lanes <- function(p) as.character(ggplot2::layer_scales(p)$y$get_limits())

  # Discrete scales drop unused levels by default, but we want to keep
  # all requested lanes, whether they are empty because of a time range
  # restriction (`from`/`to`) or because they don't contain data.
  p <- plot_sampling_effort(acc = a, gps = g, bin_width = 10, to = .as.POSIXct(100))
  expect_setequal(lanes(p), c("acc", "gps"))

  p <- plot_sampling_effort(acc = a, gps = g[rep(NA_integer_, 4)], bin_width = 10)
  expect_setequal(lanes(p), c("acc", "gps"))
})

test_that("plot alpha scale keeps any bin with data visible", {
  skip_if_not_installed("ggplot2")

  built <- ggplot2::ggplot_build(
    plot_sampling_effort(acc_example(), bin_width = 4)
  )$data[[1]]

  expect_gte(min(built$alpha), 0.28)
  expect_equal(max(built$alpha), 1)
})

test_that("plot_sampling_effort reports the bin width in the caption", {
  skip_if_not_installed("ggplot2")

  expect_match(
    plot_sampling_effort(acc_example(), bin_width = 1800)$labels$caption,
    "Bin width: 30 min"
  )
})

test_that("the plotted axis spans the window that was asked for", {
  skip_if_not_installed("ggplot2")

  a <- acc(
    list(matrix(1:100, ncol = 1, dimnames = list(NULL, "X"))),
    units::set_units(1, "Hz"),
    start = .as.POSIXct(0)
  )

  at <- function(s) .as.POSIXct(s)
  hi <- function(p) max(as.numeric(ggplot2::layer_scales(p)$x$get_limits()))
  lo <- function(p) min(as.numeric(ggplot2::layer_scales(p)$x$get_limits()))

  p <- plot_sampling_effort(a, from = at(-600), to = at(600))

  expect_equal(hi(p), 600)
  expect_equal(lo(p), -600)
  expect_equal(hi(plot_sampling_effort(a, to = at(600))), 600)
  expect_equal(lo(plot_sampling_effort(a, from = at(-600))), -600)

  # We expand the range outside of a scale_ function to avoid conflicts
  # if a user provides their own x scale.
  expect_silent(
    ggplot2::ggplot_build(p + ggplot2::scale_x_datetime(date_labels = "%H:%M"))
  )
})

test_that("no fill scale is added", {
  skip_if_not_installed("ggplot2")

  p <- plot_sampling_effort(acc = acc_example())

  expect_null(p$scales$get_scales("fill"))
  expect_no_message(p + ggplot2::scale_fill_brewer(palette = "Dark2"))
})

test_that("correct default theme elements", {
  skip_if_not_installed("ggplot2")

  p <- plot_sampling_effort(
    acc = acc_example(),
    gps = .as.POSIXct(c(0, 5)),
    ids = c("x", "y")
  )

  t <- p$theme

  expect_identical(t$panel.grid.major.y, ggplot2::element_blank())
  expect_identical(t$panel.grid.minor, ggplot2::element_blank())
  expect_equal(t$panel.grid.major.x$linetype, "dashed")
  expect_true(is.na(t$panel.border$fill))
  expect_equal(t$strip.text.y.left$hjust, 1)
  expect_equal(t$strip.placement, "outside")
})

test_that("plot renders", {
  skip_if_not_installed("ggplot2")

  p <- plot_sampling_effort(
    acc = acc_example(),
    gps = .as.POSIXct(c(0, 5)),
    ids = c("x", "y")
  )

  withr::local_pdf(nullfile())
  expect_no_warning(ggplot2::ggplotGrob(p))
})


# ---- bin_samples() ----------------------------------------------------------

test_that("bin_samples returns the documented shape", {
  # acc_example: 30 samples at 20 Hz from t = 0, then 20 from t = 10
  a <- acc_example()
  b <- bin_samples(a, bin_width = 4)

  expect_named(b, c("lane", "time", "n", "rate", "effort"))
  expect_s3_class(b$lane, "factor")
  expect_s3_class(b$time, "POSIXct")
  # Bin starts should carry time zone of the input data, not the session
  expect_equal(attr(b$time, "tzone"), "UTC")
  expect_equal(attr(b, "bin_width"), 4)
  expect_equal(sum(b$n), 50L)
  expect_equal(b$rate, b$n / 4)

  # Correctly attach bin_width attribute in seconds
  expect_equal(
    attr(bin_samples(a, bin_width = units::set_units(1, "min")), "bin_width"),
    60
  )

  # `id` appears only when there is something to group by.
  expect_false("id" %in% names(b))
  b2 <- bin_samples(a, ids = c("x", "y"), bin_width = 4)
  expect_s3_class(b2$id, "factor")
  expect_setequal(levels(b2$id), c("x", "y"))
})

test_that("Use the input argument names to label lanes in the output", {
  d <- list(acc = acc_example(), gps = acc_example())

  expect_equal(
    levels(bin_samples(d$acc, d$gps, bin_width = 1)$lane),
    c("d$acc", "d$gps")
  )
  expect_equal(
    levels(bin_samples(acc = d$acc, d$gps, bin_width = 1)$lane),
    c("acc", "d$gps")
  )
  expect_equal(
    levels(bin_samples(as_acc(d$acc), bin_width = 1)$lane),
    "as_acc(d$acc)"
  )
  expect_equal(
    levels(bin_samples(gps = d$gps, acc = d$acc, bin_width = 1)$lane),
    c("gps", "acc")
  )
})

test_that("Binned samples share one bin grid across lanes", {
  # Tiles must line up vertically across lanes and facets
  # This means that all inputs should snap to same grid/bin edges

  # Build acc and timestamp inputs that fall in same bin cell.
  # grid_origin = floor(3050 / 100) * 100 = 3000
  a <- acc(
    list(cbind(X = 1:10)),
    units::set_units(1, "Hz"),
    start = .as.POSIXct(3050)
  )

  # grid_origin = floor(3055 / 100) * 100 = 3000
  ts <- .as.POSIXct(3055)

  b <- bin_samples(a, ts, bin_width = 100)

  expect_equal(unique(as.numeric(b$time)), 3000)
})

test_that("Default bin width divides the plot time span into 300 bins", {
  .acc <- function(t) {
    acc(
      rep(list(cbind(X = 1:2)), length(t)),
      units::set_units(1, "Hz"),
      start = .as.POSIXct(t)
    )
  }

  # 3 bursts, 1 Hz frequency. Last burst ends at 10001s
  a <- .acc(c(0, 5000, 10000))

  # Span reaches the last *sample*, not the last burst start: two 1 Hz samples
  # extend the range by 1 s past the final start.
  expect_equal(
    attr(bin_samples(a), "bin_width"),
    10001 / 300
  )

  # A lone instant has no span to divide, so the width is arbitrary: whatever
  # it is, it spans the whole plot. We use one second by default.
  expect_equal(attr(bin_samples(.as.POSIXct(0)), "bin_width"), 1)
})

test_that("Only drop NA panels when both ID and data are NA", {
  a <- acc_example()

  # NA in both ID and data represents a true NA element that should be removed
  b <- bin_samples(
    acc = a[c(1L, NA, 2L, NA)],
    ids = c("x", NA, "x", NA),
    bin_width = 1
  )

  expect_equal(levels(b$id), "x")
  expect_identical(
    b,
    bin_samples(acc = a, ids = c("x", "x"), bin_width = 1)
  )

  # When NA is included as an ID, but there are data for that record, treat it
  # as a legitimate ID level
  b2 <- bin_samples(acc = a, ids = factor(c("x", NA)), bin_width = 1)
  expect_true(anyNA(levels(b2$id)))

  # When ID is present, but there aren't data for the record, treat it as a
  # legitimate ID level
  b3 <- bin_samples(acc = a[c(1L, NA)], ids = factor(c("x", "y")), bin_width = 1)
  expect_equal(levels(b3$id), c("x", "y"))
})

test_that("bin_samples keeps an all-NA lane as a level with no rows", {
  # We want to show empty lanes so users can easily see that no data were
  # collected
  a <- acc_example()
  empty <- .as.POSIXct(c(NA, NA))

  b <- bin_samples(a, empty, bin_width = 1)

  expect_true("empty" %in% levels(b$lane))
  expect_false(any(b$lane == "empty"))
})

test_that("bin_samples normalizes effort within each lane", {
  a <- acc_example()
  ts <- .as.POSIXct(c(0, 5))

  b <- bin_samples(acc = a, gps = ts, bin_width = 1)

  expect_equal(b$effort[b$lane == "acc"], c(1, 0.5, 1))
  expect_equal(b$effort[b$lane == "gps"], c(1, 1))
  expect_true(all(b$effort > 0 & b$effort <= 1))
})

test_that("Carry through time zone correctly", {
  # Need to ensure time zone is respected else the grid may become unexpectedly
  # misaligned
  local <- .as.POSIXct(c(0, 3600), "")
  expect_identical(attr(bin_samples(local, bin_width = 60)$time, "tzone"), "")

  utc <- .as.POSIXct(c(0, 3600))
  expect_identical(attr(bin_samples(utc, bin_width = 60)$time, "tzone"), "UTC")

  ny <- .as.POSIXct(c(0, 3600), "America/New_York")
  expect_identical(attr(bin_samples(ny, bin_width = 60)$time, "tzone"), "America/New_York")
})

test_that("disjoint sensors bin correctly from one aligned set of vectors", {
  skip_if_not_installed("move2")
  skip_if_not_installed("sf")

  alb <- albatrosses()
  acc <- as_acc(alb)
  gps <- replace(move2::mt_time(alb), sf::st_is_empty(alb), NA)

  b <- bin_samples(acc, gps, ids = move2::mt_track_id(alb))

  expect_setequal(levels(b$lane), c("acc", "gps"))
  expect_equal(sum(b$n[b$lane == "gps"]), 9L)
  expect_equal(
    sum(b$n[b$lane == "acc"]),
    sum(n_samples(acc), na.rm = TRUE)
  )
})

test_that("bin_samples validates lanes, types and lengths", {
  a <- acc_example()
  ts <- .as.POSIXct(c(0, 10, 20))

  expect_error(bin_samples(ids = 1), "No data provided")
  expect_error(bin_samples(letters[1:3]), "must be an <imu> or timestamp vector")
  expect_error(bin_samples(a, bad = letters[1:2]), "Problems with `bad`")
  expect_error(
    bin_samples(x = letters[1:2], y = letters[1:2]),
    "Problems with `x` and `y`"
  )

  # Length match within `...`
  expect_error(bin_samples(a, ts), "Got lengths 2 and 3")
  expect_error(bin_samples(a, ts, ids = c("x", "y")), "Got lengths 2 and 3")

  # `ids` length match with `...`
  expect_error(bin_samples(ts, ids = c("x", "y")), "Length of `ids` \\(2\\)")
  expect_error(bin_samples(ts, ids = c("x", "y")), "`\\.\\.\\.` \\(3\\)")

  # Unique argument names to `...`
  expect_error(bin_samples(a, a), "unique names")
  expect_no_error(bin_samples(first = a, second = a))

  # Missing start times
  no_start <- acc(
    list(cbind(X = 1:4)),
    units::set_units(1, "Hz"),
    start = as.POSIXct(NA, tz = "UTC")
  )
  expect_error(suppressWarnings(bin_samples(no_start)), "timestamp information")
  expect_error(bin_samples(as.POSIXct(character(0))), "timestamp information")
})

test_that("Warn and drop on improper frequency inputs", {
  a <- acc(
    list(cbind(X = 1:10, Y = 1:10, Z = 1), cbind(X = 1:10, Y = 1:10, Z = 1)),
    frequency = units::set_units(c(0, 20), "Hz"),
    start = .as.POSIXct(c(0, 100))
  )

  expect_warning(b <- bin_samples(a), "Omitting 1 burst")
  expect_equal(sum(b$n), 10L)
})

test_that("error if bin_width is too fine for the input data", {
  a <- acc(
    list(matrix(0, 300, 1, dimnames = list(NULL, "X"))),
    frequency = units::set_units(1, "Hz"),
    start = .as.POSIXct(0)
  )

  expect_error(
    bin_samples(a, bin_width = 1e-6),
    "too fine.+Increase `bin_width`"
  )
})

# ---- from / to windows ------------------------------------------------------

test_that("from and to args clip samples within a burst", {
  a <- acc(
    list(matrix(1:10, ncol = 1, dimnames = list(NULL, "X"))),
    units::set_units(1, "Hz"),
    start = .as.POSIXct(0)
  )

  b <- bin_samples(
    a,
    bin_width = 100,
    from = .as.POSIXct(3),
    to = .as.POSIXct(7)
  )

  expect_equal(sum(b$n), 4L)

  b <- bin_samples(a, bin_width = 100, from = .as.POSIXct(6))
  expect_equal(sum(b$n), 4L)

  b <- bin_samples(a, bin_width = 100, to = .as.POSIXct(6))
  expect_equal(sum(b$n), 6L)
})

test_that("Provided from/to window sets the bin grid and the span", {
  a <- acc(
    list(matrix(1:100, ncol = 1, dimnames = list(NULL, "X"))),
    units::set_units(1, "Hz"),
    start = .as.POSIXct(0)
  )

  # Bins start on `from` even if it is not a multiple of the bin width
  b <- bin_samples(a, bin_width = 4, from = .as.POSIXct(3), to = .as.POSIXct(11))
  expect_equal(as.numeric(min(b$time)), 3)
  expect_equal(as.numeric(max(b$time)), 7)

  # The default bin width is calculated from the overall plot window, not the
  # data span. (Default is 300 bins)
  b <- bin_samples(a, from = .as.POSIXct(0), to = .as.POSIXct(300))
  expect_equal(attr(b, "bin_width"), 1)
})

test_that("bin_samples errors on improper time window specifications", {
  a <- acc_example()

  expect_error(
    bin_samples(a, from = .as.POSIXct(10), to = .as.POSIXct(3)),
    "must be after"
  )
  expect_error(
    bin_samples(a, from = .as.POSIXct(1e6), to = .as.POSIXct(2e6)),
    "No samples fall"
  )

  expect_error(bin_samples(a, from = as.POSIXct(NA)), "`from` must be a finite")
  expect_error(bin_samples(a, to = as.POSIXct(NA)), "`to` must be a finite")
  expect_no_error(bin_samples(a, from = as.Date("1970-01-01")))
})

# ---- count_in_bins() --------------------------------------------------------

test_that("count_in_bins counts bursts within bins correctly", {
  # Whole burst inside one bin: 5 samples at 1 Hz from t = 0, bin_width = 5
  # (last at t = 4).
  expect_equal(
    count_in_bins(0, 1, 5L, "a", bin_width = 5, grid_origin = 0)$n,
    5L
  )

  # One sample spills over the edge: 6 samples, so t = 5 starts the next bin.
  b <- count_in_bins(0, 1, 6L, "a", bin_width = 5, grid_origin = 0)
  expect_equal(b$bin_i, c(0, 1))
  expect_equal(b$n, c(5L, 1L))

  # Straddling an edge on both sides: t = 2,3 then t = 4,5 with bin_width = 4.
  expect_equal(
    count_in_bins(2, 1, 4L, "a", bin_width = 4, grid_origin = 0)$n,
    c(2L, 2L)
  )

  # Several bins with a partial tail: 10 samples, bin_width = 4 -> 4 + 4 + 2.
  b <- count_in_bins(0, 1, 10L, "a", bin_width = 4, grid_origin = 0)
  expect_equal(b$bin_i, c(0, 1, 2))
  expect_equal(b$n, c(4L, 4L, 2L))

  # Samples landing exactly on bin edges must not be double-counted. This is
  # what the 1e-9 tolerance guards.
  expect_equal(
    count_in_bins(0, 5, 3L, "a", bin_width = 5, grid_origin = 0)$n,
    c(1L, 1L, 1L)
  )

  # An instant, as burst_timing() produces it: n_samp = 1, samp_period = 1.
  b <- count_in_bins(7, 1, 1L, "a", bin_width = 4, grid_origin = 0)
  expect_equal(b$bin_i, 1)
  expect_equal(b$n, 1L)
})

test_that("count_in_bins groups by id and bin", {
  # Elements in the same bin with same ID are combined, other IDs are split
  b <- count_in_bins(
    c(0, 1, 2),
    c(1, 1, 1),
    c(2L, 2L, 3L),
    c("a", "a", "b"),
    bin_width = 10,
    grid_origin = 0
  )

  expect_equal(b$id, c("a", "b"))
  expect_equal(b$bin_i, c(0, 0))
  expect_equal(b$n, c(4, 3))
})

test_that("count_in_bins counts relative to the specified origin", {
  b1 <- count_in_bins(100, 1, 2L, "a", bin_width = 10, grid_origin = 100)
  b2 <- count_in_bins(100, 1, 2L, "a", bin_width = 10, grid_origin = 0)
  b3 <- count_in_bins(100, 1, 2L, "a", bin_width = 10, grid_origin = 110)

  expect_equal(b1$bin_i, 0)
  expect_equal(b2$bin_i, 10)
  expect_equal(b3$bin_i, -1)
})

test_that("count_in_bins handles empty input without error", {
  # An all-NA lane arrives here with nothing in it.
  b <- count_in_bins(
    numeric(0),
    numeric(0),
    integer(0),
    character(0),
    bin_width = 10,
    grid_origin = 0
  )

  expect_equal(nrow(b), 0L)
  expect_named(b, c("id", "bin_i", "n"))
})

# ---- burst_timing() ---------------------------------------------------------

test_that("burst_timing extracts burst metadata into data.frame", {
  a <- acc_example()

  b <- burst_timing(a)

  expect_s3_class(b, "data.frame")
  expect_named(b, c("start", "samp_period", "n_samp"))
  expect_equal(nrow(b), length(a))
  expect_equal(b$start, c(0, 10))
  expect_equal(b$samp_period, c(0.05, 0.05))
  expect_equal(b$n_samp, c(30L, 20L))
})

test_that("burst_timing treats timestamps and single-sample bursts as instants", {
  b <- burst_timing(.as.POSIXct(c(0, 60, 120)))

  # We use artificially positive period so single timestamps don't get dropped
  expect_equal(b$samp_period, c(1, 1, 1))
  expect_equal(b$start, c(0, 60, 120))
  expect_equal(b$n_samp, c(1L, 1L, 1L))

  # Similarly, don't fail on a single-sample burst
  a <- acc(
    list(cbind(X = 1)),
    units::set_units(NA, "Hz"),
    start = .as.POSIXct(0)
  )

  expect_silent(b <- burst_timing(a))
  expect_equal(b$n_samp, 1L)
  expect_equal(b$samp_period, 1)

  # A `Date` is an instant too, but counts days rather than seconds, so it has
  # to be converted before it reaches the bin grid. R reads dates in UTC.
  d <- burst_timing(as.Date(c("1970-01-01", "1970-01-05")))

  expect_equal(d$start, c(0, 4 * 86400))
  expect_equal(d$n_samp, c(1L, 1L))
  expect_equal(attr(d, "tzone"), "UTC")
})

test_that("burst_timing handles NA and malformed elements correctly", {
  # A missing element is bookkeeping: lanes are kept the length of `ids` by
  # masking out-of-scope records, so positions must be preserved without
  # comment.
  a <- acc(
    list(cbind(X = 1:4), NULL, cbind(X = 1:2)),
    units::set_units(c(2, NA, 1), "Hz"),
    start = .as.POSIXct(c(0, NA, 100))
  )

  expect_silent(e <- burst_timing(a))
  expect_equal(e$start, c(0, NA, 100))
  expect_equal(e$n_samp, c(4L, NA, 2L))

  # NA timestamps behave the same way: silently ignored, positions preserved.
  expect_silent(e2 <- burst_timing(.as.POSIXct(c(0, NA, 100))))
  expect_equal(e2$start, c(0, NA, 100))

  # An element that holds data but cannot be placed in time is a real defect,
  # and is reported once, counting the bursts dropped.
  d <- acc(
    list(cbind(X = 1:4), cbind(X = 1:4), cbind(X = 1:4)),
    units::set_units(c(2, 2, NA), "Hz"),
    start = .as.POSIXct(c(0, NA, 100))
  )

  expect_warning(e3 <- burst_timing(d), "2 bursts")
  expect_equal(e3$start, c(0, NA, NA))
})

# ---- argument normalization -------------------------------------------------

test_that("bin_to_sec normalizes any time width to seconds", {
  expect_null(bin_to_sec(NULL))
  expect_equal(bin_to_sec(30), 30)
  expect_equal(bin_to_sec(units::set_units(2, "hour")), 7200)
  expect_equal(bin_to_sec(as.difftime(30, units = "mins")), 1800)

  expect_error(bin_to_sec(-5), "greater than 0")
  expect_error(bin_to_sec(NA_real_), "finite")
  expect_error(bin_to_sec(c(1, 2)), "length 1")

  expect_error(bin_to_sec("1 min"), "must be a <units> object")

  # Ensure correct error when passing invalid value as variable
  w <- 0
  expect_error(bin_samples(acc_example(), bin_width = w), "`bin_width` must be")
})

test_that("timestamp_to_sec normalizes a window edge to seconds", {
  expect_null(timestamp_to_sec(NULL))
  expect_equal(timestamp_to_sec(.as.POSIXct(60)), 60)
  expect_equal(timestamp_to_sec(as.Date("1970-01-02")), 86400)

  expect_equal(timestamp_to_sec(60), 60)
  expect_error(timestamp_to_sec("1970-01-01"), "must be a timestamp")
  expect_error(timestamp_to_sec(as.difftime(1, units = "hours")), "must be a timestamp")
  expect_error(timestamp_to_sec(.as.POSIXct(c(0, 60))), "length 1")
  expect_error(timestamp_to_sec(as.POSIXct(NA)), "finite")
  expect_error(timestamp_to_sec(as.Date(NA)), "finite")
  expect_error(timestamp_to_sec(.POSIXct(Inf)), "finite")
})

test_that("format_bin labels sub-second bin widths", {
  expect_equal(format_bin(0.009125), "0.0091 s")
  expect_equal(format_bin(2.998167), "3 s")
  expect_equal(format_bin(1800), "30 min")
  expect_equal(format_bin(86400), "1 day")
})

test_that("`from`/`to` accept POSIXlt", {
  # `is.finite()` has no POSIXlt method before R 4.3, so `timestamp_to_sec()`
  # normalizes before checking. `strptime()` returns POSIXlt.
  a <- acc(
    list(cbind(X = 1:20)),
    frequency = units::set_units(20, "Hz"),
    start = .as.POSIXct(0)
  )

  expect_equal(timestamp_to_sec(as.POSIXlt(.as.POSIXct(60))), 60)
  expect_error(timestamp_to_sec(as.POSIXlt(as.POSIXct(NA))), "finite")

  lt <- strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_no_error(b <- bin_samples(a, from = lt, bin_width = 1))
  expect_equal(as.numeric(min(b$time)), 0)
})

test_that("Lanes and window edges accept every timestamp form", {
  # Numeric lanes and bounds are seconds since the epoch, UTC, matching what a
  # move2 numeric time column means
  b <- bin_samples(gps = c(0, 5), bin_width = 5)
  expect_identical(attr(b$time, "tzone"), "UTC")
  expect_equal(as.numeric(min(b$time)), 0)

  a <- acc(
    list(cbind(X = 1:20)),
    frequency = units::set_units(20, "Hz"),
    start = .as.POSIXct(0)
  )

  # `from`/`to` bound an instant, so every spelling of the same instant agrees
  by_num <- bin_samples(a, from = 0, to = 1, bin_width = 1)
  by_ct <- bin_samples(a, from = .as.POSIXct(0), to = .as.POSIXct(1), bin_width = 1)
  expect_equal(by_num$n, by_ct$n)

  expect_equal(timestamp_to_sec(as.Date("1970-01-02")), 86400)
  expect_equal(timestamp_to_sec(86400), 86400)
})
