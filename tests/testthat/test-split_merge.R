test_that("Can combine adjacent bursts into single burst", {
  # 5 bursts at 10 Hz, 30 samples each (3s duration). First 3 are adjacent,
  # then a gap, then 1 standalone, then another gap — should merge into 3.
  a <- acc(
    c(
      acc_burst_example(1:30, 1:30),
      acc_burst_example(31:60, 31:60),
      acc_burst_example(61:90, 61:90),
      acc_burst_example(91:120, 91:120),
      acc_burst_example(121:150, 121:150)
    ),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3, 6, 20, 50))
  )

  a2 <- merge_imu(a, drop = TRUE)

  expect_true(is_acc(a2))
  expect_length(a2, 3)

  expect_equal(n_samples(a2), as.integer(c(90, 30, 30)))
  expect_identical(starts(a2), .as.POSIXct(c(0, 20, 50)))

  # Merged burst data matches concatenated originals
  expect_equal(
    bursts(a2)[[1]],
    do.call(rbind, bursts(a)[1:3])
  )
  expect_equal(bursts(a2)[[2]], bursts(a)[[4]])
  expect_equal(bursts(a2)[[3]], bursts(a)[[5]])
})

test_that("Can merge with drop = FALSE", {
  d <- albatrosses_df()
  d$id <- rep("tmp", nrow(d))
  d$timestamp <- seq(
    min(d$timestamp),
    by = "12 s",
    length.out = nrow(d)
  )
  a <- as_acc_df(d, merge_continuous = TRUE, drop = FALSE)

  expect_length(a, nrow(d))
  expect_length(a[!is.na(a)], 9)
  expect_identical(a[!is.na(a)], as_acc_df(d, drop = TRUE))
})

test_that("Same merged result if drop = TRUE regardless of NAs", {
  # Construct an acc with NAs interspersed (simulating drop = FALSE output)
  a1 <- acc(
    c(
      acc_burst_example(1:30, 1:30),
      acc_burst_example(31:60, 31:60),
      acc_burst_example(61:90, 61:90)
    ),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3, 6))
  )
  a2 <- c(
    a1[1],
    acc(list(NULL), units::set_units(NA, "Hz")),
    a1[2:3]
  )
  expect_identical(merge_imu(a1, drop = TRUE), merge_imu(a2, drop = TRUE))
})

test_that("Can combine adjacent bursts with embedded NA", {
  # 4 bursts with an NA at position 3. Bursts 1-2 are adjacent and should
  # merge. Bursts 4-5 are adjacent and should merge. The NA is skipped.
  a <- c(
    acc(
      c(acc_burst_example(1:30), acc_burst_example(31:60)),
      frequency = units::set_units(10, "Hz"),
      start = .as.POSIXct(c(0, 3))
    ),
    acc(list(NULL), units::set_units(NA, "Hz")),
    acc(
      c(acc_burst_example(61:90), acc_burst_example(91:120)),
      frequency = units::set_units(10, "Hz"),
      start = .as.POSIXct(c(20, 23))
    )
  )

  a2 <- merge_imu(a, drop = TRUE)

  expect_true(is_acc(a2))
  expect_length(a2, 2)
  expect_equal(n_samples(a2), as.integer(c(60, 60)))
  expect_identical(starts(a2), .as.POSIXct(c(0, 20)))

  # drop = FALSE should only change indexing, not merged values
  a3 <- merge_imu(a, drop = FALSE)

  expect_length(a3, length(a))
  expect_identical(which(!is.na(a3)), c(1L, 4L))
  expect_identical(a3[!is.na(a3)], a2)
})

test_that("drop = FALSE places merged bursts at correct indices", {
  d <- albatrosses_df()
  d$id <- rep("tmp", nrow(d))
  d$timestamp <- seq(
    min(d$timestamp),
    by = "12 s",
    length.out = nrow(d)
  )
  a <- as_acc_df(d, merge_continuous = TRUE, drop = FALSE)

  # Start times of merged bursts should match the dropped version
  expect_identical(starts(a[!is.na(a)]), starts(as_acc_df(d, drop = TRUE)))

  # Positions of non-NA entries should be a subset of the original burst positions
  a_raw <- as_acc_df(d, merge_continuous = FALSE, drop = FALSE)
  expect_true(all(which(!is.na(a)) %in% which(!is.na(a_raw))))
})

test_that("Non-mergeable bursts ignore merge arg regardless of drop arg", {
  g1 <- as_acc_df(gulls_df(), drop = FALSE, colset = acc_colset_raw_xyz())
  g2 <- as_acc_df(gulls_df(), merge_continuous = FALSE, drop = FALSE, colset = acc_colset_raw_xyz())
  expect_identical(g1, g2)
})

test_that("Partial merge with drop = FALSE respects ID boundaries", {
  # 4 adjacent bursts, same freq, but IDs split at position 3
  a <- acc(
    c(
      acc_burst_example(1:30),
      acc_burst_example(31:60),
      acc_burst_example(61:90),
      acc_burst_example(91:120)
    ),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3, 6, 9))
  )

  merged <- merge_imu(a, ids = c("a", "a", "b", "b"), drop = FALSE)

  expect_length(merged, 4)
  expect_identical(which(!is.na(merged)), c(1L, 3L))
  expect_equal(n_samples(merged[1]), as.integer(nrow(bursts(a)[[1]]) + nrow(bursts(a)[[2]])))
  expect_equal(n_samples(merged[3]), as.integer(nrow(bursts(a)[[3]]) + nrow(bursts(a)[[4]])))
})

test_that("Do not combine bursts with different axes", {
  # 3 adjacent bursts: XYZ, XY, XYZ. Middle burst has different axes so
  # none should merge despite being temporally adjacent.
  b_xyz1 <- matrix(1:30, ncol = 3, dimnames = list(NULL, c("X", "Y", "Z")))
  b_xy <- matrix(1:10, ncol = 2, dimnames = list(NULL, c("X", "Y")))
  b_xyz2 <- matrix(31:45, ncol = 3, dimnames = list(NULL, c("X", "Y", "Z")))

  a <- acc(
    list(b_xyz1, b_xy, b_xyz2),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 1, 1.5))
  )

  expect_identical(merge_imu(a), a)
  expect_identical(
    purrr::map(bursts(a), colnames),
    list(c("X", "Y", "Z"), c("X", "Y"), c("X", "Y", "Z"))
  )
})

test_that("Do not combine bursts with different frequencies", {
  # 3 adjacent bursts with frequencies 10, 20, 10 Hz. Frequency mismatch
  # should prevent any merging.
  a <- acc(
    c(
      acc_burst_example(1:20, 1:20),
      acc_burst_example(21:40, 21:40),
      acc_burst_example(41:50, 41:50)
    ),
    frequency = units::set_units(c(10, 20, 10), "Hz"),
    start = .as.POSIXct(c(0, 2, 3))
  )

  expect_identical(merge_imu(a), a)
  expect_identical(as.numeric(freqs(a)), c(10, 20, 10))
})

test_that("Do not combine bursts with different IDs", {
  # 4 adjacent bursts, same freq/axes, but ids split at position 3-4
  a <- acc(
    c(
      acc_burst_example(1:30),
      acc_burst_example(31:60),
      acc_burst_example(61:90),
      acc_burst_example(91:120)
    ),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3, 6, 9))
  )

  a2 <- merge_imu(a, ids = c(1, 1, 1, 2), drop = TRUE)

  expect_length(a2, 2)
  expect_equal(n_samples(a2), as.integer(c(90, 30)))
})

test_that("Do not combine bursts with different units", {
  # 4 adjacent bursts, same freq/axes. Bursts 1-2 in m/s^2, burst 3 unitless,
  # burst 4 in g. None of the boundaries should merge.
  a <- acc(
    c(
      acc_burst_example(1:30),
      acc_burst_example(31:60),
      acc_burst_example(61:90),
      acc_burst_example(91:120)
    ),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3, 6, 9))
  )

  a_mixed <- c(
    set_imu_units(a[1:2], "m/s^2"),
    a[3],
    set_imu_units(a[4], "standard_free_fall")
  )

  # Bursts 1-2 share units and should merge; boundaries 2-3 (units vs
  # unitless) and 3-4 (unitless vs convertible-but-different units) must not.
  merged <- merge_imu(a_mixed, drop = TRUE)
  expect_length(merged, 3)
  expect_equal(n_samples(merged), as.integer(c(60, 30, 30)))
  expect_identical(units::deparse_unit(bursts(merged)[[1]]), "m s-2")
  expect_false(inherits(bursts(merged)[[2]], "units"))
  expect_identical(units::deparse_unit(bursts(merged)[[3]]), "standard_free_fall")

  # Bursts with the same units should still merge among themselves
  a_same <- set_imu_units(a, "m/s^2")

  merged_same <- merge_imu(a_same, drop = TRUE)

  expect_length(merged_same, 1)
  expect_equal(n_samples(merged_same), 120L)

  merged_unitless <- merge_imu(a, drop = TRUE)

  expect_length(merged_unitless, 1)
  expect_equal(n_samples(merged_same), n_samples(merged_unitless))
})

test_that("Don't combine bursts without start time", {
  a <- acc(
    c(acc_burst_example(x = 1:10), acc_burst_example(x = 1:10)),
    frequency = units::set_units(1, "Hz")
  )

  expect_identical(a, merge_imu(a))
})

test_that("Handle empty acc vectors when binding", {
  expect_identical(merge_imu(acc()), acc())
  expect_identical(merge_imu(c(acc(), acc())), acc())
})

test_that("split_imu() on empty acc returns empty list", {
  expect_identical(split_imu(acc(), 1), list())
})

test_that("split_imu() on single-element acc returns length-1 list", {
  a <- acc(
    acc_burst_example(1:20),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(0)
  )

  sp <- split_imu(a, 0.5)

  expect_length(sp, 1)
  expect_true(is_acc(sp[[1]]))
  expect_length(sp[[1]], 4)
  expect_identical(merge_imu(purrr::reduce(sp, c), drop = TRUE), a)
})

test_that("Can split acc at a given interval", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = .as.POSIXct(c(0, 10))
  )

  interval <- 0.5
  split <- split_imu(a, interval = interval)

  # Returns a list the same length as the input
  expect_length(split, length(a))
  expect_true(is.list(split))
  expect_true(all(purrr::map_lgl(split, is_acc)))

  # Individual elements contain the expected number of split bursts
  expect_length(split[[1]], 6)
  expect_length(split[[2]], 2)

  # Flatten to check burst properties
  flat <- purrr::reduce(split, c)

  expect_true(all(units::drop_units(burst_dur(flat)) == interval))

  expect_equal(
    purrr::map_int(bursts(flat), nrow),
    c(rep(10, 6), rep(20, 2))
  )
  expect_equal(
    do.call(rbind, bursts(flat)[1:6]),
    bursts(a)[[1]]
  )
  expect_equal(
    do.call(rbind, bursts(flat)[7:8]),
    bursts(a)[[2]]
  )
  expect_equal(
    freqs(flat),
    units::set_units(c(rep(20, 6), rep(40, 2)), "Hz")
  )
  expect_identical(
    starts(a)[1] + cumsum(c(0, rep(interval, 5))),
    starts(flat)[1:6]
  )
  expect_identical(
    starts(a)[2] + cumsum(c(0, interval)),
    starts(flat)[7:8]
  )
})

test_that("Correctly split when burst length not divisible by interval", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = .as.POSIXct(c(0, 10))
  )

  interval <- 0.7
  split <- split_imu(a, interval = interval)
  flat <- purrr::reduce(split, c)
  dur <- burst_dur(a)

  expect_length(flat, units::drop_units(sum(ceiling(burst_dur(a) / interval))))

  # Bursts should be split into equal time lengths other than for the last
  # element of each split burst, which will capture whatever burst duration remains
  expect_equal(
    units::drop_units(burst_dur(flat)),
    c(
      c(rep(interval, dur[1] %/% interval), dur[1] - (interval * dur[1] %/% interval)),
      c(rep(interval, dur[2] %/% interval), dur[2] - (interval * dur[2] %/% interval))
    )
  )
})

test_that("split_imu() retains NA", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), new_burst_list(list(NULL), "acc"), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(NA, "Hz"), units::set_units(40, "Hz")),
    start = .as.POSIXct(c(0, 10, 10))
  )

  sp <- split_imu(a, 0.5)

  expect_length(sp, length(a))

  # NA element produces a length-1 NA acc
  expect_length(sp[[2]], 1)
  expect_true(is.na(sp[[2]]))

  # Flattened non-NA results match splitting only the non-NA input
  flat <- purrr::reduce(sp, c)
  flat_no_na <- purrr::reduce(split_imu(a[!is.na(a)], 0.5), c)
  expect_identical(flat[!is.na(flat)], flat_no_na)
})

test_that("Can recover split continuous data by merging", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = .as.POSIXct(c(0, 10))
  )

  flat <- purrr::reduce(split_imu(a, interval = 0.5), c)
  expect_identical(merge_imu(flat, drop = TRUE), a)
})

test_that("Can recover split continuous data by merging with NA", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), new_burst_list(list(NULL), "acc"), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(NA, "Hz"), units::set_units(40, "Hz")),
    start = .as.POSIXct(c(0, 10, 10))
  )

  flat <- purrr::reduce(split_imu(a, interval = 0.5), c)
  expect_identical(merge_imu(flat, drop = TRUE), a[!is.na(a)])
})

test_that("split_imu() preserves 1-sample bursts", {
  a <- acc(
    c(acc_burst_example(42, 43), acc_burst_example(1:20, 1:20)),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 5))
  )

  sp <- split_imu(a, 0.5)

  # 1-sample burst should pass through unchanged
  expect_length(sp[[1]], 1)
  expect_false(is.na(sp[[1]]))
  expect_identical(sp[[1]], a[1])

  # Multi-sample burst still splits normally
  expect_length(sp[[2]], 4)

  # Round-trip preserves the 1-sample burst
  flat <- purrr::reduce(sp, c)
  expect_identical(merge_imu(flat, drop = TRUE), a)
})

test_that("Long intervals do not modify input acc", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = .as.POSIXct(c(0, 10))
  )

  split <- split_imu(a, interval = max(burst_dur(a)))
  expect_identical(purrr::reduce(split, c), a)
})

test_that("Can standardize interval units when splitting", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = .as.POSIXct(c(0, 10))
  )

  split <- split_imu(a, interval = 0.5)
  flat <- purrr::reduce(split, c)

  # Frequency is stored in Hz, so a bare-numeric interval is in seconds
  # (the implied period unit is 1/Hz = s); an explicit time unit is
  # standardized to the same interval.
  expect_length(flat, 8)
  expect_identical(
    split,
    split_imu(a, interval = units::set_units(500, "ms"))
  )
})

test_that("split_imu() errors on invalid interval", {
  a <- acc(
    acc_burst_example(1:20),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(0)
  )

  expect_error(split_imu(a, 0), "`interval` must be a positive")
  expect_error(split_imu(a, -1), "`interval` must be a positive")
})

test_that("merge_imu validates ids length", {
  a <- acc(
    c(acc_burst_example(1:30), acc_burst_example(31:60), acc_burst_example(61:90)),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3, 6))
  )

  expect_error(merge_imu(a, ids = c(1, 1)), "same length")
})

test_that("split_imu start times track actual samples for non-integer chunks", {
  # 0.125 s at 20 Hz = 2.5 samples/chunk: chunk sizes alternate, so a fixed
  # `interval` step would drift. Starts must match each chunk's first sample.
  b <- acc(
    list(cbind(X = 1:30)),
    frequency = units::set_units(20, "Hz"),
    start = .as.POSIXct(0)
  )
  sp <- split_imu(b, 0.125)[[1]]

  first_idx <- cumsum(c(1L, utils::head(n_samples(sp), -1)))
  expected <- .as.POSIXct(0) + (first_idx - 1) / 20

  expect_identical(starts(sp), expected)
})

test_that("split_imu() passes through non-empty bursts with a missing frequency", {
  a_naf <- acc(
    list(cbind(X = 1:5)),
    frequency = units::set_units(NA, "Hz"),
    start = .as.POSIXct(0)
  )

  split <- split_imu(a_naf, 0.5)
  expect_length(split, 1)
  expect_length(split[[1]], 1)
  expect_equal(n_samples(split[[1]]), 5L)
  expect_equal(as.numeric(bursts(split[[1]])[[1]]), 1:5)

  # In a mixed vector the good burst still splits; the NA-freq one passes through
  mix <- c(
    acc(
      list(cbind(X = 1:40)),
      frequency = units::set_units(20, "Hz"),
      start = .as.POSIXct(0)
    ),
    a_naf
  )
  split_mix <- split_imu(mix, 0.5)
  expect_length(split_mix, 2)
  expect_length(split_mix[[1]], 4L) # 40 samples / (0.5 s * 20 Hz = 10) = 4
  expect_length(split_mix[[2]], 1L) # NA-freq burst unsplit
})

test_that("split_imu() round-trip in dataframe workflow", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  # Covers normal bursts, adjacent bursts, NA element, and 1-sample burst
  a <- acc(
    c(
      acc_burst_example(1:60, 1:60),
      acc_burst_example(61:100, 61:100),
      new_burst_list(list(NULL), "acc"),
      acc_burst_example(42, 43),
      acc_burst_example(101:140)
    ),
    frequency = c(
      units::set_units(20, "Hz"), units::set_units(20, "Hz"),
      units::set_units(NA, "Hz"),
      units::set_units(10, "Hz"), units::set_units(40, "Hz")
    ),
    start = .as.POSIXct(c(0, 3, 10, 20, 30))
  )

  tbl <- tibble::tibble(
    id = c("x", "x", "y", "z", "z"),
    a = a,
    row_id = seq_len(5)
  )

  # Split, unnest, re-merge with row_id to prevent cross-row merging, filter
  result <- tbl |>
    dplyr::mutate(a = split_imu(a, units::set_units(1, "s"))) |>
    tidyr::unnest(a) |>
    dplyr::mutate(a2 = merge_imu(a, ids = row_id, drop = FALSE)) |>
    dplyr::filter(!is.na(a2))

  # NA row drops after filter, all others recover
  expect_equal(nrow(result), 4)
  expect_identical(result$id, c("x", "x", "z", "z"))
  expect_identical(result$a2, a[!is.na(a)])
})

test_that("merge_imu() errors on invalid tolerances", {
  a <- acc(
    c(acc_burst_example(1:30, 1:30), acc_burst_example(31:60, 31:60)),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3))
  )

  expect_error(merge_imu(a, gap_tol = -1), "`gap_tol` must be greater than")
  expect_error(merge_imu(a, freq_tol = -1), "`freq_tol` must be greater than")
  expect_error(
    merge_imu(a, freq_tol = units::set_units(1e-2, "Hz")),
    "`freq_tol` must be a bare numeric value"
  )
})

test_that("gap_tol bridges a small boundary glitch", {
  # Two 10 Hz bursts (30 samples = 3 s each). The second starts 0.5 ms late, so
  # they are not exactly adjacent.
  a <- acc(
    c(
      acc_burst_example(1:30, 1:30),
      acc_burst_example(31:60, 31:60)
    ),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3.0005))
  )

  # Default gap_tol keeps them separate (exact adjacency required).
  expect_length(merge_imu(a, drop = TRUE), 2)

  # A gap_tol larger than the glitch merges them into one burst.
  merged <- merge_imu(a, gap_tol = 0.001, drop = TRUE)
  expect_length(merged, 1)
  expect_equal(n_samples(merged), 60L)
  expect_identical(starts(merged), as.POSIXct(0, tz = "UTC", origin = "1970-01-01"))
})

test_that("freq_tol controls merging across a small frequency glitch", {
  # Two exactly-abutting bursts; the second's frequency is a hair different
  # (10 vs 10.001 Hz, a 0.01% relative difference).
  a <- acc(
    c(
      acc_burst_example(1:30, 1:30),
      acc_burst_example(31:60, 31:60)
    ),
    frequency = units::set_units(c(10, 10.001), "Hz"),
    start = .as.POSIXct(c(0, 3))
  )

  # freq_tol is relative: the default (1%) treats a sub-percent glitch as
  # the same frequency, so the bursts merge.
  expect_length(merge_imu(a, drop = TRUE), 1)

  # A freq_tol tighter than the 1e-4 relative difference keeps them apart.
  expect_length(merge_imu(a, freq_tol = 1e-5, drop = TRUE), 2)
})

test_that("freq_tol is symmetric in burst order", {
  f1 <- 10
  f2 <- 10.001
  
  a1 <- acc(
    c(acc_burst_example(1:30, 1:30), acc_burst_example(31:60, 31:60)),
    frequency = units::set_units(c(f1, f2), "Hz"),
    start = .as.POSIXct(c(0, 30 / f1))
  )
  
  a2 <- acc(
    c(acc_burst_example(1:30, 1:30), acc_burst_example(31:60, 31:60)),
    frequency = units::set_units(c(f2, f1), "Hz"),
    start = .as.POSIXct(c(0, 30 / f2))
  )
  
  m1 <- merge_imu(a1, drop = TRUE)
  m2 <- merge_imu(a2, drop = TRUE)

  expect_identical(m1, m2)
  expect_equal(as.numeric(freqs(m1)), 10.0005)
  expect_equal(n_samples(m1), 60)
})

test_that("merged frequency is recomputed from the burst span", {
  # Two exactly-adjacent 10 Hz bursts merge to exactly 10 Hz (frequency
  # recomputation is faithful for clean data).
  a <- acc(
    c(
      acc_burst_example(1:30, 1:30),
      acc_burst_example(31:60, 31:60)
    ),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3))
  )
  expect_equal(as.numeric(freqs(merge_imu(a, drop = TRUE))), 10)

  # With a 0.5 ms boundary gap absorbed, the merged freq reflects the true
  # merged burst time span (59 intervals over 5.9005 s), slightly below 10 Hz.
  b <- acc(
    c(
      acc_burst_example(1:30, 1:30),
      acc_burst_example(31:60, 31:60)
    ),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(c(0, 3.0005))
  )
  merged <- merge_imu(b, gap_tol = 0.001, drop = TRUE)
  expect_equal(as.numeric(freqs(merged)), signif(59 / 5.9005, 6))
})

test_that("Burst with no frequency does not merge", {
  a <- c(
    acc(
      acc_burst_example(1:30, 1:30),
      frequency = units::set_units(10, "Hz"),
      start = .as.POSIXct(0)
    ),
    acc(
      acc_burst_example(31, 31),
      frequency = units::set_units(NA, "Hz"),
      start = .as.POSIXct(3)
    ),
    acc(
      acc_burst_example(32:61, 32:61),
      frequency = units::set_units(10, "Hz"),
      start = .as.POSIXct(3.1)
    )
  )

  expect_identical(merge_imu(a), a)
})

test_that("Two adjacent frequency-less bursts do not merge", {
  a <- c(
    acc(
      acc_burst_example(1, 1),
      frequency = units::set_units(NA, "Hz"),
      start = .as.POSIXct(0)
    ),
    acc(
      acc_burst_example(2, 2),
      frequency = units::set_units(NA, "Hz"),
      start = .as.POSIXct(0.1)
    )
  )

  expect_identical(merge_imu(a), a)
})

test_that("A single-sample burst with a defined frequency merges normally", {
  # A one-sample burst is only special when its rate is unknown. With a defined
  # frequency it has a well-defined end and merges like any other burst.
  a <- c(
    acc(
      acc_burst_example(1:30, 1:30),
      frequency = units::set_units(10, "Hz"),
      start = .as.POSIXct(0)
    ),
    acc(
      acc_burst_example(31, 31),
      frequency = units::set_units(10, "Hz"),
      start = .as.POSIXct(3)
    )
  )

  merged <- merge_imu(a, drop = TRUE)
  expect_length(merged, 1)
  expect_equal(n_samples(merged), 31L)
  expect_equal(as.numeric(starts(merged)), 0)
  expect_equal(as.numeric(freqs(merged)), 10)
})

test_that("merge_imu preserves the input time zone when bursts merge", {
  for (tz in c("UTC", "CET", "America/New_York", "")) {
    # 60 samples at 20 Hz span 3 s, so the two bursts are contiguous and merge
    a <- acc(
      list(cbind(X = 1:60), cbind(X = 61:120)),
      frequency = units::set_units(20, "Hz"),
      start = .as.POSIXct(c(0, 3), tz)
    )

    kept <- merge_imu(a, drop = FALSE)
    dropped <- merge_imu(a, drop = TRUE)

    # Must make sure the bursts actually merge and don't hit an early return
    # branch in merge_imu()
    expect_length(dropped, 1)
    expect_identical(n_samples(dropped), 120L)

    expect_identical(attr(starts(kept), "tzone"), tz)
    expect_identical(attr(starts(dropped), "tzone"), tz)

    # The merged start is the first burst's start, unchanged
    expect_identical(starts(dropped), .as.POSIXct(0, tz))
  }
})
