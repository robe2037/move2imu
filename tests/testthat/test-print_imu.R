# Burst body -------------------------------------------------------------------

test_that("format renders each burst as parenthesized column means", {
  a <- acc(
    list(
      cbind(X = c(1, 3), Y = c(2, 4), Z = c(0, 10)),
      cbind(X = c(0, 8), Y = c(1, 1), Z = c(2, 4))
    ),
    frequency = units::set_units(c(20, 20), "Hz"),
    start = .as.POSIXct(c(0, 10))
  )
  expect_identical(format(a), c("(2 3 5)", "(4 1 3)"))
})

test_that("format has one entry per burst", {
  expect_length(format(acc_example()), length(acc_example()))
})

test_that("format rounds column means to two decimals", {
  a <- acc(
    list(cbind(X = 1.234, Y = 5.678, Z = 9)),
    frequency = units::set_units(20, "Hz"),
    start = .as.POSIXct(0)
  )
  expect_identical(format(a), "(1.23 5.68 9)")
})

test_that("format renders a missing burst as NA", {
  expect_identical(format(c(acc_example(), NA))[3], NA_character_)
})

test_that("format appends the unit for bursts carrying units", {
  b1 <- cbind(X = c(1, 3), Y = c(2, 4), Z = c(9, 11))
  units(b1) <- units::as_units("m/s^2")
  a <- acc(
    list(b1),
    frequency = units::set_units(20, "Hz"),
    start = .as.POSIXct(0)
  )
  expect_identical(format(a), "(2 3 10) [m/s^2]")
})

# Frequency footer -------------------------------------------------------------

# Build an acc with one burst per supplied frequency. Frequencies may include
# NA (undetermined frequency on a present burst).
footer_acc <- function(freq) {
  acc(
    lapply(seq_along(freq), function(i) cbind(X = i, Y = i, Z = i)),
    frequency = units::set_units(freq, "Hz"),
    start = .as.POSIXct(seq(0, by = 10, length.out = length(freq)))
  )
}

freq_footer <- function(x) {
  out <- capture.output(print(x))
  line <- out[grepl("^# frequency:", out)]
  expect_length(line, 1)
  line
}

test_that("footer shows a single frequency when all bursts agree", {
  expect_match(freq_footer(footer_acc(c(20, 20))), "# frequency: 20 [Hz]", fixed = TRUE)
})

test_that("footer shows a range when frequencies differ", {
  expect_match(freq_footer(footer_acc(c(10, 20))), "# frequency: 10 [Hz] - 20 [Hz]", fixed = TRUE)
})

test_that("footer excludes NA frequencies from the range", {
  line <- freq_footer(footer_acc(c(10, NA, 20)))
  expect_match(line, "# frequency: 10 [Hz] - 20 [Hz]", fixed = TRUE)
  expect_false(grepl("NA", line))
})

test_that("footer shows NA when every frequency is NA", {
  expect_match(freq_footer(footer_acc(c(NA, NA))), "NA")
})

test_that("footer ignores fully-missing bursts", {
  a <- footer_acc(c(10, 20, 20))
  a[2] <- NA
  line <- freq_footer(a)
  expect_match(line, "# frequency: 10 [Hz] - 20 [Hz]", fixed = TRUE)
  expect_false(grepl("NA", line))
})
