expected_vedba <- function(b) {
  if (inherits(b, "units")) b <- units::drop_units(b)
  b <- t(b) - colMeans(b)
  mean(sqrt(colSums(b^2)))
}

expected_odba <- function(b) {
  if (inherits(b, "units")) b <- units::drop_units(b)
  b <- t(b) - colMeans(b)
  mean(colSums(abs(b)))
}

test_that("Can calcluate vedba (with units)", {
  a <- acc_example()
  bursts(a) <- lapply(bursts(a), units::set_units, "m/s^2")

  result <- vedba(a)

  expect_s3_class(result[[1]], "units")
  expect_equal(units(result[[1]]), units(bursts(a)[[1]]))
  expect_equal(as.numeric(result[[1]]), expected_vedba(bursts(a)[[1]]))
  expect_equal(as.numeric(result[[2]]), expected_vedba(bursts(a)[[2]]))
})

test_that("Can calculate odba (with units)", {
  a <- acc_example()
  bursts(a) <- lapply(bursts(a), units::set_units, "m/s^2")

  result <- odba(a)

  expect_s3_class(result[[1]], "units")
  expect_equal(units(result[[1]]), units(bursts(a)[[1]]))
  expect_equal(as.numeric(result[[1]]), expected_odba(bursts(a)[[1]]))
  expect_equal(as.numeric(result[[2]]), expected_odba(bursts(a)[[2]]))
})

test_that("Can calculate vedba (without units)", {
  a <- acc_example()
  result <- vedba(a)

  expect_length(result, length(a))
  expect_true(all(result >= 0))

  expect_equal(result[[1]], expected_vedba(bursts(a)[[1]]))
  expect_equal(result[[2]], expected_vedba(bursts(a)[[2]]))
})

test_that("Can calculate odba (without units)", {
  a <- acc_example()
  result <- odba(a)

  expect_length(result, length(a))
  expect_true(all(result >= 0))

  expect_equal(result[[1]], expected_odba(bursts(a)[[1]]))
  expect_equal(result[[2]], expected_odba(bursts(a)[[2]]))
})

test_that("Single-sample burst returns zero", {
  a <- acc(
    acc_burst_example(1, 2, 3),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(1)
  )

  # A single sample has colMeans equal to itself, so centered values are all 0
  expect_equal(vedba(a)[[1]], 0)
  expect_equal(odba(a)[[1]], 0)
})

test_that("Return NULL dba on empty acc", {
  expect_null(vedba(acc()))
  expect_null(odba(acc()))
})

test_that("vedba and odba return NA for NA elements (unitless)", {
  a <- acc_example()
  a_na <- c(a[1], acc(list(NULL), units::set_units(NA, "Hz")), a[2])

  v <- vedba(a_na)
  o <- odba(a_na)

  expect_length(v, 3)
  expect_length(o, 3)
  expect_true(is.na(v[2]))
  expect_true(is.na(o[2]))
  expect_false(is.na(v[1]))
  expect_false(is.na(o[1]))
})

test_that("vedba and odba return units NA for NA elements (with units)", {
  a <- set_imu_units(acc_example(), "m/s^2")
  a_na <- c(acc(list(NULL), units::set_units(NA, "Hz")), a)

  v <- vedba(a_na)
  o <- odba(a_na)

  expect_length(v, 3)
  expect_true(is.na(v[1]))
  expect_true(is.na(o[1]))
  expect_s3_class(v, "units")
  expect_s3_class(o, "units")
})

test_that("dba uses first available units for output", {
  a1 <- set_imu_units(acc_example()[1], "m/s^2")
  a2 <- set_imu_units(acc_example()[2], "standard_free_fall")

  v1 <- vedba(c(a1, a2))
  o1 <- odba(c(a1, a2))

  expect_s3_class(v1, "units")
  expect_s3_class(o1, "units")
  expect_equal(units(v1)$numerator, "m")
  expect_equal(units(v1)$denominator, c("s", "s"))
  expect_equal(units(o1)$numerator, "m")
  expect_equal(units(o1)$denominator, c("s", "s"))

  v2 <- vedba(c(a2, a1))
  o2 <- odba(c(a2, a1))

  expect_true(inherits(v2, "units"))
  expect_true(inherits(o2, "units"))
  expect_equal(units(v2)$numerator, "standard_free_fall")
  expect_equal(units(o2)$numerator, "standard_free_fall")

  expect_equal(as.numeric(rev(v2) * 9.80665), as.numeric(v1))
  expect_equal(as.numeric(rev(o2) * 9.80665), as.numeric(o1))
})

test_that("vedba and odba error on mixed unitless and units bursts", {
  a <- c(acc_example()[1], set_imu_units(acc_example()[2], "m/s^2"))

  expect_error(vedba(a), "Can't combine")
  expect_error(odba(a), "Can't combine")
})
