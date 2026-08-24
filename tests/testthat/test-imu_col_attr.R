# Build a small move2 carrying one column of each sensor type.
move2_with_imu <- function() {
  skip_if_not_installed("move2")

  df <- data.frame(
    t = as.POSIXct("2020-01-01", tz = "UTC") + 1:2,
    id = "a",
    lon = 1:2,
    lat = 1:2
  )
  
  m <- move2::mt_as_move2(
    df,
    time_column = "t",
    track_id_column = "id",
    coords = c("lon", "lat")
  )
  
  m$a <- acc_example()
  
  m$m <- mag(
    list(cbind(X = 1:3, Y = 1:3, Z = 1:3), cbind(X = 4:6, Y = 4:6, Z = 4:6)),
    units::set_units(20, "Hz")
  )
  
  m$g <- gyro(
    list(cbind(X = 1:3, Y = 1:3), cbind(X = 4:6, Y = 4:6)),
    units::set_units(20, "Hz")
  )
  
  m
}

test_that("setter records the column and getter returns it", {
  df <- move2_with_imu()
  
  df <- mt_set_acc_column(df, "a")
  df <- mt_set_mag_column(df, "m")
  df <- mt_set_gyro_column(df, "g")
  
  expect_identical(attr(df, "acc_column"), "a")
  expect_identical(attr(df, "mag_column"), "m")
  expect_identical(attr(df, "gyro_column"), "g")
  expect_identical(mt_acc_column(df), "a")
  expect_identical(mt_mag_column(df), "m")
  expect_identical(mt_gyro_column(df), "g")
})

test_that("Value accessor extracts data from correct column", {
  df <- move2_with_imu()
  
  df <- mt_set_acc_column(df, "a")
  df <- mt_set_mag_column(df, "m")
  df <- mt_set_gyro_column(df, "g")
  
  a <- mt_acc(df)
  m <- mt_mag(df)
  g <- mt_gyro(df)
  
  expect_true(is_acc(a))
  expect_identical(a, df$a)
  
  expect_true(is_mag(m))
  expect_identical(m, df$m)
  
  expect_true(is_gyro(g))
  expect_identical(g, df$g)
})

test_that("setter rejects a column of the wrong sensor type", {
  m <- move2_with_imu()

  expect_error(mt_set_mag_column(m, "a"), "must be of class <mag>")
  expect_error(mt_set_acc_column(m, "m"), "must be of class <acc>")
})

test_that("setter rejects a non-string or non-existent column", {
  m <- move2_with_imu()

  expect_error(mt_set_acc_column(m, 1), "length-1 character")
  expect_error(mt_set_acc_column(m, c("a", "m")), "length-1 character")
  expect_error(mt_set_acc_column(m, "foobar"), "name of a column")
})

test_that("getter errors when no attribute is set", {
  m <- move2_with_imu()

  expect_error(mt_acc_column(m), "No acc_column detected.+mt_set_acc_column")
  expect_error(mt_gyro_column(m), "No gyro_column detected.+mt_set_gyro_column")
})

test_that("Error when designated column has inconsistencies", {
  m <- mt_set_acc_column(move2_with_imu(), "a")
  
  m$a <- NULL
  expect_error(mt_acc(m), "does not exist")
  
  m$a <- m$m
  expect_error(mt_acc(m), "must be of class <acc>, not <mag>")
})

test_that("accessors reject non-move2 input", {
  df <- data.frame(a = 1:2)
  df$a <- acc_example()

  expect_error(mt_set_acc_column(df, "a"), "must be a <move2> object")
  expect_error(mt_acc_column(df), "must be a <move2> object")
  expect_error(mt_acc(df), "must be a <move2> object")
})
