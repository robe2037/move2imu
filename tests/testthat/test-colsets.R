# Rename columns to mimic Movebank's manual (web) downloads, which use ":" and
# "-" separators (and a stripped `mag:` namespace) in place of the API's "_".
# For testing manual colset behavior below
to_alt_cols <- function(x, cols) {
  names(x)[match(cols, names(x))] <- to_alt_names(cols)
  x
}

alb_alt <- function() {
  to_alt_cols(albatrosses(), acc_colset_eobs())
}

gul_alt <- function() {
  to_alt_cols(gulls(), acc_colset_raw_xyz())
}

test_that("Can find active colsets in move2 object", {
  skip_if_not_installed("move2")
  expect_identical(active_acc_colsets(albatrosses()), list(eobs = acc_colset_eobs()))
  expect_identical(active_acc_colsets(gulls()), list(raw_xyz = acc_colset_raw_xyz()))
})

test_that("Correctly subset active colsets for expanded-format acc cols", {
  skip_if_not_installed("move2")
  gulls_data <- gulls()
  gulls_sub <- gulls_data[, setdiff(colnames(gulls_data), "acceleration_raw_y")]
  expect_identical(
    active_acc_colsets(gulls_sub),
    list(raw_xyz = new_imu_colset(
      c(X = "acceleration_raw_x", Z = "acceleration_raw_z"),
      type = "expanded"
    ))
  )
})

test_that("Can find active colsets in move2 object with multiple colsets", {
  skip_if_not_installed("move2")
  cols <- active_acc_colsets(move2::mt_stack(albatrosses(), gulls()))
  expect_identical(
    cols,
    list(eobs = acc_colset_eobs(), raw_xyz = acc_colset_raw_xyz())
  )
})

test_that("Error if no colset detected", {
  skip_if_not_installed("move2")
  alb_data <- albatrosses()

  col_subset <- setdiff(colnames(alb_data), "eobs_acceleration_axes")
  alb_data <- alb_data[, col_subset]

  expect_error(
    active_acc_colsets(alb_data),
    "Could not identify a full acc column set"
  )
})

test_that("Use data values to determine active colset if multiple present", {
  skip_if_not_installed("move2")
  m <- move2::mt_stack(gulls(), albatrosses())

  # Missing data shouldn't matter if at least one of the set still contains data
  m[["acceleration_raw_x"]] <- NA
  m[["acceleration_raw_y"]] <- NA

  colsets <- active_acc_colsets(m)
  expect_identical(
    colsets$raw_xyz,
    new_imu_colset(c(Z = "acceleration_raw_z"), type = "expanded")
  )

  # If all cols in a set are missing, then the next colset will be used
  m[["acceleration_raw_z"]] <- NA

  expect_identical(active_acc_colsets(m), list(eobs = acc_colset_eobs()))

  # Unless neither have data, in which case first is used
  m[["eobs_acceleration_axes"]] <- NA
  m[["eobs_acceleration_sampling_frequency_per_axis"]] <- NA
  m[["eobs_accelerations_raw"]] <- NA

  expect_error(active_acc_colsets(m), "Could not identify a full")
})

test_that("Correctly identify that a non-full compact-format colset is invalid", {
  skip_if_not_installed("move2")
  alb <- albatrosses()
  alb$eobs_acceleration_axes <- NA
  expect_error(active_acc_colsets(alb))

  alb$eobs_acceleration_axes <- rep(list(NULL), nrow(alb))
  expect_error(active_acc_colsets(alb))

  alb$eobs_acceleration_axes <- NULL
  expect_error(active_acc_colsets(alb))
})

test_that("Currently supported colsets", {
  expect_identical(
    movebank_acc_colsets(),
    list(
      eobs = acc_colset_eobs(),
      raw = acc_colset_raw(),
      acc = acc_colset_acc(),
      xyz = acc_colset_xyz(),
      raw_xyz = acc_colset_raw_xyz()
    )
  )
})

test_that("imu_colset() errors on invalid specifications", {
  # No columns specified
  expect_error(imu_colset(), "No IMU data columns specified")

  # Incomplete burst args
  expect_error(imu_colset(bursts = "b"), "requires")
  expect_error(imu_colset(bursts = "b", axes = "a"), "requires")

  # Mixed formats
  expect_error(
    imu_colset(x = "x", bursts = "b", axes = "a", frequency = "f"),
    "Cannot mix"
  )
})

test_that("Can get colset type from colset", {
  expect_equal(colset_type(acc_colset_eobs()), "compact")
  expect_equal(colset_type(acc_colset_raw()), "compact")
  expect_equal(colset_type(acc_colset_acc()), "compact")
  expect_equal(colset_type(acc_colset_xyz()), "expanded")
  expect_equal(colset_type(acc_colset_raw_xyz()), "expanded")
})

test_that("API column names convert to the expected alternate spellings", {
  cols <- unlist(
    c(movebank_acc_colsets(), movebank_mag_colsets(), movebank_gyro_colsets()),
    use.names = FALSE
  )

  # The expected alternate (manual-download) spellings. Based on 
  # `move2:::to_download_names()`, which is not used because it is internal
  # to move2.
  expected <- c(
    "eobs:accelerations-raw",
    "eobs:acceleration-axes",
    "eobs:acceleration-sampling-frequency-per-axis",
    "accelerations-raw",
    "acceleration-axes",
    "acceleration-sampling-frequency-per-axis",
    "accelerations",
    "acceleration-axes",
    "acceleration-sampling-frequency-per-axis",
    "acceleration-x",
    "acceleration-y",
    "acceleration-z",
    "acceleration-raw-x",
    "acceleration-raw-y",
    "acceleration-raw-z",
    "mag:magnetic-fields-raw",
    "mag:magnetic-field-axes",
    "mag:magnetic-field-sampling-frequency-per-axis",
    "mag:magnetic-field-x",
    "mag:magnetic-field-y",
    "mag:magnetic-field-z",
    "mag:magnetic-field-raw-x",
    "mag:magnetic-field-raw-y",
    "mag:magnetic-field-raw-z",
    "angular-velocities-raw",
    "gyroscope-axes",
    "gyroscope-sampling-frequency-per-axis",
    "angular-velocity-x",
    "angular-velocity-y",
    "angular-velocity-z"
  )

  expect_identical(to_alt_names(cols), expected)
})

test_that("Only genuinely distinct alternate spellings are added as alt colsets", {
  # A colset whose names have separators gets a distinct manual spelling
  expect_named(
    movebank_alt_colsets(list(raw_xyz = acc_colset_raw_xyz())),
    "raw_xyz_alt"
  )

  # A colset whose names have no separators converts to itself, so no alt is
  # added (the API colset already covers the single spelling)
  cs <- list(cs = new_imu_colset(c(X = "x", Y = "y", Z = "z"), "expanded"))
  expect_length(movebank_alt_colsets(cs), 0)
})

test_that("Active colsets match alternate column names", {
  skip_if_not_installed("move2")
  
  # Compact-format (eobs): recognized as the hidden `eobs_alt` colset
  cs <- active_acc_colsets(alb_alt())
  
  expect_named(cs, "eobs_alt")
  expect_identical(
    cs[[1]],
    imu_colset(
      bursts = "eobs:accelerations-raw",
      axes = "eobs:acceleration-axes",
      frequency = "eobs:acceleration-sampling-frequency-per-axis"
    )
  )

  # Expanded-format: recognized as the hidden `raw_xyz_alt` colset
  cs <- active_acc_colsets(gul_alt())

  expect_named(cs, "raw_xyz_alt")
  expect_identical(
    cs[[1]],
    imu_colset(
      x = "acceleration-raw-x",
      y = "acceleration-raw-y",
      z = "acceleration-raw-z"
    )
  )
})

test_that("as_acc() parses alternate names identically to canonical", {
  skip_if_not_installed("move2")
  
  expect_identical(as_acc(alb_alt()), as_acc(albatrosses()))
  expect_identical(as_acc(gul_alt()), as_acc(gulls()))
})

# Test mag as insurance since mag columns behave slightly differently with
# their `mag:` prefix.
test_that("Alternate names are detected and parsed for mag", {
  skip_if_not_installed("move2")

  # Compact-format
  mc <- to_alt_cols(mag_example_compact(), mag_colset_raw())
  expect_named(active_mag_colsets(mc), "raw_alt")
  expect_identical(as_mag(mc), as_mag(mag_example_compact()))

  # Expanded-format (`mag:magnetic-field-x`, etc.)
  me <- to_alt_cols(mag_example_expanded(), mag_colset_xyz())
  expect_named(active_mag_colsets(me), "xyz_alt")
  expect_identical(as_mag(me), as_mag(mag_example_expanded()))
})

test_that("API and alternate spellings are detected as separate colsets", {
  skip_if_not_installed("move2")
  
  g <- gulls()
  
  cols <- c("acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z")
  
  for (col in cols) {
    g[[to_alt_names(col)]] <- g[[col]]
  }
  
  expect_named(active_acc_colsets(g), c("raw_xyz", "raw_xyz_alt"))
})

test_that("Overlapping data in API and alt col names reported as duplicated rows", {
  skip_if_not_installed("move2")
  
  g <- gulls()
  
  cols <- c("acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z")
  
  for (col in cols) {
    g[[to_alt_names(col)]] <- g[[col]]
  }
  
  expect_true(any(duplicated_acc_rows(g)))
  expect_error(suppressWarnings(as_acc(g)), "multiple sources")
})

test_that("Explicit colsets bypass detection, in either spelling", {
  skip_if_not_installed("move2")
  
  g <- gulls()
  
  cols <- c("acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z")
  
  for (col in cols) {
    g[[to_alt_names(col)]] <- g[[col]]
  }
  
  # Auto-detection would be an overlapping conflict, but naming the columns
  # explicitly works with no detection and no ambiguity, in either spelling.
  alt_acc <- as_acc(
    g, 
    colset = imu_colset(
      x = "acceleration-raw-x",
      y = "acceleration-raw-y",
      z = "acceleration-raw-z"
    )
  )
  
  expect_identical(alt_acc, as_acc(gulls()))
})

test_that("Colset detection accepts a plain data.frame", {
  # No `move2` involved: both helpers are documented for `data.frame` too
  df <- data.frame(
    acceleration_raw_x = c(1, NA, 1),
    acceleration_raw_y = c(2, NA, 2),
    acceleration_raw_z = c(3, NA, 3),
    eobs_accelerations_raw = c(NA, "1 2 3", "1 2 3"),
    eobs_acceleration_axes = c(NA, "XYZ", "XYZ"),
    eobs_acceleration_sampling_frequency_per_axis = c(NA, 10, 10)
  )

  expect_named(active_acc_colsets(df), c("eobs", "raw_xyz"))
  expect_identical(duplicated_acc_rows(df), c(FALSE, FALSE, TRUE))
})
