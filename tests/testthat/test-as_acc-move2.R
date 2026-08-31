# Test move2 inputs to the as_*() pipeline. data.frame pipeline is tested
# elsewhere. These tests test move2-specific behavior and ensure that move2
# inputs produce output consistent with data.frame inputs

skip_if_not_installed("move2")

test_that("Can resolve IMU timestamp ordering issues with move2 helpers", {
  m <- df_to_move2(
    acc_example_expanded(c(0, 0, 0, 1, 2, 1.5, 3), c(1, 2, 1, 1, 1, 2, 2))
  )

  expect_error(as_acc(m), "Not all tracks are grouped")
  expect_error(as_acc(m[order(move2::mt_track_id(m)), ]), "strictly increasing")

  m <- m[order(move2::mt_track_id(m), move2::mt_time(m)), ]

  expect_error(as_acc(m), "strictly increasing")

  m <- move2::mt_filter_unique(m, "first")

  expect_no_error(as_acc(m))
})

test_that("as_acc() rejects timestamp and track_id for move2 input", {
  alb <- albatrosses()

  expect_error(
    as_acc(alb, timestamp = move2::mt_time(alb)),
    "`timestamp` must not be supplied when"
  )
  expect_error(as_acc(alb, track_id = 1), "`track_id` must not be supplied when")
})

test_that("as_acc() rejects a partially spelled move2 metadata argument", {
  alb <- albatrosses()
  expect_error(as_acc(alb, track = 1), "`\\.\\.\\.` must be empty")
})

test_that("as_acc() dispatches on data.frame subclasses", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("sf")

  alb <- albatrosses()
  alb_df <- tibble::as_tibble(alb)

  expect_identical(
    as_acc(alb),
    as_acc(
      alb_df,
      timestamp = alb_df[[move2::mt_time_column(alb)]],
      track_id = alb_df[[move2::mt_track_id_column(alb)]]
    )
  )

  alb_sf <- sf::st_sf(
    alb_df,
    geometry = sf::st_sfc(rep(list(sf::st_point()), nrow(alb_df)))
  )

  expect_identical(
    as_acc(alb),
    as_acc(
      alb_sf,
      timestamp = alb_sf[[move2::mt_time_column(alb)]],
      track_id = alb_sf[[move2::mt_track_id_column(alb)]]
    )
  )
})

# The two entry points must agree, and the data.frame fixtures the rest of the
# suite runs on must reproduce what their move2 originals produce. Both hold if
# the move2 object and the data.frame derived from it parse identically, in the
# compact (albatrosses) and expanded (gulls) formats alike.
test_that("the data.frame fixtures match their move2 originals", {
  expect_identical(as_acc(gulls()), as_acc_df(gulls_df()))
  expect_identical(as_acc(albatrosses()), as_acc_df(albatrosses_df()))
})

test_that("as_acc() takes timestamp and track_id from move2 metadata", {
  ts <- c(0, 0.1, 0.2, 0.3)
  id <- c("a", "a", "b", "b")

  d <- acc_example_expanded(ts, id = id)
  m <- df_to_move2(d)

  # The move2 method fills both arguments from the object; the data.frame
  # method has to be handed them explicitly, and the two agree.
  expect_identical(as_acc(m, drop = TRUE), as_acc_df(d, drop = TRUE))

  # The track boundary is genuinely consumed: one burst per track, where
  # ignoring the split yields a single burst spanning all four samples.
  expect_length(as_acc(m, drop = TRUE), 2)
  expect_length(
    as_acc(d, timestamp = d$timestamp, track_id = NULL, drop = TRUE),
    1
  )
})
