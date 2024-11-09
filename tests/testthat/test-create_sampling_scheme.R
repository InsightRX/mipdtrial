regimen <- PKPDsim::new_regimen(amt = 1, n = 10, interval = 12)

test_that("simple usage works, with fixed pre-specified timepoints", {
  sampling_design1 <- create_sampling_design(time = c(1, 8, 25))
  tdm1 <- get_sampling_times_from_scheme(sampling_design1$scheme, regimen)
  expect_equal(
    sampling_design1$scheme,
    data.frame(
      base = c("dose", "dose", "dose"), offset = c(1, 8, 25),
      at = c(1, 1, 1), anchor = c("dose", "dose", "dose")
    )
  )
  expect_equal(tdm1, c(1, 8, 25))
})

test_that("error when both time and offset specified", {
  expect_error(
    create_sampling_design(
      time = c(0, 0, 0, 0),
      offset = c(1, 2, 3, 4)
    )
  )
})

test_that("sampling on first dose of *day* 1 and 3, irrespective of dosing regimen", {
  sampling_design2 <- create_sampling_design(
    when = c("peak", "trough", "peak", "trough"),
    at = c(1, 1, 3, 3),
    anchor = "day"
  )
  tdm2 <- get_sampling_times_from_scheme(sampling_design2$scheme, regimen)
  expect_equal(
    sampling_design2$scheme,
    data.frame(
      base = c("peak", "trough", "peak", "trough"),
      offset = c(0, 0, 0, 0),
      at = c(1, 1, 3, 3),
      anchor = c("day", "day", "day", "day"),
      scatter = c(0, 0, 0, 0)
    )
  )
  expect_equal(tdm2, c(0, 12, 48, 60))
})

test_that("sampling on first dose of *day* 1 and 3, irrespective of dosing regimen", {
  sampling_design3 <- create_sampling_design(
    offset = c(0.5, 0, 1, -0.5),
    when = c("peak", "trough", "peak", "trough"),
    at = c(1, 1, 3, 3),
    anchor = "day"
  )
  tdm3 <- get_sampling_times_from_scheme(sampling_design3$scheme, regimen)
  expect_equal(
    sampling_design3$scheme,
    data.frame(
      base = c("peak", "trough", "peak", "trough"),
      offset = c(0.5, 0, 1.0, -0.5),
      at = c(1, 1, 3, 3),
      anchor = c("day", "day", "day", "day"),
      scatter = c(0, 0, 0, 0)
    )
  )
  expect_equal(tdm3, c(0.5, 12, 49, 59.5))
})

test_that("sampling on *dose* 1 and 3, irrespective of dosing regimen", {
  sampling_design4 <- create_sampling_design(
    offset = c(0.5, 0, 1, -0.5),
    when = c("peak", "trough", "peak", "trough"),
    at = c(1, 1, 3, 3),
    anchor = "dose"
  )
  tdm4 <- get_sampling_times_from_scheme(sampling_design4$scheme, regimen)
  expect_equal(
    sampling_design4$scheme,
    data.frame(
      base = c("peak", "trough", "peak", "trough"),
      offset = c(0.5, 0, 1.0, -0.5),
      at = c(1, 1, 3, 3),
      anchor = c("dose", "dose", "dose", "dose"),
      scatter = c(0, 0, 0, 0)
    )
  )
  expect_equal(tdm4, c(0.5, 12, 25, 35.5))
})

test_that("can use 'middle', 'cmid' and 'random' timepoints", {
  set.seed(12345)
  sampling_design5 <- create_sampling_design(
    when = c("peak", "middle", "random", "trough"),
    at = c(1, 1, 1, 1),
    anchor = "dose"
  )
  tdm5 <- get_sampling_times_from_scheme(sampling_design5$scheme, regimen)
  expect_equal(
    sampling_design5$scheme,
    data.frame(
      base = c("peak", "middle", "random", "trough"),
      offset = c(0, 0, 0, 0),
      at = c(1, 1, 1, 1),
      anchor = c("dose", "dose", "dose", "dose"),
      scatter = rep(0, 4)
    )
  )
  expect_equal(tdm5, c(0, 6, 8.65, 12.0))
})

test_that("'scatter' options works", {
  sampling_design6 <- create_sampling_design(
    when = c("peak", "middle", "random", "trough"),
    at = c(1, 1, 1, 1),
    anchor = "dose",
    scatter = 0.2
  )
  set.seed(123)
  tdm6 <- get_sampling_times_from_scheme(sampling_design6$scheme, regimen)
  expect_equal(
    sampling_design6$scheme,
    data.frame(
      base = c("peak", "middle", "random", "trough"),
      offset = c(0, 0, 0, 0),
      at = c(1, 1, 1, 1),
      anchor = c("dose", "dose", "dose", "dose"),
      scatter = rep(0.2, 4)
    )
  )
  expect_equal(tdm6, c(-0.11, 5.95, 10.95, 12.25))
})
