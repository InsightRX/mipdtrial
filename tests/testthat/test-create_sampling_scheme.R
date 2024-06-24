regimen <- PKPDsim::new_regimen(amt = 1, n = 10, interval = 12)

test_that("simple usage works, with fixed pre-specified timepoints", {
  scheme1 <- create_sampling_design(time = c(1, 8, 25))
  tdm1 <- get_sampling_times_from_scheme(scheme1, regimen)
  expect_equal(
    scheme1,
    data.frame(
      base = c("dose", "dose", "dose"), offset = c(1, 8, 25),
      anchor = c(1, 1, 1), anchor_by = c("dose", "dose", "dose")
    )
  )
  expect_equal(tdm1, c(1, 8, 25))
})

test_that("sampling on first dose of *day* 1 and 3, irrespective of dosing regimen", {
  scheme2 <- create_sampling_design(
    time = c(0, 0, 0, 0),
    offset_from = c("peak", "trough", "peak", "trough"),
    anchor = c(1, 1, 3, 3),
    anchor_by = "day"
  )
  tdm2 <- get_sampling_times_from_scheme(scheme2, regimen)
  expect_equal(
    scheme2,
    data.frame(
      base = c("peak", "trough", "peak", "trough"), offset = c(0, 0, 0, 0),
      anchor = c(1, 1, 3, 3), anchor_by = c("day", "day", "day", "day")
    )
  )
  expect_equal(tdm2, c(0, 12, 48, 60))
})

test_that("sampling on first dose of *day* 1 and 3, irrespective of dosing regimen", {
  scheme3 <- create_sampling_design(
    time = c(0.5, 0, 1, -0.5),
    offset_from = c("peak", "trough", "peak", "trough"),
    anchor = c(1, 1, 3, 3),
    anchor_by = "day"
  )
  tdm3 <- get_sampling_times_from_scheme(scheme3, regimen)
  expect_equal(
    scheme3,
    data.frame(
      base = c("peak", "trough", "peak", "trough"), offset = c(0.5, 0, 1.0, -0.5),
      anchor = c(1, 1, 3, 3), anchor_by = c("day", "day", "day", "day")
    )
  )
  expect_equal(tdm3, c(0.5, 12, 49, 59.5))
})

test_that("## sampling on *dose* 1 and 3, irrespective of dosing regimen", {
  scheme4 <- create_sampling_design(
    time = c(0.5, 0, 1, -0.5),
    offset_from = c("peak", "trough", "peak", "trough"),
    anchor = c(1, 1, 3, 3),
    anchor_by = "dose"
  )
  tdm4 <- get_sampling_times_from_scheme(scheme4, regimen)
  expect_equal(
    scheme4,
    data.frame(
      base = c("peak", "trough", "peak", "trough"), offset = c(0.5, 0, 1.0, -0.5),
      anchor = c(1, 1, 3, 3), anchor_by = c("dose", "dose", "dose", "dose")
    )
  )
  expect_equal(tdm4, c(0.5, 12, 25, 35.5))
})
