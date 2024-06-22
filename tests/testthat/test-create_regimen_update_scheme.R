regimen <- PKPDsim::new_regimen(amt = 1, n = 10, interval = 12)

test_that("simple usage works, anchoring to doses", {
  scheme1 <- create_regimen_update_scheme(
    time = c(0, 0, 0),
    anchor = c(1, 2, 3),
    anchor_by = "dose"
  )
  times <- get_sampling_times_from_scheme(scheme1, regimen)
  expect_equal(
    scheme1,
    data.frame(
      base = c("dose", "dose", "dose"), offset = c(0, 0, 0),
      anchor = c(1, 2, 3), anchor_by = c("dose", "dose", "dose" ),
      update_type = c("dose", "dose", "dose")
    )
  )
  expect_equal(times, c(0, 12, 24))
})

test_that("simple usage works, anchoring to days, different `time` values", {
  scheme2 <- create_regimen_update_scheme(
    time = c(0, 12, 0),
    anchor = c(1, 2, 5),
    anchor_by = "day"
  )
  times <- get_sampling_times_from_scheme(scheme2, regimen)
  expect_equal(
    scheme1,
    data.frame(
      base = c("dose", "dose", "dose"), offset = c(0, 12, 0),
      anchor = c(1, 2, 5), anchor_by = c("day", "day", "day"),
      update_type = c("dose", "dose", "dose")
    )
  )
  expect_equal(times, c(0, 36, 96))
})
