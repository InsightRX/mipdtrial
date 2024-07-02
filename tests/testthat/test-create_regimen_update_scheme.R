regimen <- PKPDsim::new_regimen(amt = 1, n = 10, interval = 12)

test_that("simple usage works, anchoring to doses", {
  design1 <- create_regimen_update_design(
    at = c(1, 2, 3),
    anchor = "dose"
  )
  times <- get_sampling_times_from_scheme(design1$scheme, regimen)
  expect_equal(
    design1$scheme,
    data.frame(
      base = c("dose", "dose", "dose"), offset = c(0, 0, 0),
      at = c(1, 2, 3), anchor = c("dose", "dose", "dose" ),
      update_type = c("dose", "dose", "dose")
    )
  )
  expect_equal(times, c(0, 12, 24))
})

test_that("simple usage works, anchoring to days, different `time` values", {
  design2 <- create_regimen_update_design(
    at = c(1, 2, 5),
    anchor = "day"
  )
  times <- get_sampling_times_from_scheme(design2$scheme, regimen)
  expect_equal(
    design2$scheme,
    data.frame(
      base = c("dose", "dose", "dose"),
      offset = c(0, 0, 0),
      at = c(1, 2, 5),
      anchor = c("day", "day", "day"),
      update_type = c("dose", "dose", "dose")
    )
  )
  expect_equal(times, c(0, 24, 96))
})
