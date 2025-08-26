test_that("get_single_target_design works with single target", {
  target_design <- list(
    scheme = data.frame(time = 0),
    value = 15,
    min = 10,
    max = 20
  )
  result <- get_single_target_design(target_design)
  expect_equal(result, target_design)
})

test_that("get_single_target_design extracts correct target from time-varying design", {
  ## Target, first 5-10 and then 10-20
  target_design <- create_target_design(
    targettype = "cmin",
    targetmin = c(5, 10),
    targetmax = c(10, 20),
    at = c(8, 12), # target troughs of dose 8 and dose 12
    anchor = "dose"
  )
  
  # Test getting first target
  result1 <- get_single_target_design(target_design, idx = 1)
  expect_equal(result1$value, 7.5)
  expect_equal(result1$min, 5)
  expect_equal(result1$max, 10)
  expect_equal(nrow(result1$scheme), 1)
  
  # Test getting last target by default
  result2 <- get_single_target_design(target_design)
  expect_equal(result2$value, 15)
  expect_equal(result2$min, 10)
  expect_equal(result2$max, 20)
  expect_equal(result2$scheme, structure(list(base = "cmin", offset = 0, at = 12, anchor = "dose", 
    scatter = 0), row.names = 2L, class = "data.frame"))
})

test_that("get_single_target_design errors with invalid idx", {
  target_design <- list(
    scheme = data.frame(time = c(0, 24)),
    value = c(15, 20),
    min = c(10, 15),
    max = c(20, 25)
  )
  expect_error(
    get_single_target_design(target_design, idx = 3),
    "Not enough target values specified"
  )
})
