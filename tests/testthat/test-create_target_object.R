test_that("minimum necessary target info is supplied", {
  expect_error(
    create_target_design(),
    'argument "time" is missing'
  )
  expect_error(
    create_target_design(time = 24),
    "Either targetmin + targetmax or midpoint must be supplied",
    fixed = TRUE
  )
  expect_error(
    create_target_design(time = 24, targetmin = 1),
    "Either targetmin + targetmax or midpoint must be supplied",
    fixed = TRUE
  )
  expect_error(
    create_target_design(time = 24, targetmax = 10),
    "Either targetmin + targetmax or midpoint must be supplied",
    fixed = TRUE
  )
  expect_error(
    create_target_design(time = 24, targetmax = 10, targetvalue = 8),
    NA
  )
})

test_that("Expected errors if target values are misspecified", {
  expect_error(
    create_target_design(time = 24, targetmin = "a", targetmax = 10),
    "targetmin or targetmax misspecified/not numeric",
    fixed = TRUE
  )
  expect_error(
    create_target_design(time = 24, targetmin = 1, targetmax = "b"),
    "targetmin or targetmax misspecified/not numeric",
    fixed = TRUE
  )
  expect_error(
    create_target_design(time = 24, targetmin = NA_real_, targetmax = "10"),
    "targetmin or targetmax misspecified/not numeric",
    fixed = TRUE
  )
  expect_error(
    create_target_design(time = 24, targetvalue = list(min = 1, max = 10)),
    "targetvalue misspecified/not numeric",
    fixed = TRUE
  )
  expect_error(
    create_target_design(time = 24, targetvalue = 20, single_point_variation = NULL),
    "single_point_variation misspecified/not numeric",
    fixed = TRUE
  )
})

test_that("Correct output with targetmin and targetmax", {
  result <- create_target_design(time = 24, targetmin = 1, targetmax = 3)
  expect_equal(result$value, 2)
  expect_equal(result$min, 1)
  expect_equal(result$max, 3)
  # Assuming default type from mipd_target_types()
  expect_equal(result$type, mipd_target_types()[1])
  expect_equal(result$scheme, data.frame(base = "dose", offset = 24, anchor = 1, anchor_by = "dose"))
})

test_that("Correct output with targetvalue", {
  result <- create_target_design(time = 24, targetvalue = 5)
  expect_equal(result$value, 5)
  expect_equal(result$min, 4)
  expect_equal(result$max, 6)
  # Assuming default type from mipd_target_types()
  expect_equal(result$type, mipd_target_types()[1])
  expect_equal(result$scheme, data.frame(base = "dose", offset = 24, anchor = 1, anchor_by = "dose"))
})
test_that("Correct output with targetvalue", {
  result <- create_target_design(time = 24, targetvalue = 5, single_point_variation = 0.5)
  expect_equal(result$value, 5)
  expect_equal(result$min, 2.5)
  expect_equal(result$max, 7.5)
  # Assuming default type from mipd_target_types()
  expect_equal(result$type, mipd_target_types()[1])
  expect_equal(result$scheme, data.frame(base = "dose", offset = 24, anchor = 1, anchor_by = "dose"))
})

test_that("Correct target type validation", {
  expect_error(create_target_design(targettype = "unknown", targetvalue = 5))
})

test_that("on-target values are correctly flagged", {
  expect_equal(
    is_on_target(c(10, 20, 30), list(min = 15, max = 25)),
    c(FALSE, TRUE, FALSE)
  )
})
