test_that("minimum necessary target info is supplied", {
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
  expect_equal(result$scheme, data.frame(base = "dose", offset = 24, at = 1, anchor = "dose"))
})

test_that("Correct output with targetvalue", {
  result <- create_target_design(time = 24, targetvalue = 5)
  expect_equal(result$value, 5)
  expect_equal(result$min, 4)
  expect_equal(result$max, 6)
  # Assuming default type from mipd_target_types()
  expect_equal(result$type, mipd_target_types()[1])
  expect_equal(result$scheme, data.frame(base = "dose", offset = 24, at= 1, anchor = "dose"))
})
test_that("Correct output with targetvalue", {
  result <- create_target_design(time = 24, targetvalue = 5, single_point_variation = 0.5)
  expect_equal(result$value, 5)
  expect_equal(result$min, 2.5)
  expect_equal(result$max, 7.5)
  # Assuming default type from mipd_target_types()
  expect_equal(result$type, mipd_target_types()[1])
  expect_equal(result$scheme, data.frame(base = "dose", offset = 24, at = 1, anchor = "dose"))
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

test_that("Simpler interface using less arguments correctly infers arguments", {
  expect_equal(
    create_target_design(
      targettype = "trough",
      targetvalue = 10,
      time = 72
    ),
    list(
      type = "trough", value = 10, min = 8, max = 12,
      scheme = data.frame(
        base = "dose", offset = 72, at = 1, anchor = "dose"
      )
    )
  )
  expect_equal(
    create_target_design( # trough at day 6
      targettype = "trough",
      targetvalue = 10,
      at = 6,
      anchor = "day"
    ),
    list(
      type = "trough", value = 10, min = 8, max = 12,
      scheme = data.frame(
        base = "cmin", offset = 0, at = 6, anchor = "day", scatter = 0
      )
    )
  )
  expect_equal(
    create_target_design( # peak at dose 6
      targettype = "peak",
      targetvalue = 10,
      at = 6,
      anchor = "dose"
    ),
    list(
      type = "peak", value = 10, min = 8, max = 12,
      scheme = data.frame(
        base = "cmax", offset = 0, at = 6, anchor = "dose", scatter = 0
      )
    )
  )
})

test_that("target design works for absolute time", {
  expect_equal(
    create_target_design(
      time = 192,
      targettype = "cum_auc",
      targetvalue = 90000
    ),
    list(type = "cum_auc", value = 90000, min = 72000, max = 108000,
         scheme = data.frame(base = "dose", offset = 192, at = 1, anchor = "dose"))
  )
})

test_that("create_eval_design: handles multiple metrics", {
  eval_design <- create_eval_design(
    evaltype = c("conc"),
    at = c(14, 28, 42),
    anchor = "day"
  )
  eval_design2 <- create_eval_design(
    evaltype = c("conc", "auc24"),
    at = c(14, 28, 42),
    anchor = "day"
  )
  eval_design3 <- create_eval_design(
    evaltype = c("cmin", "cmax"),
    at = c(14, 28, 42),
    anchor = "day"
  )

  eval1 <- list(
    conc = data.frame(
      base = rep("dose", 3L),
      offset = numeric(3),
      at = c(14, 28, 42),
      anchor = rep("day", 3L),
      scatter = numeric(3)
    )
  )

  eval2 <- list(
    conc = data.frame(
      base = rep("dose", 3L),
      offset = numeric(3),
      at = c(14, 28, 42),
      anchor = rep("day", 3L),
      scatter = numeric(3)
    ),
    auc24 = data.frame(
      base = rep("dose", 3L),
      offset = rep(24, 3L),
      at = c(14, 28, 42),
      anchor = rep("day", 3L),
      scatter = numeric(3)
    )
  )

  eval3 <- list(
    cmin = data.frame(
      base = rep("cmin", 3L),
      offset = numeric(3),
      at = c(14, 28, 42),
      anchor = rep("day", 3L),
      scatter = numeric(3)
    ),
    cmax = data.frame(
      base = rep("cmax", 3L),
      offset = numeric(3),
      at = c(14, 28, 42),
      anchor = rep("day", 3L),
      scatter = numeric(3)
    )
  )

  expect_equal(eval_design, eval1)
  expect_equal(eval_design2, eval2)
  expect_equal(eval_design3, eval3)
})

