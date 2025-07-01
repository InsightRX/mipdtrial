is_PKPDsim_cov <- function(covs) {
  all(
    vapply(
      covs,
      function(x) inherits(x, "covariate"),
      FUN.VALUE = logical(1)
    )
  )
}

test_that("handle missing columns", {
  dat <- data.frame(ID = 1, weight = 70, CRCL = 4)
  mapping <- c(WT = "weight", MISSING = "missing_column")
  expect_warning(
    out <- create_cov_object(dat, mapping),
    "Missing covariates: missing_column"
  )
  expect_null(out)
})

test_that("handle NA values in columns", {
  dat <- data.frame(ID = 1, weight = NA, CRCL = 4)
  mapping <- c(WT = "weight", CRCL = "CRCL")
  expect_warning(
    out <- create_cov_object(dat, mapping),
    "Missing covariates: weight"
  )
  expect_null(out)
})

test_that("handle combined NA and missing columns", {
  dat <- data.frame(ID = 1, a = NA, b = 4, c = 1)
  mapping <- c(A = "a", B = "b", C = "c", D = "d")
  expect_warning(
    out <- create_cov_object(dat, mapping),
    "Missing covariates: d, a"
  )
  expect_null(out)
})

test_that("handle missing time column", {
  dat <- data.frame(ID = 1, weight = c(70, 90), CRCL = c(4, 6))
  mapping <- c(WT = "weight", CRCL = "CRCL")
  expect_warning(
    out <- create_cov_object(dat, mapping, time_column = "time"),
    "Missing time column: time"
  )
  expect_null(out)
})

test_that("When no time column supplied, take first row", {
  dat <- data.frame(ID = 1, weight = c(70, 90), CRCL = c(4, 6))
  mapping <- c(WT = "weight", CRCL = "CRCL")
  result <- create_cov_object(dat, mapping)
  expect_true(is_PKPDsim_cov(result))
  expect_equal(
    vapply(result, `[[`, "times", FUN.VALUE = numeric(1), USE.NAMES = FALSE),
    c(0, 0)
  )
  expect_equal(
    vapply(result, `[[`, "value", FUN.VALUE = numeric(1), USE.NAMES = FALSE),
    c(70, 4) # first row
  )
})

# Test if function handles specified time column correctly
test_that("handle specified time column correctly", {
  dat <- data.frame(ID = 1, weight = c(70, 90), CRCL = c(4, 6), time = c(0,5))
  mapping <- c(WT = "weight", CRCL = "CRCL")
  result <- create_cov_object(dat, mapping, time_column = "time")
  expect_true(is_PKPDsim_cov(result))
  expect_equal(
    vapply(result, `[[`, "times", FUN.VALUE = numeric(2), USE.NAMES = FALSE),
    matrix(c(0, 5, 0, 5), nrow = 2)
  )
  expect_equal(
    vapply(result, `[[`, "value", FUN.VALUE = numeric(2), USE.NAMES = FALSE),
    matrix(c(70, 90, 4, 6), nrow = 2)
  )
})

# Test function implementation defaulting mechanism
test_that("default implementation is correct", {
  dat <- data.frame(ID = 1, weight = 70, CRCL = 4)
  mapping <- c(WT = "weight", CRCL = "CRCL")
  result <- create_cov_object(dat, mapping)
  expect_true(is_PKPDsim_cov(result))
  expect_equal(
    vapply(result, `[[`, "implementation", FUN.VALUE = "", USE.NAMES = FALSE),
    rep("interpolate", 2)
  )
})
