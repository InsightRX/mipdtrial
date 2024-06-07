test_that("round_to_multiple function works correctly", {
  # Test with multiple = NULL
  expect_equal(round_to_multiple(5, NULL), 5)
  expect_equal(round_to_multiple(10, NULL), 10)

  # Test with multiple > 1
  expect_equal(round_to_multiple(5, 3), 6)
  expect_equal(round_to_multiple(10, 3), 9)
  expect_equal(round_to_multiple(15, 7), 14)
  expect_equal(round_to_multiple(23.134, 0.1), 23.1)
})


test_that("filters down to two 0/100 rows", {
  dat <- data.frame(
    dose = c(50, 1240, 4810, 6000),
    y = c(0, 0, 100, 100)
  )
  res <- filter_rows_0_100(dat)
  expect_equal(res$y, c(0, 100))
  expect_equal(res$dose, c(1240, 4810))
})

test_that("keeps all non-0/100 rows if 2 or more", {
  dat <- data.frame(
    dose = c(50, 1240, 2430, 3620, 4810, 6000),
    y = c(0, 0, 56, 93, 100, 100)
  )
  res <- filter_rows_0_100(dat)
  expect_equal(res$y, c(56, 93))
  expect_equal(res$dose, c(2430, 3620))
})

test_that("if 1 non-0/100 row, keeps that plus largest 0 and smallest 100", {
  dat <- data.frame(
    dose = c(50, 1240, 3620, 4810, 6000),
    y = c(0, 0, 93, 100, 100)
  )
  res <- filter_rows_0_100(dat)
  expect_equal(res$y, c(0, 93, 100))
  expect_equal(res$dose, c(1240, 3620, 4810))
})

test_that("works if only 1 row has 0/100", {
  dat <- data.frame(
    dose = c(1240, 3620, 4810),
    y = c(0, 93, 100)
  )
  res <- filter_rows_0_100(dat)
  expect_equal(res$y, c(0, 93, 100))
  expect_equal(res$dose, c(1240, 3620, 4810))
})

test_that("works if you have 0 and not 100 or vice versa", {
  dat1 <- data.frame(
    dose = c(50, 1240, 3620),
    y = c(0, 0, 93)
  )
  res1 <- filter_rows_0_100(dat1)

  dat2 <- data.frame(
    dose = c(3620, 4810, 6000),
    y = c(93, 100, 100)
  )
  res2 <- filter_rows_0_100(dat2)

  expect_equal(res1$y, c(0, 93))
  expect_equal(res1$dose, c(1240, 3620))
  expect_equal(res2$y, c(93, 100))
  expect_equal(res2$dose, c(3620, 4810))
})

test_that("is_single_valid_number returns TRUE for valid numbers", {
  expect_true(is_single_valid_number(1))
  expect_true(is_single_valid_number(-1))
  expect_true(is_single_valid_number(0))
  expect_true(is_single_valid_number(1.5))
})

test_that("is_single_valid_number returns FALSE for non-numeric values", {
  expect_false(is_single_valid_number("a"))
  expect_false(is_single_valid_number(TRUE))
  expect_false(is_single_valid_number(NULL))
  expect_false(is_single_valid_number(list(1)))
})

test_that("is_single_valid_number returns FALSE if length not equal to 1", {
  expect_false(is_single_valid_number(c(1, 2)))
  expect_false(is_single_valid_number(c()))
})

test_that("is_single_valid_number returns FALSE for NA, Inf, and NaN values", {
  expect_false(is_single_valid_number(NA))
  expect_false(is_single_valid_number(Inf))
  expect_false(is_single_valid_number(-Inf))
  expect_false(is_single_valid_number(NaN))
})

test_that("is_single_valid_number returns FALSE for non-atomic types", {
  expect_false(is_single_valid_number(data.frame(a = 1)))
})
