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
