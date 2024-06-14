
test_that("simple usage works, with fixed pre-specified timepoints", {
  scheme1 <- create_sampling_scheme(time = c(1, 8))
})

test_that("sampling on first dose of *day* 1 and 3, irrespective of dosing regimen", {
  scheme2 <- create_sampling_scheme(
    time = c("peak", "trough", "peak", "trough"),
    anchors = c(1, 1, 3, 3),
    anchor_by = "day"
  )
})

test_that("sampling on first dose of *day* 1 and 3, irrespective of dosing regimen", {
  scheme3 <- create_sampling_scheme(
    time = c("peak+0.5", "trough", "peak+1", "trough-0.5"),
    anchors = c(1, 1, 3, 3),
    anchor_by = "day"
  )
})

test_that("## sampling on *dose* 1 and 3, irrespective of dosing regimen", {
  scheme4 <- create_sampling_scheme(
    time = c("peak", "trough", "peak", "trough"),
    anchors = c(1, 1, 3, 3),
    anchor_by = "dose"
  )
})

