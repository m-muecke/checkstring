test_that("assert_empty_dots works", {
  expect_invisible(assert_empty_dots())
  expect_null(assert_empty_dots())
  expect_error(
    assert_empty_dots(1L, 2L, 3L),
    "Received 3 unused unnamed argument(s).",
    fixed = TRUE
  )
  expect_error(
    assert_empty_dots(a = 1L, b = 2L),
    "Received unused named argument(s): a, b.",
    fixed = TRUE
  )
  expect_error(
    assert_empty_dots(1L, 2L, a = 3L, b = 4L),
    "Received 2 unused unnamed argument(s) and unused named argument(s): a, b.",
    fixed = TRUE
  )
})
