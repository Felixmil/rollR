set.seed(42)

test_that("simple roll works", {
  roll1 = roll_one("1d6")
  roll2 = roll_one("1d6")

  expect_true(roll1 == 1)
  expect_true(roll2 == 5)
})
