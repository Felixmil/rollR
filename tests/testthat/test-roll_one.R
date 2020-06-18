test_that("simple roll works", {
  set.seed(42)
  roll1 = roll_one("1d6")
  roll2 = roll_one("1d6")

  expect_true(roll1 == 1)
  expect_true(roll2 == 5)
})

test_that("keep highest works",{
  set.seed(42)
  roll = roll_one("10d10h2")
  expect_equal(roll, 20)
  })

test_that("keep lowest works",{
  set.seed(42)
  roll = roll_one("10d10l2")
  expect_equal(roll, 2)
})



test_that("warning works",{
  set.seed(42)
  expect_warning(roll_one("wrong_syntax"))
})

