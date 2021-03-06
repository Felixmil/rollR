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

test_that("exploding dice works", {
  set.seed(42)
  roll = roll_one("5d10!")
  expect_equal(roll,30)
})

test_that("reroll dice works",{
  set.seed(42)
  roll = roll_one("10d6r1")
  expect_equal(roll,36)
})


test_that("sucess dice works",{
  set.seed(42)
  roll = roll_one("6d6>4")
  expect_equal(roll,1)
  roll = roll_one("6d6>=4")
  expect_equal(roll, 2)
  roll = roll_one("6d6<3")
  expect_equal(roll, 3)
  roll = roll_one("6d6<=2")
  expect_equal(roll, 1)
  roll = roll_one("6d6=4")
  expect_equal(roll, 2)
})

test_that("warning works",{
  set.seed(42)
  expect_warning(roll_one("wrong_syntax"))
})

