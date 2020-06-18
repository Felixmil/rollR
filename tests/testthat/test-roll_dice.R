
test_that("roll dice with addition works", {
  set.seed(42)
  roll = roll_dice("1d6+1")
  expect_true(roll == 2)
})

test_that("roll with substraction", {
  set.seed(42)
  roll = roll_dice("1d100-20")
  expect_true(roll == 29)
})

test_that("addition of rolls works", {
  set.seed(42)
  roll = roll_dice("1d4+1d6+1d8+1d10")
  expect_true(roll == 16)
})

test_that("substraction of rolls works", {
  set.seed(42)
  roll = roll_dice("1d100-1d20")
  expect_true(roll == 44)
})

test_that("print history works",{
  set.seed(42)
  expect_message(roll_dice("1d100",roll_history = TRUE))
})



