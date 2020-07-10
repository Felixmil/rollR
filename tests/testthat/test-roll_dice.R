
test_that("roll dice with addition works", {
  set.seed(42)
  roll = roll_dice("1d6+1")
  expect_true(as.numeric(roll) == 2)
})

test_that("roll with substraction", {
  set.seed(42)
  roll = roll_dice("1d100-20")
  expect_true(as.numeric(roll) == 29)
})

test_that("addition of rolls works", {
  set.seed(42)
  roll = roll_dice("1d4+1d6+1d8+1d10")
  expect_true(as.numeric(roll) == 16)
})

test_that("substraction of rolls works", {
  set.seed(42)
  roll = roll_dice("1d100-1d20")
  expect_true(as.numeric(roll) == 44)
})

test_that("parentheticals work", {
  set.seed(42)
  roll = roll_dice("(1d4+1d6)*20+(1d8+1d10)/2")
  expect_true(as.numeric(roll) == 125)
})

test_that("verbose works",{
  set.seed(42)
  expect_message(roll_dice("1d100",verbose = TRUE))
})

test_that("roll history works", {
  set.seed(42)
  roll = roll_dice("8d6!", roll_history = TRUE)
  expect_named(roll, c("result", "roll_history"))
  expect_equal(roll$result, calculate(roll$roll_history$`8d6!`))
})



