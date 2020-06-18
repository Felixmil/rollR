
test_that("roll dice with addition works", {
  set.seed(42)
  roll = roll_dice("1d6+1")
  expect_true(roll == 2)
})

test_that("addition of rolls works", {
  set.seed(42)
  roll = roll_dice("1d4+1d6+1d8+1d10")
  expect_true(roll == 16)
})
