test_that("simple roll works", {
  set.seed(42)
  roll1 = as.numeric(calculate(rolltable("1d6")))
  roll2 = as.numeric(calculate(rolltable("1d6")))

  expect_true(roll1 == 1)
  expect_true(roll2 == 5)
})

test_that("keep highest works",{
  set.seed(42)
  roll = as.numeric(calculate(rolltable(("10d10h2"))))
  expect_equal(roll, 20)
  })

test_that("keep lowest works",{
  set.seed(42)
  roll = as.numeric(calculate(rolltable("10d10l2")))
  expect_equal(roll, 2)
})

test_that("exploding dice works", {
  set.seed(42)
  roll = as.numeric(calculate(rolltable("5d10!")))
  expect_equal(roll,30)
  roll = as.numeric(calculate(rolltable("10d10!>8")))
  expect_equal(roll, 87)
})

test_that("reroll dice works",{
  set.seed(42)
  roll = as.numeric(calculate(rolltable("10d6r1")))
  expect_equal(roll,35)
})

test_that("doubling dice works",{
  set.seed(42)
  roll = as.numeric(calculate(rolltable("10d6t1")))
  expect_equal(roll,27)
})



test_that("success dice works",{
  set.seed(42)
  roll = as.numeric(calculate(rolltable("6d6>4")))
  expect_equal(roll,1)
  roll = as.numeric(calculate(rolltable("6d6>=4")))
  expect_equal(roll, 2)
  roll = as.numeric(calculate(rolltable("6d6<3")))
  expect_equal(roll, 3)
  roll = as.numeric(calculate(rolltable("6d6<=2")))
  expect_equal(roll, 1)
  roll = as.numeric(calculate(rolltable("6d6=4")))
  expect_equal(roll, 2)
  roll = as.numeric(calculate(rolltable("6d6!>5=6")))
  expect_equal(roll, 3)
})

test_that("error works",{
  set.seed(42)
  expect_error(rolltable("wrong_syntax"))
})

