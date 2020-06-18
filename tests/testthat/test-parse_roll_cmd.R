test_that("parsing simple roll cmd works", {
  parsed_cmd = parse_roll_cmd("1d6")
  expect_type(parsed_cmd, "list")
  expect_equal(parsed_cmd$elements[1], "1d6")
  expect_true(is.na(parsed_cmd$operators[1]))
})


test_that("parsing simple roll cmd works", {
  parsed_cmd = parse_roll_cmd("1d6 + 2d10 + 4")
  expect_type(parsed_cmd,"list")
})
