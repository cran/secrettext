context("Does the output have the correct number of characters?")

test_that("character encryption results in correct number of characters", {
  expect_equal(str_length(setcode("hello", 2, 2)), 5)
  expect_equal(str_length(setcode("hello   ", 2, 2)), 8)
})

context("Is the string decoded correctly?")

test_that("strings encrypted using setcode are correctly decoded", {
  expect_equal(decode(setcode("hello world", 5, 5), 5, 5), "hello world")
})

context("Are errors in argument inputs caught?")

test_that("setcode keys are between 1 and 25", {
  expect_error(setcode("hello world", 1, 26))
  expect_error(setcode("hello world", 26, 1))
  expect_error(setcode("hello world", 26, 26))
})

test_that("decode keys are between 1 and 25", {
  expect_error(decode("hello world", 1, 26))
  expect_error(decode("hello world", 26, 1))
  expect_error(decode("hello world", 26, 26))
})

test_that("setcode text is of length 1", {
  expect_error(setcode(c("hello", "world"), 5, 5))
})

test_that("setcode text is a character string", {
  expect_error(setcode(100, 5, 5))
})

test_that("decode text is of length 1", {
  expect_error(decode(c("hello", "world"), 5, 5))
})

test_that("decode text is a character string", {
  expect_error(decode(100, 5, 5))
})
