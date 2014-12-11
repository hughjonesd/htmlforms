

library(testthat)

context("Checks")

test_that("checker works", {
  check13 <- checker(x >= 1 && x <= 3, "error")
  expect_true(
    check13(3, list())
  )
  expect_match(
    check13(4, list())
  , "error")
  
  checkval <- checker(x == vals$y, "error")
  expect_true(
    checkval(3, list(y=3))
  )
  expect_match(
    checkval(4, list(y=3))
  , "error")
})

test_that("checker works in context", {
  y <- 4
  check_y <- checker(x==y, "myerror")
  expect_true(
    check_y(4, list())
  )
  expect_match(
    check_y(3, list())
  , "myerror")
})

test_that("check_length works", {
  chl <- check_length(2,4)
  expect_true(
    chl("foo", list())
  )
  expect_match(
    chl("foobar", list())
  , "4")
})

test_that("R errors dealt with right", {
  check_fail <- checker(nonexistent, "myerror")
  expect_that(
    check_fail(1, list())
  , throws_error())
# expect_that bug:
#   check_fail <- checker(nonexistent, "myerror", error=warning)
#   expect_that(
#     check_fail(1, list())
#   , gives_warning())
#   expect_match(
#     check_fail(1, list())
#   , "myerror")
  check_fail <- checker(nonexistent, "myerror", error=identity)
  expect_match(
    check_fail(1, list())
  , "myerror")
})