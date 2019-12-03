context("checkInteger64")

# testthat::test_file("tests/testthat/test_checkInteger64.R")

test_that("checkInteger64", {
  myobj = bit64::as.integer64(1)
  expect_succ_all(Integer64, myobj)
  myobj = 1L
  expect_fail_all(Integer64, myobj) # Int32
  myobj = 1
  expect_fail_all(Integer64, myobj) # double/float

  expect_true(testInteger64(bit64::as.integer64(0)))
  expect_false(testInteger64(NULL))
  # expect_false(testInteger64(bit64::as.integer64(TRUE)))
  # expect_false(testInteger64(bit64::as.integer64(FALSE)))
  expect_true(testInteger64(bit64::as.integer64(NA)))
  expect_false(testInteger64(NA, any.missing = FALSE))
  expect_false(testInteger64(NA, all.missing = FALSE))
  expect_true(testInteger64(bit64::as.integer64(1L)))
  expect_true(testInteger64(bit64::as.integer64(1:3), any.missing = FALSE, min.len = 1L, max.len = 3L))
  expect_false(testInteger64(bit64::as.integer64(1:3), any.missing = FALSE, len = 5))
  expect_true(testInteger64(bit64::as.integer64(1:3), lower = 1L, upper = 3L))
  expect_false(testInteger64(bit64::as.integer64(1:3), lower = 5))
  expect_false(testInteger64(bit64::as.integer64(1:3), upper = 1))

  expect_error(assertInteger64(1), "integer64")
})

test_that("bounds of vectors with only missings are not checked", {
  expect_true(checkInteger64(NA, lower = 1))
  expect_true(checkInteger64(NA_character_, upper = 10))
  expect_fail_all(Integer64, 0L, lower = 1L)
  expect_fail_all(Integer64, 100L, upper = 10L)
})

test_that("sorted works", {
  xu = sample(10)
  while(!is.unsorted(xu))
    xu = sample(10)
  xs = sort(xu)

  expect_true(checkInteger64(xs, sorted = TRUE))
  expect_true(grepl("sorted", checkInteger64(xu, sorted = TRUE), fixed = TRUE))

  expect_true(checkInteger64(1L, sorted = TRUE))
  expect_true(checkInteger64(Integer64(0), sorted = TRUE))
  expect_true(checkInteger64(bit64::NA_integer64_, sorted = TRUE))
  expect_true(checkInteger64(rep(bit64::NA_integer64_, 10), sorted = TRUE))

  for (i in 1:10) {
    x = sample(10)
    x[sample(10, sample(7:9, 1))] = NA
    if (is.unsorted(na.omit(x)))
      expect_true(grepl("sorted", checkInteger64(xu, sorted = TRUE), fixed = TRUE))
    else
      expect_true(grepl("sorted", checkInteger64(xu, sorted = TRUE), fixed = TRUE))
  }
})
