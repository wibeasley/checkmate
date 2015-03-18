context("guessType")

test_that("guessType", {
  xb = logical(10)
  xi = integer(10)
  xd = double(10)
  xc = complex(10)
  xs = letters[1:10]
  xl = as.list(1:10)
  xm = matrix(1:9, 3)
  xa = array(1:3)
  xf = data.frame(a=1:5, b=1:5)

  expect_true(grepl("NULL'$", checkLogical(NULL)))
  expect_true(grepl("logical'$", checkInteger(xb)))
  expect_true(grepl("integer'$", checkLogical(xi)))
  expect_true(grepl("double'$", checkLogical(xd)))
  expect_true(grepl("complex'$", checkLogical(xc)))
  expect_true(grepl("character'$", checkLogical(xs)))
  expect_true(grepl("factor'$", checkLogical(factor(xs))))
  expect_true(grepl("list'$", checkLogical(xl)))
  expect_true(grepl("matrix'$", checkLogical(xm)))
  expect_true(grepl("array'$", checkLogical(xa)))
  expect_true(grepl("frame'$", checkLogical(xf)))
  expect_true(grepl("myclass'$", checkLogical(setClasses(1, "myclass"))))
})
