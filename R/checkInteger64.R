#' Check if an argument is a bit64::integer64
#'
#' @templateVar fn Integer64
#' @template x
#' @template na-handling
#' @inheritParams checkVector
#' @template bounds
#' @template sorted
#' @template null.ok
#' @template checker
#' @family basetypes
#' @export
#' @examples
#' library(bit64)
#' x <- as.integer64(11:15)
#' testInteger64(x)
#' testInteger64(x, len=5, any.missing = FALSE)
checkInteger64 <- function( lower = -Inf, upper = Inf, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, null.ok = FALSE) {
  if (!requireNamespace("bit64", quietly = TRUE))
    stop("Install package 'bit64' to perform checks of 64-bit integers")
  qassert(null.ok, "B1")
  if (is.null(x)) {
    if (null.ok)
      return(TRUE)
    return("Must be a bit64::integer64, not 'NULL'")
  }
  if (!bit64::is.integer64(x))
    return(paste0("Must be a bit64::integer64", if (null.ok) " (or 'NULL')" else "", sprintf(", not %s", guessType(x))))
  checkInteger64(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok)
}

#' @export
#' @rdname checkInteger64
check_integer64 = checkInteger64

#' @export
#' @include makeAssertion.R
#' @template assert
#' @rdname checkInteger64
assertInteger64 = makeAssertionFunction(checkInteger64, use.namespace = FALSE)

#' @export
#' @rdname checkInteger64
assert_integer64 = assertInteger64

#' @export
#' @include makeTest.R
#' @rdname checkInteger64
testInteger64 = makeTestFunction(checkInteger64)

#' @export
#' @rdname checkInteger64
test_integer64 = testInteger64

#' @export
#' @include makeExpectation.R
#' @template expect
#' @rdname checkInteger64
expect_integer64 = makeExpectationFunction(checkInteger64, use.namespace = FALSE)
