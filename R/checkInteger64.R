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
checkInteger64 <- function(
  x,
  lower         = -Inf,
  upper         =  Inf,
  any.missing   = TRUE,
  all.missing   = TRUE,
  len           = NULL,
  min.len       = NULL,
  max.len       = NULL,
  unique        = FALSE,
  sorted        = FALSE,
  names         = NULL,
  null.ok       = FALSE
) {
  if (!requireNamespace("bit64", quietly = TRUE))
    stop("Install package 'bit64' to perform checks of 64-bit integers")
  qassert(null.ok, "B1") # I don't know what this does.  I copied it from the tibble checks.
  if (is.null(x)) {
    if (null.ok)
      return(TRUE)
    return("Must be a bit64::integer64, not 'NULL'")
  }
  if (!bit64::is.integer64(x))
    return(paste0("Must be a bit64::integer64", if (null.ok) " (or 'NULL')" else "", sprintf(", not %s", guessType(x))))
  #checkInteger64(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok)

  # This section uses R code for the Int64, where was C code is used for the Int32.

  check_bound_lower_integer64(x, lower)
  check_bound_upper_integer64(x, upper)

}

check_bound_lower_integer64 <- function(x, lower) {
  # Don't check values if the lower bound is -Inf
  if (is.infinite(lower) && (lower < 0))  # This can be a floating point
    return(TRUE)

  if (!bit64::is.integer64(lower))
    return("Lower bound must be -Inf or of type 'bit64::integer64'")

  checks_success <- (lower <= x)
  if (bit64::any.integer64(!checks_success)) {
    # bad_index_first <- which(!checks_success)[1]
    # bad_value_first <- x[bad_index_first]

    return(sprintf("Element %i is not >= %g", which(!checks_success)[1], lower))
  }

  return(TRUE)
}

check_bound_upper_integer64 <- function(x, upper) {
  # Don't check values if the upper bound is +Inf
  if (is.infinite(lower) && (0 < upper))  # This can be a floating point
    return(TRUE)

  if (!bit64::is.integer64(upper))
    return("Upper bound must be +Inf or of type 'bit64::integer64'")

  checks_success <- (x <= upper)
  if (bit64::any.integer64(!checks_success)) {
    # bad_index_first <- which(!checks_success)[1]
    # bad_value_first <- x[bad_index_first]

    return(sprintf("Element %i is not <= %g", which(!checks_success)[1], upper))
  }

  return(TRUE)
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
