#' Check for existence and access rights of directories
#'
#' @templateVar fn Directory
#' @template checker
#' @inheritParams checkAccess
#' @inheritParams checkFile
#' @family filesystem
#' @export
#' @examples
#'  # Is R's home directory readable?
#'  test(R.home(), "access", "r")
#'
#'  # Is R's home directory writable?
#'  test(R.home(), "access", "w")
checkDirectory = function(x, access = "") {
  qassert(x, "S")
  if (length(x) == 0L)
    return("No directory provided")

  isdir = file.info(x)$isdir
  w = which.first(is.na(isdir))
  if (length(w) > 0L)
    return(sprintf("Directory '%s' does not exists", x[w]))
  w = which.first(!isdir)
  if (length(w) > 0L)
    return(sprintf("Directory extected, but file in place: '%s'", x[w]))

  return(checkAccess(x, access))
}

#' @rdname checkDirectory
#' @export
assertDirectory = function(x, access = "", .var.name) {
  makeAssertion(checkDirectory(x, access), vname(x, .var.name))
}

#' @rdname checkDirectory
#' @export
testDirectory = function(x, access = "", .var.name) {
  makeTest(checkDirectory(x, access))
}