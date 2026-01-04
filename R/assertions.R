#' Assert that dots are empty
#'
#' @param ... (`any`)\cr
#'   Ellipsis arguments to check.
#' @export
assert_empty_dots <- function(...) {
  nx <- ...length()
  if (nx == 0L) {
    return(invisible())
  }
  nms <- ...names()
  if (is.null(nms)) {
    stop(sprintf("Received %i unused unnamed arguments.", nx), call. = FALSE)
  }
  named <- nms[nzchar(nms)]
  if (nx == length(named)) {
    stop(sprintf("Received unused named argument(s): %s.", toString(named)), call. = FALSE)
  }
  stop(
    sprintf(
      "Received %i unused unnamed argument(s) and unused named argument(s): %s.",
      nx - length(named),
      toString(named)
    ),
    call. = FALSE
  )
}
