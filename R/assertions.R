#' Assert that dots are empty
#'
#' @param ... (`any`)\cr
#'   Ellipsis arguments to check.
#' @export
assert_empty_dots <- function(...) {
  n <- ...length()
  if (n == 0L) {
    return(invisible())
  }
  nms <- ...names()
  if (is.null(nms)) {
    stop(sprintf("Received %i unused unnamed arguments.", n), call. = FALSE)
  }
  named <- nms[nzchar(nms)]
  n_unnamed <- n - length(named)
  if (n_unnamed == 0L) {
    stop(sprintf("Received unused named argument(s): %s.", toString(named)), call. = FALSE)
  }
  stop(
    sprintf(
      "Received %i unused unnamed argument(s) and unused named argument(s): %s.",
      n_unnamed,
      toString(named)
    ),
    call. = FALSE
  )
}
