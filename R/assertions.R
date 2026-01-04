#' Assert that dots are empty
#'
#' @param ... (`any`) ellipsis arguments to check.
#' @export
assert_empty_dots <- function(...) {
  nx <- ...length()
  if (nx == 0L) {
    return()
  }
  nms <- ...names()
  if (is.null(nms)) {
    stop(sprintf("Received %i unnamed argument that was not used.", nx), call. = FALSE)
  }
  nms2 <- nms[nzchar(nms)]
  if (length(nms2) == length(nms)) {
    stop(
      sprintf("Received the following named arguments that were unused: %s.", toString(nms2)),
      call. = FALSE
    )
  }
  stop(
    sprintf(
      "Received unused arguments: %i unnamed, as well as named arguments %s.",
      length(nms) - length(nms2),
      toString(nms2)
    ),
    call. = FALSE
  )
}
