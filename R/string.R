#' Check if an argument is a email address string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid email address string, `FALSE` otherwise.
#' @export
is_email <- function(x) {
  regex <- "^(?!\\.)(?!.*\\.\\.)([A-Z0-9_'+\\-\\.]*[A-Z0-9_+\\-])@([A-Z0-9][A-Z0-9\\-]*\\.)+[A-Z]{2,}$" # nolint
  is_string(x) && grepl(regex, x, ignore.case = TRUE, perl = TRUE)
}

#' Check if an argument is an UUID string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid UUID string, `FALSE` otherwise.
#' @export
is_uuid <- function(x) {
  regex <- "^[0-9a-f]{8}\\b-[0-9a-f]{4}\\b-[0-9a-f]{4}\\b-[0-9a-f]{4}\\b-[0-9a-f]{12}$" # nolint
  is_string(x) && grepl(regex, x, ignore.case = TRUE, perl = TRUE)
}

#' Check if an argument is a base64 string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid base64 string, `FALSE` otherwise.
#' @export
is_base64 <- function(x) {
  regex <- "^([0-9a-zA-Z+/]{4})*((([0-9a-zA-Z+/]{2}==)|([0-9a-zA-Z+/]{3}=))?)$"
  is_string(x) && grepl(regex, x, perl = TRUE)
}

#' Check if an argument is base64url string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid base64url string, `FALSE` otherwise.
#' @export
is_base64url <- function(x) {
  regex <- "^([0-9a-zA-Z\\-_]{4})*((([0-9a-zA-Z\\-_]{2}(==)?)|([0-9a-zA-Z\\-_]{3}(=)?))?)$" # nolint
  is_string(x) && grepl(regex, x, perl = TRUE)
}

#' Check if an argument is url string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid URL string, `FALSE` otherwise.
#' @export
is_url <- function(x) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Please install the curl package to use this function.", call. = FALSE)
  }
  if (!is_string(x) || !nzchar(x)) {
    return(FALSE)
  }
  tryCatch(
    {
      curl::curl_parse_url(x, decode = FALSE)
      TRUE
    },
    error = function(e) FALSE
  )
}

#' Check if an argument is an IPv4 address string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid IPv4 address string, `FALSE` otherwise.
#' @export
is_ipv4 <- function(x) {
  octet <- "(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])"
  regex <- paste0("^", paste(rep(octet, 4L), collapse = "\\."), "$")
  is_string(x) && grepl(regex, x, perl = TRUE)
}

#' Check if an argument is a hexadecimal string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid non-empty hexadecimal string, `FALSE`
#'   otherwise.
#' @export
is_hex <- function(x) {
  is_string(x) && grepl("^[0-9a-f]+$", x, ignore.case = TRUE)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}
