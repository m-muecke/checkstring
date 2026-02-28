#' Check if an argument is a email address string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid email address string, `FALSE` otherwise.
#' @examples
#' is_email("user@example.com")
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
#' @examples
#' is_uuid("550e8400-e29b-41d4-a716-446655440000")
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
#' @examples
#' is_base64("SGVsbG8gV29ybGQ=")
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
#' @examples
#' is_base64url("SGVsbG8gV29ybGQ")
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
#' @examples
#' is_url("https://example.com")
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
#' @examples
#' is_ipv4("192.168.1.1")
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
#' @examples
#' is_hex("deadbeef")
#' @export
is_hex <- function(x) {
  is_string(x) && grepl("^[0-9a-f]+$", x, ignore.case = TRUE)
}

#' Check if an argument is an MD5 hash string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid MD5 hash string, `FALSE` otherwise.
#' @examples
#' is_md5("d41d8cd98f00b204e9800998ecf8427e")
#' @export
is_md5 <- function(x) {
  is_string(x) && grepl("^[0-9a-f]{32}$", x, ignore.case = TRUE)
}

#' Check if an argument is a SHA-256 hash string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid SHA-256 hash string, `FALSE` otherwise.
#' @examples
#' is_sha256("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
#' @export
is_sha256 <- function(x) {
  is_string(x) && grepl("^[0-9a-f]{64}$", x, ignore.case = TRUE)
}

#' Check if an argument is a semantic versioning string
#'
#' Validates semantic versioning 2.0.0 format, including optional pre-release and build metadata.
#' Uses the official recommended regex from <https://semver.org/>.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid semver string, `FALSE` otherwise.
#' @references
#' <https://semver.org/>
#' @examples
#' is_semver("1.0.0")
#' @export
is_semver <- function(x) {
  regex <- "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$" # nolint
  is_string(x) && grepl(regex, x, perl = TRUE)
}

#' Check if an argument is a hostname string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid hostname string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/Hostname>
#' @examples
#' is_hostname("example.com")
#' @export
is_hostname <- function(x) {
  regex <- "^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)*[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$" # nolint
  is_string(x) && nchar(x) <= 253L && grepl(regex, x, ignore.case = TRUE)
}

#' Check if an argument is a MAC address string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid MAC address string, `FALSE` otherwise.
#' @examples
#' is_mac("00:1B:44:11:3A:B7")
#' @export
is_mac <- function(x) {
  regex <- "^[0-9a-f]{2}([:-])[0-9a-f]{2}(\\1[0-9a-f]{2}){4}$"
  is_string(x) && grepl(regex, x, ignore.case = TRUE)
}

#' Check if an argument is a ULID string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid ULID string, `FALSE` otherwise.
#' @references
#' <https://github.com/ulid/spec>
#' @examples
#' is_ulid("01ARZ3NDEKTSV4RRFFQ69G5FAV")
#' @export
is_ulid <- function(x) {
  is_string(x) && grepl("^[0-9A-HJKMNP-TV-Z]{26}$", x)
}

#' Check if an argument is a Nano ID string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid Nano ID string, `FALSE` otherwise.
#' @examples
#' is_nanoid("V1StGXR8_Z5jdHi6B-myT")
#' @export
is_nanoid <- function(x) {
  is_string(x) && grepl("^[A-Za-z0-9_-]{21}$", x)
}

#' Check if an argument is a CUID2 string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid CUID2 string, `FALSE` otherwise.
#' @examples
#' is_cuid2("ckopqwooh000001la8mbi2im9")
#' @export
is_cuid2 <- function(x) {
  is_string(x) && grepl("^[a-z][0-9a-z]{23,}$", x)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}
