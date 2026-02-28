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

#' Check if an argument is a hostname string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid hostname string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/Hostname>
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
#' @export
is_ulid <- function(x) {
  is_string(x) && grepl("^[0-9A-HJKMNP-TV-Z]{26}$", x)
}

#' Check if an argument is a Nano ID string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid Nano ID string, `FALSE` otherwise.
#' @export
is_nanoid <- function(x) {
  is_string(x) && grepl("^[A-Za-z0-9_-]{21}$", x)
}

#' Check if an argument is a CUID2 string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid CUID2 string, `FALSE` otherwise.
#' @export
is_cuid2 <- function(x) {
  is_string(x) && grepl("^[a-z][0-9a-z]{23,}$", x)
}

#' Check if an argument is a DOI string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid DOI string, `FALSE` otherwise.
#' @references
#' <https://www.doi.org/the-identifier/what-is-a-doi/>
#' @export
is_doi <- function(x) {
  regex <- "^10\\.\\d{4,9}/[-._;()/:A-Z0-9]+$"
  is_string(x) && grepl(regex, x, ignore.case = TRUE, perl = TRUE)
}

#' Check if an argument is an ORCID string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid ORCID string, `FALSE` otherwise.
#' @references
#' <https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier>
#' @export
is_orcid <- function(x) {
  is_string(x) && grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", x)
}

#' Check if an argument is an ISBN string
#'
#' Validates ISBN-10 and ISBN-13 formats, including check digit verification.
#' Hyphens and spaces are allowed as separators.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid ISBN string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/International_Standard_Book_Number>
#' @export
is_isbn <- function(x) {
  if (!is_string(x)) {
    return(FALSE)
  }
  digits <- gsub("[- ]", "", x)
  if (grepl("^\\d{9}[0-9X]$", digits)) {
    d <- parse_check_digits(digits, 10L)
    sum(d * 10:1) %% 11L == 0L
  } else if (grepl("^\\d{13}$", digits)) {
    d <- as.integer(strsplit(digits, "", fixed = TRUE)[[1L]])
    sum(d * rep_len(c(1L, 3L), 13L)) %% 10L == 0L
  } else {
    FALSE
  }
}

#' Check if an argument is an ISSN string
#'
#' Validates ISSN format including check digit verification.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid ISSN string, `FALSE` otherwise.
#' @references
#' <https://www.loc.gov/issn/check.html>
#' @export
is_issn <- function(x) {
  if (!is_string(x) || !grepl("^\\d{4}-\\d{3}[0-9X]$", x)) {
    return(FALSE)
  }
  d <- parse_check_digits(gsub("-", "", x, fixed = TRUE), 8L)
  sum(d * 8:1) %% 11L == 0L
}

parse_check_digits <- function(x, n) {
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  vapply(chars, \(x) if (x == "X") 10L else as.integer(x), NA_integer_, USE.NAMES = FALSE)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}
