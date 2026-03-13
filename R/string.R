#' Check if an argument is an ISO 8601 date string
#'
#' Validates the `YYYY-MM-DD` format with calendar correctness, including leap year handling.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid ISO 8601 date string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/ISO_8601>
#' @examples
#' is_iso_date("2024-01-15")
#' is_iso_date("2024-02-30")
#' @export
is_iso_date <- function(x) {
  if (!is_string(x) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    return(FALSE)
  }
  parts <- as.integer(strsplit(x, "-", fixed = TRUE)[[1L]])
  year <- parts[[1L]]
  month <- parts[[2L]]
  day <- parts[[3L]]
  if (month < 1L || month > 12L || day < 1L) {
    return(FALSE)
  }
  days_in_month <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
  if (month == 2L && (year %% 4L == 0L && (year %% 100L != 0L || year %% 400L == 0L))) {
    days_in_month[[2L]] <- 29L
  }
  day <= days_in_month[[month]]
}

#' Check if an argument is an ISO 8601 datetime string
#'
#' Validates the `YYYY-MM-DDTHH:MM:SS[.sss]` format with calendar correctness, including leap year
#' handling. The timezone designator is optional and can be `Z` or an offset like `+05:30`.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid ISO 8601 datetime string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/ISO_8601>
#' @examples
#' is_iso_datetime("2024-01-15T12:00:00Z")
#' is_iso_datetime("2024-01-15T12:00:00+05:30")
#' is_iso_datetime("2024-01-15T12:00:00")
#' @export
is_iso_datetime <- function(x) {
  regex <- "^(\\d{4}-\\d{2}-\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?(?:Z|[+-]\\d{2}:\\d{2})?$"
  if (!is_string(x) || !grepl(regex, x, perl = TRUE)) {
    return(FALSE)
  }
  m <- regmatches(x, regexec(regex, x, perl = TRUE))[[1L]]
  if (!is_iso_date(m[[2L]])) {
    return(FALSE)
  }
  hour <- as.integer(m[[3L]])
  min <- as.integer(m[[4L]])
  sec <- as.integer(m[[5L]])
  hour <= 23L && min <= 59L && sec <= 59L
}

#' Check if an argument is a hex color string
#'
#' Validates hex color codes in `#RGB`, `#RGBA`, `#RRGGBB`, or `#RRGGBBAA` format.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid hex color string, `FALSE` otherwise.
#' @references
#' <https://www.w3.org/TR/css-color-4/#hex-notation>
#' @examples
#' is_color_hex("#FF5733")
#' is_color_hex("#fff")
#' @export
is_color_hex <- function(x) {
  is_string(x) && grepl("^#([0-9a-f]{3,4}|[0-9a-f]{6}|[0-9a-f]{8})$", x, ignore.case = TRUE)
}

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

#' Check if an argument is a MIME type string
#'
#' Validates the `type/subtype` format where the top-level type must be one of the IANA-registered
#' types: `application`, `audio`, `font`, `image`, `message`, `model`, `multipart`, `text`, or
#' `video`.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid MIME type string, `FALSE` otherwise.
#' @references
#' <https://www.iana.org/assignments/media-types/media-types.xhtml>
#' @examples
#' is_mime("application/json")
#' is_mime("text/plain")
#' @export
is_mime <- function(x) {
  types <- "application|audio|font|image|message|model|multipart|text|video"
  regex <- paste0("^(", types, ")/[a-zA-Z0-9][a-zA-Z0-9!#$&\\-^_.+]*$")
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
