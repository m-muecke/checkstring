#' Check if an argument is a valid IBAN string
#'
#' Validates IBAN format including MOD-97-10 check digit verification (ISO/IEC 7064).
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid IBAN string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/International_Bank_Account_Number>
#' @export
is_iban <- function(x) {
  if (!is_string(x) || !grepl("^[A-Z]{2}[0-9]{2}[A-Z0-9]{11,30}$", x)) {
    return(FALSE)
  }
  x <- paste0(substring(x, 5L), substring(x, 1L, 4L))
  mod97(x) == 1L
}

#' Check if an argument is a BIC/SWIFT code string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid BIC/SWIFT code string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/ISO_9362>
#' <https://knowledge.xmldation.com/support/iso20022/general_rules/bic>
#' @export
is_bic <- function(x) {
  is_string(x) && grepl("^[A-Z]{4}[A-Z]{2}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3})?$", x)
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

#' Check if an argument is a valid LEI string
#'
#' Validates LEI (Legal Entity Identifier) format including MOD-97-10 check digit verification
#' (ISO/IEC 7064).
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid LEI string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/Legal_Entity_Identifier>
#' <https://www.govinfo.gov/content/pkg/CFR-2016-title12-vol8/xml/CFR-2016-title12-vol8-part1003-appC.xml>
#' @export
is_lei <- function(x) {
  if (!is_string(x) || !grepl("^[A-Z0-9]{18}[0-9]{2}$", x)) {
    return(FALSE)
  }
  mod97(x) == 1L
}

#' Check if an argument is an ORCID string
#'
#' Validates ORCID format including ISO/IEC 7064:2003, MOD 11-2 check digit
#' verification.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid ORCID string, `FALSE` otherwise.
#' @references
#' <https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier>
#' @export
is_orcid <- function(x) {
  if (!is_string(x) || !grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", x)) {
    return(FALSE)
  }
  chars <- strsplit(gsub("-", "", x, fixed = TRUE), "", fixed = TRUE)[[1L]]
  total <- 0L
  for (d in as.integer(chars[1:15])) {
    total <- (total + d) * 2L
  }
  result <- (12L - total %% 11L) %% 11L
  expected <- if (result == 10L) "X" else as.character(result)
  chars[16L] == expected
}

parse_check_digits <- function(x, n) {
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  vapply(chars, \(x) if (x == "X") 10L else as.integer(x), NA_integer_, USE.NAMES = FALSE)
}

mod97 <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  rem <- 0L
  for (ch in chars) {
    if (grepl("[A-Z]", ch)) {
      rem <- (rem * 100L + match(ch, LETTERS) + 9L) %% 97L
    } else {
      rem <- (rem * 10L + as.integer(ch)) %% 97L
    }
  }
  rem
}
