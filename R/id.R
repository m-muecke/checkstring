#' Check if an argument is a valid IBAN string
#'
#' Validates IBAN format including MOD-97-10 check digit verification (ISO/IEC 7064).
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid IBAN string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/International_Bank_Account_Number>
#' @examples
#' is_iban("GB29NWBK60161331926819")
#' @export
is_iban <- function(x) {
  if (!is_string(x) || !grepl("^[A-Z]{2}[0-9]{2}[A-Z0-9]{11,30}$", x)) {
    return(FALSE)
  }
  codes <- utf8ToInt(x)
  n <- length(codes)
  mod97(c(codes[5:n], codes[1:4])) == 1L
}

#' Check if an argument is a BIC/SWIFT code string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid BIC/SWIFT code string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/ISO_9362>
#' <https://knowledge.xmldation.com/support/iso20022/general_rules/bic>
#' @examples
#' is_bic("DEUTDEFF")
#' @export
is_bic <- function(x) {
  is_string(x) && grepl("^[A-Z]{4}[A-Z]{2}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3})?$", x)
}

#' Check if an argument is a valid CUSIP string
#'
#' Validates CUSIP format including Luhn-variant check digit verification.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid CUSIP string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/CUSIP>
#' @examples
#' is_cusip("037833100")
#' @export
is_cusip <- function(x) {
  if (!is_string(x) || !grepl("^[A-Z0-9]{8}[0-9]$", x)) {
    return(FALSE)
  }
  codes <- utf8ToInt(x)
  vals <- char_to_val(codes[1:8])
  pos <- seq.int(2L, 8L, by = 2L)
  vals[pos] <- vals[pos] * 2L
  total <- sum(vals %/% 10L + vals %% 10L)
  check <- (10L - total %% 10L) %% 10L
  codes[9L] - 48L == check
}

#' Check if an argument is a valid FIGI string
#'
#' Validates FIGI (Financial Instrument Global Identifier) format including Luhn check digit
#' verification.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid FIGI string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/Financial_Instrument_Global_Identifier>
#' <https://www.openfigi.com/about/overview>
#' @examples
#' is_figi("BBG000BLNNH6")
#' @export
is_figi <- function(x) {
  if (!is_string(x) || !grepl("^[B-DF-HJ-NP-TV-Z]{2}G[B-DF-HJ-NP-TV-Z0-9]{8}[0-9]$", x)) {
    return(FALSE)
  }
  codes <- utf8ToInt(x)
  vals <- char_to_val(codes[1:11])
  pos <- seq.int(2L, 10L, by = 2L)
  vals[pos] <- vals[pos] * 2L
  total <- sum(vals %/% 10L + vals %% 10L)
  check <- (10L - total %% 10L) %% 10L
  codes[12L] - 48L == check
}

#' Check if an argument is a DOI string
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid DOI string, `FALSE` otherwise.
#' @references
#' <https://www.doi.org/the-identifier/what-is-a-doi/>
#' @examples
#' is_doi("10.1038/nphys1170")
#' @export
is_doi <- function(x) {
  regex <- "^10\\.\\d{4,9}/[-._;()/:A-Z0-9]+$"
  is_string(x) && grepl(regex, x, ignore.case = TRUE, perl = TRUE)
}

#' Check if an argument is a valid ISIN string
#'
#' Validates ISIN (International Securities Identification Number) format including Luhn check
#' digit verification.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid ISIN string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/International_Securities_Identification_Number>
#' @examples
#' is_isin("US0378331005")
#' @export
is_isin <- function(x) {
  if (!is_string(x) || !grepl("^[A-Z]{2}[A-Z0-9]{9}[0-9]$", x)) {
    return(FALSE)
  }
  codes <- utf8ToInt(x)
  vals <- char_to_val(codes)
  digits <- integer(24L)
  k <- 0L
  for (v in vals) {
    if (v >= 10L) {
      k <- k + 1L
      digits[k] <- v %/% 10L
      k <- k + 1L
      digits[k] <- v %% 10L
    } else {
      k <- k + 1L
      digits[k] <- v
    }
  }
  digits <- digits[1:k]
  pos <- seq.int(from = k - 1L, to = 1L, by = -2L)
  digits[pos] <- digits[pos] * 2L
  digits[pos] <- ifelse(digits[pos] > 9L, digits[pos] - 9L, digits[pos])
  sum(digits) %% 10L == 0L
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
#' @examples
#' is_isbn("978-0-306-40615-7")
#' @export
is_isbn <- function(x) {
  if (!is_string(x)) {
    return(FALSE)
  }
  digits <- gsub("[- ]", "", x)
  if (grepl("^\\d{9}[0-9X]$", digits)) {
    codes <- utf8ToInt(digits)
    d <- parse_check_digits(codes)
    sum(d * 10:1) %% 11L == 0L
  } else if (grepl("^\\d{13}$", digits)) {
    d <- utf8ToInt(digits) - 48L
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
#' @examples
#' is_issn("0378-5955")
#' @export
is_issn <- function(x) {
  if (!is_string(x) || !grepl("^\\d{4}-\\d{3}[0-9X]$", x)) {
    return(FALSE)
  }
  codes <- utf8ToInt(x)
  d <- parse_check_digits(codes[c(1:4, 6:9)])
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
#' @examples
#' is_lei("7H6GLXDRUGQFU57RNE97")
#' @export
is_lei <- function(x) {
  if (!is_string(x) || !grepl("^[A-Z0-9]{18}[0-9]{2}$", x)) {
    return(FALSE)
  }
  mod97(utf8ToInt(x)) == 1L
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
#' @examples
#' is_orcid("0000-0002-1825-0097")
#' @export
is_orcid <- function(x) {
  if (!is_string(x) || !grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", x)) {
    return(FALSE)
  }
  codes <- utf8ToInt(x)
  d <- codes[c(1:4, 6:9, 11:14, 16:19)] - 48L
  total <- 0L
  for (i in 1:15) {
    total <- (total + d[i]) * 2L
  }
  check <- (12L - total %% 11L) %% 11L
  if (check == 10L) d[16L] + 48L == 88L else d[16L] == check
}

#' Check if an argument is a valid SEDOL string
#'
#' Validates SEDOL format including weighted check digit verification.
#'
#' @param x (`any`)\cr
#'   Object to check.
#' @return `TRUE` if `x` is a valid SEDOL string, `FALSE` otherwise.
#' @references
#' <https://en.wikipedia.org/wiki/SEDOL>
#' @examples
#' is_sedol("0263494")
#' @export
is_sedol <- function(x) {
  if (!is_string(x) || !grepl("^[B-DF-HJ-NP-TV-Z0-9]{6}[0-9]$", x)) {
    return(FALSE)
  }
  codes <- utf8ToInt(x)
  vals <- char_to_val(codes[1:6])
  total <- sum(vals * c(1L, 3L, 1L, 7L, 3L, 9L))
  check <- (10L - total %% 10L) %% 10L
  codes[7L] - 48L == check
}

char_to_val <- function(x) {
  ifelse(x >= 65L, x - 55L, x - 48L)
}

parse_check_digits <- function(x) {
  ifelse(x == 88L, 10L, x - 48L)
}

mod97 <- function(codes) {
  vals <- ifelse(codes >= 65L, codes - 55L, codes - 48L)
  rem <- 0L
  for (v in vals) {
    if (v >= 10L) {
      rem <- (rem * 100L + v) %% 97L
    } else {
      rem <- (rem * 10L + v) %% 97L
    }
  }
  rem
}
