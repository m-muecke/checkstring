#' @export
is_email <- function(x) {
  regex <- "^(?!\\.)(?!.*\\.\\.)([A-Z0-9_'+\\-\\.]*[A-Z0-9_+\\-])@([A-Z0-9][A-Z0-9\\-]*\\.)+[A-Z]{2,}$" # nolint
  is_string(x) && grepl(regex, x, ignore.case = TRUE, perl = TRUE)
}

#' @export
is_uuid <- function(x) {
  regex <- "^[0-9a-f]{8}\\b-[0-9a-f]{4}\\b-[0-9a-f]{4}\\b-[0-9a-f]{4}\\b-[0-9a-f]{12}$" # nolint
  is_string(x) && grepl(regex, x, ignore.case = TRUE, perl = TRUE)
}

#' @export
is_base64 <- function(x) {
  regex <- "^([0-9a-zA-Z+/]{4})*((([0-9a-zA-Z+/]{2}==)|([0-9a-zA-Z+/]{3}=))?)$"
  is_string(x) && grepl(regex, x, perl = TRUE)
}

#' @export
is_base64url <- function(x) {
  regex <- "^([0-9a-zA-Z\\-_]{4})*((([0-9a-zA-Z\\-_]{2}(==)?)|([0-9a-zA-Z\\-_]{3}(=)?))?)$" # nolint
  is_string(x) && grepl(regex, x, perl = TRUE)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}
