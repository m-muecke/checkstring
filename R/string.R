is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

is_email <- function(x) {
  is_string(x)
}
