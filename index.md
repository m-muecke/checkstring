# checkstring

The goal of checkstring is to provide string format validation functions
for R, inspired by [Zod’s](https://zod.dev/?id=strings) string
validators. It extends the
[checkmate](https://mllg.github.io/checkmate/) family of argument checks
with common string format validations such as email addresses, UUIDs,
URLs, IP addresses, and encodings.

## Installation

You can install the development version of checkstring from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("m-muecke/checkstring")
```

## Usage

``` r
library(checkstring)

is_email("user@example.com")
#> [1] TRUE
is_uuid("550e8400-e29b-41d4-a716-446655440000")
#> [1] TRUE
is_ipv4("192.168.1.1")
#> [1] TRUE
is_hex("deadbeef")
#> [1] TRUE
is_url("https://example.com")
#> [1] TRUE
is_base64("SGVsbG8gV29ybGQ=")
#> [1] TRUE
```
