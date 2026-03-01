# checkstring

The goal of checkstring is to provide string format validation functions
for R, inspired by [Zod’s](https://zod.dev/?id=strings) string
validators. It extends the
[checkmate](https://mllg.github.io/checkmate/) family of argument checks
with common string format validations such as email addresses, UUIDs,
URLs, IP addresses, and financial and academic identifiers like ISIN,
CUSIP, IBAN, ISBN, and ORCID, including check digit verification.

## Installation

You can install the released version of **checkstring** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("checkstring")
```

And the development version from [GitHub](https://github.com/) with:

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
is_isin("US0378331005")
#> [1] TRUE
is_iban("GB29NWBK60161331926819")
#> [1] TRUE
```

## Related work

- [checkmate](https://github.com/mllg/checkmate): Fast and versatile
  argument checks for R
- [figir](https://github.com/philaris/figir): Check validity of FIGI,
  CUSIP, ISIN, SEDOL
- [libbib](https://github.com/NYPL/libbib): Validate and normalize ISBN,
  ISSN, LCCN
