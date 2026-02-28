# Check if an argument is a valid FIGI string

Validates FIGI (Financial Instrument Global Identifier) format including
Luhn check digit verification.

## Usage

``` r
is_figi(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid FIGI string, `FALSE` otherwise.

## References

<https://en.wikipedia.org/wiki/Financial_Instrument_Global_Identifier>
<https://www.openfigi.com/assets/content/figi-check-digit-2173341b2d.pdf>

## Examples

``` r
is_figi("BBG000BLNNH6")
#> [1] TRUE
```
