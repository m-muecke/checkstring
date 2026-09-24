# Check if an argument is a valid FIGI string

Validates FIGI (Financial Instrument Global Identifier) format including
Luhn check digit verification. The prefixes `BS`, `BM`, `GG`, `GB`,
`GH`, `KY`, and `VG` are reserved to avoid collisions with ISINs and are
rejected.

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
<https://www.openfigi.com/about/overview>
<https://www.omg.org/spec/FIGI/>

## Examples

``` r
is_figi("BBG000BLNNH6")
#> [1] TRUE
```
