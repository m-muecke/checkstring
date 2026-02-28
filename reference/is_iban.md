# Check if an argument is a valid IBAN string

Validates IBAN format including MOD-97-10 check digit verification
(ISO/IEC 7064).

## Usage

``` r
is_iban(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid IBAN string, `FALSE` otherwise.

## References

<https://en.wikipedia.org/wiki/International_Bank_Account_Number>
