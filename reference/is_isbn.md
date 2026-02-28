# Check if an argument is an ISBN string

Validates ISBN-10 and ISBN-13 formats, including check digit
verification. Hyphens and spaces are allowed as separators.

## Usage

``` r
is_isbn(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid ISBN string, `FALSE` otherwise.

## References

<https://en.wikipedia.org/wiki/International_Standard_Book_Number>
