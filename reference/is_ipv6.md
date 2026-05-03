# Check if an argument is an IPv6 address string

Validates full, compressed (\`::\`) and IPv4-embedded
(\`::ffff:192.168.1.1\`) forms. Zone IDs (e.g. \`

## Usage

``` r
is_ipv6(x)
```

## Arguments

- x:

  (\`any\`)  
  Object to check.

## Value

\`TRUE\` if \`x\` is a valid IPv6 address string, \`FALSE\` otherwise.

## Examples

``` r
is_ipv6("2001:0db8:85a3:0000:0000:8a2e:0370:7334")
#> [1] TRUE
is_ipv6("::1")
#> [1] TRUE
```
