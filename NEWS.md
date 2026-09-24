# checkstring (development version)

- `is_ipv6()` no longer accepts a trailing single colon after a compressed group, such as `"1::2:"`.
- `is_iso_datetime()` now validates the timezone offset, rejecting out-of-range values such as `"+99:99"`.

# checkstring 0.2.0

- `is_color_hex()` validates hex color strings (`#RGB`, `#RGBA`, `#RRGGBB`, `#RRGGBBAA`).
- `is_ipv6()` validates IPv6 address strings, including compressed (`::`) and IPv4-embedded forms (#18).
- `is_iso_date()` and `is_iso_datetime()` validate ISO 8601 date and datetime strings.
- `is_mime()` validates MIME type strings against the IANA-registered top-level types.

# checkstring 0.1.0

- Initial CRAN submission.
