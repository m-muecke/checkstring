# checkstring 0.2.1

- `is_base64()`, `is_base64url()`, `is_doi()`, `is_email()`, `is_ipv4()`, `is_iso_datetime()`, `is_mime()`, `is_semver()`, and `is_uuid()` no longer accept strings with a trailing newline, such as `"1.0.0\n"`.
- `is_base64()` and `is_base64url()` now return `FALSE` for empty strings.
- `is_cuid2()` now rejects strings longer than 32 characters, the maximum CUID2 length.
- `is_email()` now rejects domain labels that end with a hyphen, such as `"user@domain-.com"`.
- `is_figi()` now rejects the reserved prefixes `BS`, `BM`, `GG`, `GB`, `GH`, `KY`, and `VG`.
- `is_ipv6()` no longer accepts a trailing single colon after a compressed group, such as `"1::2:"`.
- `is_isbn()` now rejects ISBN-13s that don't start with `978` or `979`, as well as ISMNs (`9790`).
- `is_iso_datetime()` now validates the timezone offset, rejecting out-of-range values such as `"+99:99"`.
- `is_ulid()` now rejects ULIDs whose first character is greater than `7`, which would overflow the 48-bit timestamp.

# checkstring 0.2.0

- `is_color_hex()` validates hex color strings (`#RGB`, `#RGBA`, `#RRGGBB`, `#RRGGBBAA`).
- `is_ipv6()` validates IPv6 address strings, including compressed (`::`) and IPv4-embedded forms (#18).
- `is_iso_date()` and `is_iso_datetime()` validate ISO 8601 date and datetime strings.
- `is_mime()` validates MIME type strings against the IANA-registered top-level types.

# checkstring 0.1.0

- Initial CRAN submission.
