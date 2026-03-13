test_that("is_iso_date works", {
  expect_true(is_iso_date("2024-01-15"))
  expect_true(is_iso_date("2024-02-29")) # leap year
  expect_true(is_iso_date("2000-02-29")) # divisible by 400
  expect_true(is_iso_date("0001-01-01"))

  expect_false(is_iso_date("2024-02-30")) # feb too many days
  expect_false(is_iso_date("2023-02-29")) # not a leap year
  expect_false(is_iso_date("1900-02-29")) # divisible by 100 but not 400
  expect_false(is_iso_date("2024-13-01")) # invalid month
  expect_false(is_iso_date("2024-00-01")) # month zero
  expect_false(is_iso_date("2024-01-00")) # day zero
  expect_false(is_iso_date("2024-1-15")) # unpadded month
  expect_false(is_iso_date("24-01-15")) # short year
  expect_false(is_iso_date("2024/01/15")) # wrong separator
  expect_false(is_iso_date(1L))
})

test_that("is_iso_datetime works", {
  expect_true(is_iso_datetime("2024-01-15T12:00:00Z"))
  expect_true(is_iso_datetime("2024-01-15T00:00:00Z"))
  expect_true(is_iso_datetime("2024-01-15T23:59:59Z"))
  expect_true(is_iso_datetime("2024-01-15T12:00:00.123Z"))
  expect_true(is_iso_datetime("2024-01-15T12:00:00.123456Z"))
  expect_true(is_iso_datetime("2024-01-15T12:00:00")) # local time
  expect_true(is_iso_datetime("2024-01-15T12:00:00+05:30")) # offset
  expect_true(is_iso_datetime("2024-01-15T12:00:00-08:00"))
  expect_true(is_iso_datetime("2024-01-15T12:00:00.123+05:30"))

  expect_false(is_iso_datetime("2024-01-15T24:00:00Z")) # hour 24
  expect_false(is_iso_datetime("2024-01-15T12:60:00Z")) # minute 60
  expect_false(is_iso_datetime("2024-01-15T12:00:60Z")) # second 60
  expect_false(is_iso_datetime("2024-02-30T12:00:00Z")) # invalid date
  expect_false(is_iso_datetime("2024-01-15 12:00:00Z")) # space instead of T
  expect_false(is_iso_datetime("2024-01-15T12:00:00+5:30")) # unpadded offset hour
  expect_false(is_iso_datetime("2024-01-15T12:00:00+05")) # missing offset minutes
  expect_false(is_iso_datetime(1L))
})

test_that("is_color_hex works", {
  expect_true(is_color_hex("#fff"))
  expect_true(is_color_hex("#FFF"))
  expect_true(is_color_hex("#fffa"))
  expect_true(is_color_hex("#FF5733"))
  expect_true(is_color_hex("#ff5733aa"))
  expect_true(is_color_hex("#000000"))

  expect_false(is_color_hex("FF5733")) # missing #
  expect_false(is_color_hex("#ff")) # too short
  expect_false(is_color_hex("#fffff")) # 5 chars
  expect_false(is_color_hex("#fffffff")) # 7 chars
  expect_false(is_color_hex("#gggggg")) # invalid hex
  expect_false(is_color_hex(1L))
})

test_that("is_email works", {
  expect_true(is_email("user.name+tag@example.com"))
  expect_true(is_email("USER_123@sub.domain.co"))
  expect_true(is_email("a_b-c.d+e@domain-name.com"))

  expect_false(is_email(".username@example.com")) # starts with dot
  expect_false(is_email("user..name@example.com")) # consecutive dots
  expect_false(is_email("user.@example.com")) # ends local part with dot
  expect_false(is_email("user@.com")) # domain starts with dot
  expect_false(is_email("user@com")) # no dot in domain
  expect_false(is_email("user@domain..com")) # consecutive dots in domain
  expect_false(is_email("user@domain.c")) # TLD too short
})

test_that("is_mime works", {
  expect_true(is_mime("application/json"))
  expect_true(is_mime("text/plain"))
  expect_true(is_mime("image/png"))
  expect_true(is_mime("application/vnd.api+json"))
  expect_true(is_mime("audio/mpeg"))
  expect_true(is_mime("multipart/form-data"))
  expect_true(is_mime("font/woff2"))
  expect_true(is_mime("model/gltf+json"))

  expect_false(is_mime("text")) # missing subtype
  expect_false(is_mime("text/")) # empty subtype
  expect_false(is_mime("foo/plain")) # invalid top-level type
  expect_false(is_mime("text/plain; charset=utf-8")) # parameters not allowed
  expect_true(is_mime("TEXT/plain")) # case-insensitive per RFC 2045
  expect_false(is_mime(1L))
})

test_that("is_uuid works", {
  expect_true(is_uuid("123e4567-e89b-12d3-a456-426614174000"))
  expect_true(is_uuid("550e8400-e29b-41d4-a716-446655440000"))

  expect_false(is_uuid("123e4567e89b12d3a456426614174000")) # missing hyphens
  expect_false(is_uuid("123e4567-e89b-12d3-a456-42661417400")) # too short
  expect_false(is_uuid("123e4567-e89b-12d3-a456-4266141740000")) # too long
  expect_false(is_uuid("g23e4567-e89b-12d3-a456-426614174000")) # invalid hex char
})

test_that("is_base64 works", {
  expect_true(is_base64("TWFu"))
  expect_true(is_base64("TWE="))
  expect_true(is_base64("TQ=="))
  expect_true(is_base64("U29tZSBkYXRhIHdpdGggL3VzZXJzLw=="))

  expect_false(is_base64("TWE")) # not padded
  expect_false(is_base64("TWE===")) # overpadded
  expect_false(is_base64("TWE*")) # invalid char
})

test_that("is_base64url works", {
  expect_true(is_base64url("TWFu"))
  expect_true(is_base64url("TWE"))
  expect_true(is_base64url("TQ"))
  expect_true(is_base64url("U29tZV9kYXRhLXdpdGgtL3VzZXJzLw"))

  expect_false(is_base64url("TWE===")) # overpadded
  expect_false(is_base64url("TWE*")) # invalid char
})

test_that("is_ipv4 works", {
  expect_true(is_ipv4("0.0.0.0"))
  expect_true(is_ipv4("127.0.0.1"))
  expect_true(is_ipv4("192.168.1.1"))
  expect_true(is_ipv4("255.255.255.255"))

  expect_false(is_ipv4("256.0.0.1")) # octet > 255
  expect_false(is_ipv4("192.168.1")) # only 3 octets
  expect_false(is_ipv4("192.168.1.1.1")) # 5 octets
  expect_false(is_ipv4("192.168.01.1")) # leading zero
  expect_false(is_ipv4("abc.def.ghi.jkl")) # non-numeric
  expect_false(is_ipv4(1L))
})

test_that("is_hex works", {
  expect_true(is_hex("1a2b3c"))
  expect_true(is_hex("DEADBEEF"))
  expect_true(is_hex("0"))
  expect_true(is_hex("abcdef0123456789"))

  expect_false(is_hex("")) # empty
  expect_false(is_hex("0x1a2b")) # prefix
  expect_false(is_hex("ghijkl")) # non-hex chars
  expect_false(is_hex("1a 2b")) # spaces
  expect_false(is_hex(123L))
})

test_that("is_md5 works", {
  expect_true(is_md5("d41d8cd98f00b204e9800998ecf8427e"))
  expect_true(is_md5("D41D8CD98F00B204E9800998ECF8427E"))

  expect_false(is_md5("d41d8cd98f00b204e9800998ecf8427")) # too short
  expect_false(is_md5("d41d8cd98f00b204e9800998ecf8427ea")) # too long
  expect_false(is_md5("g41d8cd98f00b204e9800998ecf8427e")) # non-hex
})

test_that("is_sha256 works", {
  expect_true(is_sha256("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  expect_true(is_sha256("E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855"))

  expect_false(is_sha256("e3b0c44298fc1c149afbf4c8996fb924")) # too short (md5)
  expect_false(is_sha256("g3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")) # non-hex
})

test_that("is_semver works", {
  expect_true(is_semver("1.0.0"))
  expect_true(is_semver("0.1.0"))
  expect_true(is_semver("1.0.0-alpha"))
  expect_true(is_semver("1.0.0-alpha.1"))
  expect_true(is_semver("1.0.0+build.123"))
  expect_true(is_semver("1.0.0-alpha.1+build.123"))
  expect_true(is_semver("10.20.30"))

  expect_false(is_semver("1.0")) # missing patch
  expect_false(is_semver("1")) # just major
  expect_false(is_semver("01.0.0")) # leading zero
  expect_false(is_semver("1.0.0-")) # trailing hyphen
  expect_false(is_semver("1.0.0+")) # trailing plus
  expect_false(is_semver("v1.0.0")) # v prefix
})

test_that("is_hostname works", {
  expect_true(is_hostname("example.com"))
  expect_true(is_hostname("sub.domain.example.com"))
  expect_true(is_hostname("localhost"))
  expect_true(is_hostname("my-host"))
  expect_true(is_hostname("a"))

  expect_false(is_hostname("-example.com")) # starts with hyphen
  expect_false(is_hostname("example-.com")) # ends with hyphen
  expect_false(is_hostname("exam ple.com")) # space
  expect_false(is_hostname(".example.com")) # leading dot
  expect_false(is_hostname("")) # empty
  expect_false(is_hostname(paste(rep("a", 254), collapse = ""))) # too long
})

test_that("is_mac works", {
  expect_true(is_mac("00:1B:44:11:3A:B7"))
  expect_true(is_mac("aa:bb:cc:dd:ee:ff"))
  expect_true(is_mac("AA-BB-CC-DD-EE-FF"))

  expect_false(is_mac("00:1B:44:11:3A")) # too short
  expect_false(is_mac("00:1B:44:11:3A:B7:00")) # too long
  expect_false(is_mac("00:1B:44:11:3A:GG")) # invalid hex
  expect_false(is_mac("00:1B:44-11:3A:B7")) # mixed separators
  expect_false(is_mac("001B44113AB7")) # no separators
})

test_that("is_ulid works", {
  expect_true(is_ulid("01ARZ3NDEKTSV4RRFFQ69G5FAV"))
  expect_true(is_ulid("01H5V6E3MXHP0GGQB7K8CPWM1A"))

  expect_false(is_ulid("01ARZ3NDEKTSV4RRFFQ69G5FA")) # too short
  expect_false(is_ulid("01ARZ3NDEKTSV4RRFFQ69G5FAVX")) # too long
  expect_false(is_ulid("01ARZ3NDEKTSV4RRFFQ69G5FAi")) # lowercase
  expect_false(is_ulid("01ARZ3NDEKTSV4RRFFQ69G5FAO")) # invalid char O
})

test_that("is_nanoid works", {
  expect_true(is_nanoid("V1StGXR8_Z5jdHi6B-myT"))
  expect_true(is_nanoid("012345678901234567890"))

  expect_false(is_nanoid("V1StGXR8_Z5jdHi6B-my")) # too short
  expect_false(is_nanoid("V1StGXR8_Z5jdHi6B-myTx")) # too long
  expect_false(is_nanoid("V1StGXR8_Z5jdHi6B my")) # space
})

test_that("is_cuid2 works", {
  expect_true(is_cuid2("ckopqwooh000001la8mbi2im9"))
  expect_true(is_cuid2("abcdefghijklmnopqrstuvwx"))

  expect_false(is_cuid2("Akopqwooh000001la8mbi2im9")) # starts uppercase
  expect_false(is_cuid2("1kopqwooh000001la8mbi2im9")) # starts with digit
  expect_false(is_cuid2("abc")) # too short
})
