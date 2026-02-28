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

test_that("is_doi works", {
  expect_true(is_doi("10.1000/xyz123"))
  expect_true(is_doi("10.1038/nphys1170"))
  expect_true(is_doi("10.1002/0470841559.ch1"))
  expect_true(is_doi("10.1093/ajae/aaq063"))

  expect_false(is_doi("11.1000/xyz123")) # wrong prefix
  expect_false(is_doi("10.12/xyz")) # registrant too short
  expect_false(is_doi("10.1000/")) # empty suffix
  expect_false(is_doi("10.1000")) # no slash
})

test_that("is_orcid works", {
  expect_true(is_orcid("0000-0003-0918-3766"))
  expect_true(is_orcid("0000-0002-1825-009X"))
  expect_true(is_orcid("0000-0001-5000-0007"))

  expect_false(is_orcid("0000-0003-0918-376")) # too short
  expect_false(is_orcid("0000-0003-0918-37666")) # too long
  expect_false(is_orcid("000000030918376")) # no hyphens
  expect_false(is_orcid("0000-0003-0918-376A")) # invalid check char
})
