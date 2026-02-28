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

test_that("is_bic works", {
  expect_true(is_bic("DEUTDEFF"))
  expect_true(is_bic("DEUTDEFF500"))
  expect_true(is_bic("BNPAFRPP"))
  expect_true(is_bic("CHASUS33XXX"))

  expect_false(is_bic("DEUT")) # too short
  expect_false(is_bic("DEUTDEFF5000")) # too long
  expect_false(is_bic("DEU1DEFF")) # digit in institution code
  expect_false(is_bic("DEUTD1FF")) # digit in country code
  expect_false(is_bic("DEUTDE0F")) # 0 not allowed in location position 7
  expect_false(is_bic("DEUTDE1F")) # 1 not allowed in location position 7
  expect_false(is_bic("DEUTDEFO")) # O not allowed in location position 8
  expect_false(is_bic("deutdeff")) # lowercase
  expect_false(is_bic(123L))
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
  # sample ORCIDs from the ORCID website
  expect_true(is_orcid("0000-0002-1825-0097"))
  expect_true(is_orcid("0000-0001-5109-3700"))
  expect_true(is_orcid("0000-0002-1694-233X"))
  expect_true(is_orcid("0000-0001-5000-0007"))

  expect_false(is_orcid("0000-0002-1825-0098")) # bad check digit
  expect_false(is_orcid("0000-0003-0918-376")) # too short
  expect_false(is_orcid("0000-0003-0918-37666")) # too long
  expect_false(is_orcid("000000030918376")) # no hyphens
  expect_false(is_orcid("0000-0003-0918-376A")) # invalid check char
})

test_that("is_isbn works", {
  # ISBN-10
  expect_true(is_isbn("0306406152"))
  expect_true(is_isbn("0-306-40615-2"))
  expect_true(is_isbn("080442957X"))
  # ISBN-13
  expect_true(is_isbn("9780306406157"))
  expect_true(is_isbn("978-0-306-40615-7"))
  expect_true(is_isbn("978 0 306 40615 7"))

  expect_false(is_isbn("0306406153")) # bad check digit
  expect_false(is_isbn("9780306406158")) # bad check digit
  expect_false(is_isbn("030640615")) # too short
  expect_false(is_isbn("12345")) # wrong length
  expect_false(is_isbn(123L))
})

test_that("is_issn works", {
  expect_true(is_issn("0378-5955"))
  expect_true(is_issn("0317-8471"))
  expect_true(is_issn("1050-124X"))

  expect_false(is_issn("0378-5956")) # bad check digit
  expect_false(is_issn("03785955")) # no hyphen
  expect_false(is_issn("0378-595")) # too short
  expect_false(is_issn(123L))
})
