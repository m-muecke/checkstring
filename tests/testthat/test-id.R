test_that("is_iban works", {
  expect_true(is_iban("GB29NWBK60161331926819"))
  expect_true(is_iban("DE89370400440532013000"))
  expect_true(is_iban("FR7630006000011234567890189"))

  expect_false(is_iban("GB29NWBK60161331926818")) # bad check digits
  expect_false(is_iban("DE00370400440532013000")) # check digits 00
  expect_false(is_iban("1234567890")) # no country code
  expect_false(is_iban("GB29")) # too short
  expect_false(is_iban("gb29NWBK60161331926819")) # lowercase country
  expect_false(is_iban(123L))
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

test_that("is_cusip works", {
  expect_true(is_cusip("037833100")) # Apple
  expect_true(is_cusip("17275R102")) # Cisco
  expect_true(is_cusip("594918104")) # Microsoft

  expect_false(is_cusip("037833101")) # bad check digit
  expect_false(is_cusip("03783310")) # too short
  expect_false(is_cusip("0378331000")) # too long
  expect_false(is_cusip("03783310a")) # lowercase
  expect_false(is_cusip("03783310A")) # letter in check digit position
  expect_false(is_cusip(123L))
})

test_that("is_figi works", {
  expect_true(is_figi("BBG000BLNNH6")) # IBM
  expect_true(is_figi("BBG000B9XRY4")) # Apple

  expect_false(is_figi("BBG000BLNNH7")) # bad check digit
  expect_false(is_figi("BBG000BLNNH")) # too short
  expect_false(is_figi("BBG000BLNNH60")) # too long
  expect_false(is_figi("XYG000BLNNH6")) # wrong prefix
  expect_false(is_figi("BBG000BLANH6")) # vowel A not allowed
  expect_false(is_figi(123L))
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

test_that("is_isin works", {
  expect_true(is_isin("US0378331005")) # Apple
  expect_true(is_isin("GB0002634946")) # BAE Systems
  expect_true(is_isin("DE0005810055")) # Deutsche Boerse

  expect_false(is_isin("US0378331006")) # bad check digit
  expect_false(is_isin("US037833100")) # too short
  expect_false(is_isin("US03783310050")) # too long
  expect_false(is_isin("us0378331005")) # lowercase
  expect_false(is_isin(123L))
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

test_that("is_lei works", {
  expect_true(is_lei("7H6GLXDRUGQFU57RNE97"))
  expect_true(is_lei("529900T8BM49AURSDO55"))
  expect_true(is_lei("213800WSGIIZCXF1P572"))

  expect_false(is_lei("7H6GLXDRUGQFU57RNE98")) # bad check digits
  expect_false(is_lei("7H6GLXDRUGQFU57RNE9")) # too short
  expect_false(is_lei("7H6GLXDRUGQFU57RNE970")) # too long
  expect_false(is_lei("7h6glxdrugqfu57rne97")) # lowercase
  expect_false(is_lei(123L))
})

test_that("is_sedol works", {
  expect_true(is_sedol("0263494")) # BAE Systems
  expect_true(is_sedol("B0YBKJ7")) # Vodafone
  expect_true(is_sedol("0540528")) # GlaxoSmithKline

  expect_false(is_sedol("0263495")) # bad check digit
  expect_false(is_sedol("026349")) # too short
  expect_false(is_sedol("02634940")) # too long
  expect_false(is_sedol("A263494")) # vowel A not allowed
  expect_false(is_sedol(123L))
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
