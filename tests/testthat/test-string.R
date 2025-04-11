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
