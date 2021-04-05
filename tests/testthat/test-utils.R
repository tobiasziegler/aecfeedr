test_that("replace_zeroes_with_na works", {
  x <- tibble::tribble(
    ~contest_id, ~pollingplace_id, ~candidate_id, ~votes,
     1L,         1L,                1L,              0L,
     1L,         1L,                2L,              0L,
     1L,         2L,                1L,            100L,
     1L,         2L,                2L,              0L
  )

  expect_snapshot(replace_zeroes_with_na(x, contest_id, pollingplace_id))
})

# This is an issue because the AEC feeds don't include the XML elements for
# two-candidate preferred candidates and results before results are available.
test_that("replace_zeroes_with_na handles empty results", {
  x <- tibble::tribble(
    ~contest_id, ~pollingplace_id, ~candidate_id, ~votes
  )
  expect_snapshot(replace_zeroes_with_na(x, contest_id, pollingplace_id))
})
