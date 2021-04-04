test_that("feed_get_url provides correct URLs", {
  expect_equal(
    feed_get_url(24310, "Detailed", "Light", TRUE),
    glue::as_glue("ftp://mediafeedarchive.aec.gov.au/24310/Detailed/Light/")
  )
  expect_equal(
    feed_get_url(24310, "Detailed", "Light", FALSE),
    glue::as_glue("ftp://mediafeed.aec.gov.au/24310/Detailed/Light/")
  )
  expect_equal(
    feed_get_url(24310, "Detailed", "Light"),
    glue::as_glue("ftp://mediafeed.aec.gov.au/24310/Detailed/Light/")
  )
})
