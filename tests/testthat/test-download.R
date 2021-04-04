test_that("feed_get_url provides correct URLs", {
  expect_equal(
    feed_get_url(25881, "Detailed", "Light", TRUE),
    glue::as_glue("ftp://mediafeedarchive.aec.gov.au/25881/Detailed/Light/")
  )
  expect_equal(
    feed_get_url(25881, "Detailed", "Light", FALSE),
    glue::as_glue("ftp://mediafeed.aec.gov.au/25881/Detailed/Light/")
  )
  expect_equal(
    feed_get_url(25881, "Detailed", "Light"),
    glue::as_glue("ftp://mediafeed.aec.gov.au/25881/Detailed/Light/")
  )
})

test_that("invalid feed arguments cause errors", {
  expect_error(
    feed_get_url(25881, "Invalid", "Light"),
    class = "rlang_error"
  )
  expect_error(
    feed_get_url(25881, "Detailed", "Invalid"),
    class = "rlang_error"
  )
})

test_that("invalid feed granularity/verbosity combinations cause errors", {
  expect_error(
    feed_get_url(25881, "Detailed", "Eml"),
    class = "aecfeedr_error"
  )
  expect_error(
    feed_get_url(25881, "Standard", "LightProgress"),
    class = "aecfeedr_error"
  )
})

test_that("feed_list_files gets directory listing", {
  skip_on_cran()
  skip_if_offline()

  x <- feed_list_files("ftp://mediafeedarchive.aec.gov.au/25881/Detailed/Preload/")
  expect_length(x, 2)
  expect_type(x, "character")
})

test_that("feed_get_messages downloads and unzips files", {
  skip_on_cran()
  skip_if_offline()

  dir <- withr::local_tempdir()

  x <- feed_get_messages("ftp://mediafeedarchive.aec.gov.au/25881/Detailed/Preload/", "aec-mediafeed-Detailed-Preload-25881-20201124131220.zip", dir)

  expect_type(x, "character")
  expect_length(x, 50)

  files <- list.files(dir)
  expect_snapshot(files)
})
