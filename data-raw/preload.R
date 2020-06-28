url <- "ftp://mediafeedarchive.aec.gov.au/24310/Detailed/Preload/"
filename <- "aec-mediafeed-Detailed-Preload-24310-20190517164959.zip"

feed_get_messages(url, filename, destpath = here::here("data-raw"))

file.copy(
  here::here(
    "data-raw",
    "aec-mediafeed-Detailed-Preload-24310-20190517164959",
    "xml",
    "aec-mediafeed-results-detailed-preload-24310.xml"
  ),
  here::here(
    "data-raw"
  )
)

file.rename(
  here::here(
    "data-raw",
    "aec-mediafeed-results-detailed-preload-24310.xml"
  ),
  here::here(
    "data-raw",
    "example-results-detailed-preload.xml"
  )
)

unlink(
  here::here(
    "data-raw",
    "aec-mediafeed-Detailed-Preload-24310-20190517164959.zip"
  )
)

unlink(
  here::here(
    "data-raw",
    "aec-mediafeed-Detailed-Preload-24310-20190517164959"
  ),
  recursive = TRUE
)
