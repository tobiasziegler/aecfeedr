url <- "ftp://mediafeedarchive.aec.gov.au/24310/Detailed/Light/"
filename <- "aec-mediafeed-Detailed-Light-24310-20190729153147.zip"

feed_get_messages(url, filename, destpath = here::here("data-raw"))

file.copy(
  here::here(
    "data-raw",
    "aec-mediafeed-Detailed-Light-24310-20190729153147",
    "xml",
    "aec-mediafeed-results-detailed-light-24310.xml"
  ),
  here::here(
    "data-raw"
  )
)

file.rename(
  here::here(
    "data-raw",
    "aec-mediafeed-results-detailed-light-24310.xml"
  ),
  here::here(
    "data-raw",
    "example-results-detailed-light.xml"
  )
)

unlink(
  here::here(
    "data-raw",
    "aec-mediafeed-Detailed-Light-24310-20190729153147.zip"
  )
)

unlink(
  here::here(
    "data-raw",
    "aec-mediafeed-Detailed-Light-24310-20190729153147"
  ),
  recursive = TRUE
)
