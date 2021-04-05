test_that("unpack_xml works", {
  x <- xml2::read_xml("./input/test-input.xml")
  feed_id <- xml2::xml_attr(xml2::xml_find_first(x, "/feed"), "id")
  contests_xml <- xml2::xml_find_all(x, "/feed/contest")
  df <-
    tibble::tibble(
      feed_id = feed_id,
      contest_name = xml2::xml_text(xml2::xml_find_first(contests_xml, "./contestname")),
      pp_xml = purrr::map(contests_xml, ~ xml2::xml_find_all(., "./pollingplace"))
    )

  unpack_formula <-
    ~ tibble::tibble(
      pp_id = xml2::xml_text(xml2::xml_find_first(.x, "./id")),
      pp_name = xml2::xml_text(xml2::xml_find_first(.x, "./name"))
    )

  expect_snapshot(unpack_xml(df, .data$pp_xml, unpack_formula))
})
