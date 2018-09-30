#' Read the first preference House results by polling place from an XML message
#'
#' Description goes here
#'
#' @param x A string, connection or raw vector to be processed by `read_xml`
#'
#' @return A tibble containing the first preference results by polling place
#' @export
read_results_house_fp_by_pollingplace <- function(x) {
  # Read the XML document
  xml <- xml2::read_xml(x)

  # Cache the extracted namespaces for use in XPath searches
  ns <- xml2::xml_ns(xml)

  # Start by finding the nodeset containing each House contest in the document
  contests <- xml2::xml_find_all(
    xml,
    "/d1:MediaFeed/d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest",
    ns = ns
  )

  # Create a tibble by parsing the relevant XML nodes
  results <- tibble::tibble(
    contest_id = contests %>%
      xml2::xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
      xml2::xml_attr("Id"),
    pp_xml = contests %>%
      purrr::map(~ xml2::xml_find_all(
        .,
        "./d1:PollingPlaces/d1:PollingPlace",
        ns = ns
      ))
  )

  # Unpack the polling place XML for each contest into tibbles
  results <-
    results %>%
    dplyr::mutate(
      pollingplaces = pp_xml %>%
        purrr::map(~ tibble::tibble(
          pollingplace_id = .x %>%
            xml2::xml_find_first("./d1:PollingPlaceIdentifier", ns = ns) %>%
            xml2::xml_attr("Id"),
          fp_xml = .x %>%
            purrr::map(~ xml2::xml_find_all(
              .,
              "./d1:FirstPreferences/*",
              ns = ns
            ))
        ))
    )

  # Remove the polling place XML nodesets now that they've been unpacked
  results <-
    results %>%
    dplyr::select(-pp_xml)

  # Unnest the data so each row contains results for one polling place
  results <-
    results %>%
    tidyr::unnest()

  # Unpack the first preferences XML for each polling place into tibbles
  results <-
    results %>%
    dplyr::mutate(
      firstpreferences = fp_xml %>%
        purrr::map(~ tibble::tibble(
          vote_type = .x %>%
            xml2::xml_name(),
          candidate_id = .x %>%
            xml2::xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
            xml2::xml_attr("Id"),
          votes = .x %>%
            xml2::xml_find_first("./d1:Votes", ns = ns) %>%
            xml2::xml_text()
        ))
    )

  # Remove the first preferences XML nodesets now that they've been unpacked
  results <-
    results %>%
    dplyr::select(-fp_xml)

  # Unnest the data so each row contains one first preference count
  results <-
    results %>%
    tidyr::unnest()

  # Convert the data to appropriate types
  results <-
    results %>%
    readr::type_convert(
      col_types = readr::cols(
        contest_id = readr::col_integer(),
        pollingplace_id = readr::col_integer(),
        vote_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        votes = readr::col_integer()
      )
    )

  # Return the tibble containing the results
  results
}

#' Read the House information from the Detailed Preload Results XML message
#'
#' Description goes here
#'
#' @param x A string, connection or raw vector to be processed by `read_xml`
#'
#' @return A list of tibbles containing static and historical information
#' @export
read_results_detailed_preload_house <- function(x) {
  # Read the XML document
  xml <- xml2::read_xml(x)

  # Cache the extracted namespaces for use in XPath searches
  ns <- xml2::xml_ns(xml)

  # Start by finding the nodeset containing each House contest in the document
  contests_xml <- xml2::xml_find_all(
    xml,
    "/d1:MediaFeed/d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest",
    ns = ns
  )

  # Create a tibble containing the contest (division) data
  contests <-
    tibble::tibble(
      contest_id = contests_xml %>%
        xml2::xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml2::xml_attr("Id"),
      contest_name = contests_xml %>%
        xml2::xml_find_first(
          "./eml:ContestIdentifier/eml:ContestName",
          ns = ns
        ) %>%
        xml2::xml_text(),
      district_code = contests_xml %>%
        xml2::xml_find_first("./d1:PollingDistrictIdentifier", ns = ns) %>%
        xml2::xml_attr("ShortCode"),
      state = contests_xml %>%
        xml2::xml_find_first(
          "./d1:PollingDistrictIdentifier/d1:StateIdentifier",
          ns = ns
        ) %>%
        xml2::xml_attr("Id"),
      enrolment_current = contests_xml %>%
        xml2::xml_find_first("./d1:Enrolment", ns = ns) %>%
        xml2::xml_attr("CloseOfRolls"),
      enrolment_historic = contests_xml %>%
        xml2::xml_find_first("./d1:Enrolment", ns = ns) %>%
        xml2::xml_attr("Historic")
    )

  # Convert the contest data to appropriate types
  contests <-
    contests %>%
    readr::type_convert(
      col_types = readr::cols(
        contest_id = readr::col_integer(),
        contest_name = readr::col_character(),
        district_code = readr::col_character(),
        state = readr::col_character(),
        enrolment_current = readr::col_integer(),
        enrolment_historic = readr::col_integer()
      )
    )

  # Create a tibble containing the polling place data
  pollingplaces <-
    tibble::tibble(
      contest_id = contests_xml %>%
        xml2::xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml2::xml_attr("Id"),
      pp_xml = contests_xml %>%
        purrr::map(~ xml2::xml_find_all(
          .,
          "./d1:PollingPlaces/d1:PollingPlace",
          ns = ns
        ))
    )

  # Unpack the polling place XML for each contest into tibbles
  pollingplaces <-
    pollingplaces %>%
    dplyr::mutate(
      pp_tbl = pp_xml %>%
        purrr::map(~ tibble::tibble(
          pollingplace_id = .x %>%
            xml2::xml_find_first("./d1:PollingPlaceIdentifier", ns = ns) %>%
            xml2::xml_attr("Id"),
          pollingplace_name = .x %>%
            xml2::xml_find_first("./d1:PollingPlaceIdentifier", ns = ns) %>%
            xml2::xml_attr("Name")
        ))
    )

  # Remove the polling place XML nodesets now that they've been unpacked
  pollingplaces <-
    pollingplaces %>%
    dplyr::select(-pp_xml)

  # Unnest the data so each row contains data for one polling place
  pollingplaces <-
    pollingplaces %>%
    tidyr::unnest()

  # Convert the polling place data to appropriate types
  pollingplaces <-
    pollingplaces %>%
    readr::type_convert(
      col_types = readr::cols(
        contest_id = readr::col_integer(),
        pollingplace_id = readr::col_integer(),
        pollingplace_name = readr::col_character()
      )
    )

  # Create a tibble containing the candidate data
  candidates <-
    tibble::tibble(
      contest_id = contests_xml %>%
        xml2::xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml2::xml_attr("Id"),
      candidates_xml = contests_xml %>%
        purrr::map(~ xml2::xml_find_all(., "./d1:FirstPreferences/*", ns = ns))
    )

  # Unpack the candidate XML for each contest into tibbles
  candidates <-
    candidates %>%
    dplyr::mutate(
      candidates_tbl = candidates_xml %>%
        purrr::map(~ tibble::tibble(
          candidate_type = .x %>%
            xml2::xml_name(),
          candidate_id = .x %>%
            xml2::xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
            xml2::xml_attr("Id"),
          candidate_name = .x %>%
            xml2::xml_find_first(
              "./eml:CandidateIdentifier/eml:CandidateName",
              ns = ns
            ) %>%
            xml2::xml_text()
        ))
    )

  # Remove the candidate XML nodesets now that they've been unpacked
  candidates <-
    candidates %>%
    dplyr::select(-candidates_xml)

  # Unnest the data so each row contains data for one candidate
  candidates <-
    candidates %>%
    tidyr::unnest()

  # Create a list to store and return all tibbles created from the XML
  output <-
    list(
      contests = contests,
      pollingplaces = pollingplaces,
      candidates = candidates
    )

  output
}
