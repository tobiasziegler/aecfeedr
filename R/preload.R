#' Read the House information from a Detailed Preload Results XML message
#'
#' The AEC's preload data feed provides the details about election contests,
#' candidates, and polling places, along with the historical (previous
#' election) results. This function processes the detailed preload message and
#' produces a set of tibbles that can be used to produce pre-election content
#' and can be combined with the light version of live results, which only
#' identifies contests, candidates and polling places by their ID numbers.
#'
#' @param x A string, connection or raw vector to be processed by `read_xml`
#'
#' @return A list of tibbles containing static and historical information
#' @export
read_preload_house_fp <- function(x) {
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
            xml2::xml_text(),
          affiliation_id = .x %>%
            xml2::xml_find_first("./eml:AffiliationIdentifier", ns = ns) %>%
            xml2::xml_attr("Id"),
          affiliation_code = .x %>%
            xml2::xml_find_first("./eml:AffiliationIdentifier", ns = ns) %>%
            xml2::xml_attr("ShortCode"),
          affiliation_name = .x %>%
            xml2::xml_find_first(
              "./eml:AffiliationIdentifier/eml:RegisteredName",
              ns = ns
            ) %>%
            xml2::xml_text(),
          ballot_position = .x %>%
            xml2::xml_find_first("./d1:BallotPosition") %>%
            xml2::xml_text(),
          elected_current = .x %>%
            xml2::xml_find_first("./d1:Elected") %>%
            xml2::xml_text(),
          elected_historic = .x %>%
            xml2::xml_find_first("./d1:Elected") %>%
            xml2::xml_attr("Historic"),
          incumbent = .x %>%
            xml2::xml_find_first("./d1:Incumbent") %>%
            xml2::xml_text(),
          incumbent_notional = .x %>%
            xml2::xml_find_first("./d1:Incumbent") %>%
            xml2::xml_attr("Notional")
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

  # Convert the candidate data to appropriate types
  candidates <-
    candidates %>%
    readr::type_convert(
      col_types = readr::cols(
        contest_id = readr::col_integer(),
        candidate_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        candidate_name = readr::col_character(),
        affiliation_id = readr::col_integer(),
        affiliation_code = readr::col_character(),
        affiliation_name = readr::col_character(),
        ballot_position = readr::col_integer(),
        elected_current = readr::col_logical(),
        elected_historic = readr::col_logical(),
        incumbent = readr::col_logical(),
        incumbent_notional = col_logical()
      )
    )

  # Create a tibble containing the historic vote by polling place data
  results_fp_by_pp_historic <-
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
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
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
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
    dplyr::select(-pp_xml)

  # Unnest the data so each row contains results for one polling place
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
    tidyr::unnest()

  # Unpack the first preferences XML for each polling place into tibbles
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
    dplyr::mutate(
      firstpreferences = fp_xml %>%
        purrr::map(~ tibble::tibble(
          vote_type = .x %>%
            xml2::xml_name(),
          candidate_id = .x %>%
            xml2::xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
            xml2::xml_attr("Id"),
          votes_historic = .x %>%
            xml2::xml_find_first("./d1:Votes", ns = ns) %>%
            xml2::xml_attr("Historic")
        ))
    )

  # Remove the first preferences XML nodesets now that they've been unpacked
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
    dplyr::select(-fp_xml)

  # Unnest the data so each row contains one first preference count
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
    tidyr::unnest()

  # Convert the data to appropriate types
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
    readr::type_convert(
      col_types = readr::cols(
        contest_id = readr::col_integer(),
        pollingplace_id = readr::col_integer(),
        vote_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        votes_historic = readr::col_integer()
      )
    )

  # Create a tibble containing the historic vote by type data
  results_fp_by_type_historic <-
    tibble::tibble(
      contest_id = contests_xml %>%
        xml2::xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml2::xml_attr("Id"),
      candidates_xml = contests_xml %>%
        purrr::map(~ xml2::xml_find_all(., "./d1:FirstPreferences/*", ns = ns))
    )

  # Unpack the candidate XML for each contest into tibbles
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    dplyr::mutate(
      candidates_tbl = candidates_xml %>%
        purrr::map(~ tibble::tibble(
          candidate_type = .x %>%
            xml2::xml_name(),
          candidate_id = .x %>%
            xml2::xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
            xml2::xml_attr("Id"),
          fp_xml = .x %>%
            purrr::map(~ xml2::xml_find_all(
              .,
              "./d1:VotesByType/*",
              ns = ns
            ))
        ))
    )

  # Remove the candidate XML nodesets now that they've been unpacked
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    dplyr::select(-candidates_xml)

  # Unnest the data so each row contains one candidate
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    tidyr::unnest()

  # Unpack the first preferences XML for each candidate into tibbles
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    dplyr::mutate(
      firstpreferences = fp_xml %>%
        purrr::map(~ tibble::tibble(
          vote_type = .x %>%
            xml2::xml_attr("Type"),
          votes_historic = .x %>%
            xml2::xml_attr("Historic")
        ))
    )

  # Remove the first preference XML nodesets now that they've been unpacked
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    dplyr::select(-fp_xml)

  # Unnest the data so each row contains one first preference count
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    tidyr::unnest()

  # Convert the data to appropriate types
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    readr::type_convert(
      col_types = readr::cols(
        contest_id = readr::col_integer(),
        candidate_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        vote_type = readr::col_character(),
        votes_historic = readr::col_integer()
      )
    )

  # Create a list to store and return all tibbles created from the XML
  output <-
    list(
      contests = contests,
      pollingplaces = pollingplaces,
      candidates = candidates,
      results_fp_by_pp_historic = results_fp_by_pp_historic,
      results_fp_by_type_historic = results_fp_by_type_historic
    )

  output
}
