#' Read the first preference House votes from a Detailed Light results message
#'
#' The AEC's Detailed Light results feed provides updates on election results
#' with counts at the polling place level. This function processes a message
#' from this results feed and produces tibbles that contain the vote counts by
#' polling place and vote type. Contest, candidates and polling places in the
#' data from this function are only identified by their ID numbers, with full
#' details available by combining the data with information from the preload
#' feed.
#'
#' @param x A string, connection or raw vector to be processed by `read_xml`
#'
#' @return A list of tibbles containing results by polling place and vote type
#' @export
read_results_house_fp <- function(x) {
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

  # Create a tibble for vote by polling place by parsing the relevant XML nodes
  results_fp_by_pp <- tibble::tibble(
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
  results_fp_by_pp <-
    results_fp_by_pp %>%
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
  results_fp_by_pp <-
    results_fp_by_pp %>%
    dplyr::select(-pp_xml)

  # Unnest the data so each row contains results for one polling place
  results_fp_by_pp <-
    results_fp_by_pp %>%
    tidyr::unnest()

  # Unpack the first preferences XML for each polling place into tibbles
  results_fp_by_pp <-
    results_fp_by_pp %>%
    dplyr::mutate(
      firstpreferences = fp_xml %>%
        purrr::map(~ tibble::tibble(
          candidate_type = .x %>%
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
  results_fp_by_pp <-
    results_fp_by_pp %>%
    dplyr::select(-fp_xml)

  # Unnest the data so each row contains one first preference count
  results_fp_by_pp <-
    results_fp_by_pp %>%
    tidyr::unnest()

  # Convert the data to appropriate types
  results_fp_by_pp <-
    results_fp_by_pp %>%
    readr::type_convert(
      col_types = readr::cols(
        contest_id = readr::col_integer(),
        pollingplace_id = readr::col_integer(),
        candidate_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        votes = readr::col_integer()
      )
    )

  # Create a tibble for vote by type by parsing the relevant XML nodes
  results_fp_by_type <- tibble::tibble(
    contest_id = contests %>%
      xml2::xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
      xml2::xml_attr("Id"),
    candidates_xml = contests %>%
      purrr::map(~ xml2::xml_find_all(., "./d1:FirstPreferences/*", ns = ns))
  )

  # Unpack the candidate XML for each contest into tibbles
  results_fp_by_type <-
    results_fp_by_type %>%
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
  results_fp_by_type <-
    results_fp_by_type %>%
    dplyr::select(-candidates_xml)

  # Unnest the data so each row contains one candidate
  results_fp_by_type <-
    results_fp_by_type %>%
    tidyr::unnest()

  # Unpack the first preferences XML for each candidate into tibbles
  results_fp_by_type <-
    results_fp_by_type %>%
    dplyr::mutate(
      firstpreferences = fp_xml %>%
        purrr::map(~ tibble::tibble(
          vote_type = .x %>%
            xml2::xml_attr("Type"),
          votes = .x %>%
            xml2::xml_text()
        ))
    )

  # Remove the first preference XML nodesets now that they've been unpacked
  results_fp_by_type <-
    results_fp_by_type %>%
    dplyr::select(-fp_xml)

  # Unnest the data so each row contains one first preference count
  results_fp_by_type <-
    results_fp_by_type %>%
    tidyr::unnest()

  # Convert the data to appropriate types
  results_fp_by_type <-
    results_fp_by_type %>%
    readr::type_convert(
      col_types = readr::cols(
        contest_id = readr::col_integer(),
        candidate_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        vote_type = readr::col_character(),
        votes = readr::col_integer()
      )
    )

  # Create a list to store and return all tibbles created from the XML
  output <-
    list(
      results_fp_by_pp = results_fp_by_pp,
      results_fp_by_type = results_fp_by_type
    )

  output
}
