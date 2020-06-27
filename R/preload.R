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
#'
#' @export
read_preload_house_fp <- function(x) {
  # Read the XML document
  xml <- read_xml(x)

  # Cache the extracted namespaces for use in XPath searches
  ns <- xml_ns(xml)

  # Cache the feed ID for inclusion in data tibbles
  feed_id <- xml %>%
    xml_find_first("/d1:MediaFeed", ns = ns) %>%
    xml_attr("Id")

  # Create a one-row tibble with feed details
  feeds <- tibble::tibble(
    feed_id = feed_id,
    feed_created = xml %>%
      xml_find_first("/d1:MediaFeed", ns = ns) %>%
      xml_attr("Created"),
    feed_granularity = xml %>%
      xml_find_first("/d1:MediaFeed/d1:Results", ns = ns) %>%
      xml_attr("Granularity"),
    feed_verbosity = xml %>%
      xml_find_first("/d1:MediaFeed/d1:Results", ns = ns) %>%
      xml_attr("Verbosity"),
    feed_phase = xml %>%
      xml_find_first("/d1:MediaFeed/d1:Results", ns = ns) %>%
      xml_attr("Phase"),
    feed_updated = xml %>%
      xml_find_first("/d1:MediaFeed/d1:Results", ns = ns) %>%
      xml_attr("Updated")
  )

  # Convert the data to appropriate types
  feeds <-
    feeds %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
        feed_created = readr::col_datetime(),
        feed_granularity = readr::col_character(),
        feed_verbosity = readr::col_character(),
        feed_updated = readr::col_datetime(),
        feed_phase = readr::col_character()
      )
    )

  # Start by finding the nodeset containing each House contest in the document
  contests_xml <- xml_find_all(
    xml,
    "/d1:MediaFeed/d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest",
    ns = ns
  )

  # Create a tibble containing the contest (division) data
  contests <-
    tibble::tibble(
      feed_id = feed_id,
      contest_id = contests_xml %>%
        xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml_attr("Id"),
      contest_name = contests_xml %>%
        xml_find_first(
          "./eml:ContestIdentifier/eml:ContestName",
          ns = ns
        ) %>%
        xml_text(),
      district_code = contests_xml %>%
        xml_find_first("./d1:PollingDistrictIdentifier", ns = ns) %>%
        xml_attr("ShortCode"),
      state = contests_xml %>%
        xml_find_first(
          "./d1:PollingDistrictIdentifier/d1:StateIdentifier",
          ns = ns
        ) %>%
        xml_attr("Id"),
      enrolment_current = contests_xml %>%
        xml_find_first("./d1:Enrolment", ns = ns) %>%
        xml_attr("CloseOfRolls"),
      enrolment_historic = contests_xml %>%
        xml_find_first("./d1:Enrolment", ns = ns) %>%
        xml_attr("Historic")
    )

  # Convert the contest data to appropriate types
  contests <-
    contests %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
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
      feed_id = feed_id,
      contest_id = contests_xml %>%
        xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml_attr("Id"),
      pp_xml = contests_xml %>%
        purrr::map(~ xml_find_all(
          .,
          "./d1:PollingPlaces/d1:PollingPlace",
          ns = ns
        ))
    )

  pp_formula <-
    ~ tibble::tibble(
      pollingplace_id = .x %>%
        xml_find_first("./d1:PollingPlaceIdentifier", ns = ns) %>%
        xml_attr("Id"),
      pollingplace_name = .x %>%
        xml_find_first("./d1:PollingPlaceIdentifier", ns = ns) %>%
        xml_attr("Name")
    )

  # Unpack the polling place XML for each contest into tibbles
  pollingplaces <-
    pollingplaces %>%
    unpack_xml(.data$pp_xml, pp_formula)

  # Convert the polling place data to appropriate types
  pollingplaces <-
    pollingplaces %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
        contest_id = readr::col_integer(),
        pollingplace_id = readr::col_integer(),
        pollingplace_name = readr::col_character()
      )
    )

  # Create a tibble containing the candidate data
  candidates <-
    tibble::tibble(
      feed_id = feed_id,
      contest_id = contests_xml %>%
        xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml_attr("Id"),
      candidates_xml = contests_xml %>%
        purrr::map(~ xml_find_all(., "./d1:FirstPreferences/*", ns = ns))
    )

  candidates_formula <-
    ~ tibble::tibble(
      candidate_type = .x %>%
        xml_name(),
      candidate_id = .x %>%
        xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
        xml_attr("Id"),
      candidate_name = .x %>%
        xml_find_first(
          "./eml:CandidateIdentifier/eml:CandidateName",
          ns = ns
        ) %>%
        xml_text(),
      affiliation_id = .x %>%
        xml_find_first("./eml:AffiliationIdentifier", ns = ns) %>%
        xml_attr("Id"),
      affiliation_code = .x %>%
        xml_find_first("./eml:AffiliationIdentifier", ns = ns) %>%
        xml_attr("ShortCode"),
      affiliation_name = .x %>%
        xml_find_first(
          "./eml:AffiliationIdentifier/eml:RegisteredName",
          ns = ns
        ) %>%
        xml_text(),
      ballot_position = .x %>%
        xml_find_first("./d1:BallotPosition") %>%
        xml_text(),
      elected_current = .x %>%
        xml_find_first("./d1:Elected") %>%
        xml_text(),
      elected_historic = .x %>%
        xml_find_first("./d1:Elected") %>%
        xml_attr("Historic"),
      incumbent = .x %>%
        xml_find_first("./d1:Incumbent") %>%
        xml_text(),
      incumbent_notional = .x %>%
        xml_find_first("./d1:Incumbent") %>%
        xml_attr("Notional")
    )

  # Unpack the candidate XML for each contest into tibbles
  candidates <-
    candidates %>%
    unpack_xml(.data$candidates_xml, candidates_formula)

  # Convert the candidate data to appropriate types
  candidates <-
    candidates %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
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
        incumbent_notional = readr::col_logical()
      )
    )

  # Create a tibble containing the historic vote by polling place data
  results_fp_by_pp_historic <-
    tibble::tibble(
      feed_id = feed_id,
      contest_id = contests_xml %>%
        xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml_attr("Id"),
      pp_xml = contests_xml %>%
        purrr::map(~ xml_find_all(
          .,
          "./d1:PollingPlaces/d1:PollingPlace",
          ns = ns
        ))
    )

  fp_pp_formula <-
    ~ tibble::tibble(
      pollingplace_id = .x %>%
        xml_find_first("./d1:PollingPlaceIdentifier", ns = ns) %>%
        xml_attr("Id"),
      fp_xml = .x %>%
        purrr::map(~ xml_find_all(
          .,
          "./d1:FirstPreferences/*",
          ns = ns
        ))
    )

  fp_by_pp_formula <-
    ~ tibble::tibble(
      vote_type = .x %>%
        xml_name(),
      candidate_id = .x %>%
        xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
        xml_attr("Id"),
      votes_historic = .x %>%
        xml_find_first("./d1:Votes", ns = ns) %>%
        xml_attr("Historic")
    )

  # Unpack the polling place XML for each contest into tibbles
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
    unpack_xml(.data$pp_xml, fp_pp_formula) %>%
    unpack_xml(.data$fp_xml, fp_by_pp_formula)

  # Convert the data to appropriate types
  results_fp_by_pp_historic <-
    results_fp_by_pp_historic %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
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
      feed_id = feed_id,
      contest_id = contests_xml %>%
        xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
        xml_attr("Id"),
      candidates_xml = contests_xml %>%
        purrr::map(~ xml_find_all(., "./d1:FirstPreferences/*", ns = ns))
    )

  fp_candidates_formula <-
    ~ tibble::tibble(
      candidate_type = .x %>%
        xml_name(),
      candidate_id = .x %>%
        xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
        xml_attr("Id"),
      fp_xml = .x %>%
        purrr::map(~ xml_find_all(
          .,
          "./d1:VotesByType/*",
          ns = ns
        ))
    )

  fp_by_type_formula <-
    ~ tibble::tibble(
      vote_type = .x %>%
        xml_attr("Type"),
      votes_historic = .x %>%
        xml_attr("Historic")
    )

  # Unpack the candidate XML for each contest into tibbles
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    unpack_xml(.data$candidates_xml, fp_candidates_formula) %>%
    unpack_xml(.data$fp_xml, fp_by_type_formula)

  # Convert the data to appropriate types
  results_fp_by_type_historic <-
    results_fp_by_type_historic %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
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
      feeds = feeds,
      contests = contests,
      pollingplaces = pollingplaces,
      candidates = candidates,
      results_fp_by_pp_historic = results_fp_by_pp_historic,
      results_fp_by_type_historic = results_fp_by_type_historic
    )

  output
}
