#' Read the House votes from a Detailed Light results message
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
read_results_house <- function(x) {
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
  contests <- xml_find_all(
    xml,
    "/d1:MediaFeed/d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest",
    ns = ns
  )

  # Create a tibble for vote by polling place by parsing the relevant XML nodes
  results_by_pp <- tibble::tibble(
    feed_id = feed_id,
    contest_id = contests %>%
      xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
      xml_attr("Id"),
    pp_xml = contests %>%
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
      results_updated = .x %>%
        xml_attr("Updated"),
      fp_xml = .x %>%
        purrr::map(~ xml_find_all(
          .,
          "./d1:FirstPreferences/*",
          ns = ns
        ))
    )

  fp_by_pp_formula <-
    ~ tibble::tibble(
      candidate_type = .x %>%
        xml_name(),
      candidate_id = .x %>%
        xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
        xml_attr("Id"),
      votes = .x %>%
        xml_find_first("./d1:Votes", ns = ns) %>%
        xml_text()
    )

  # Unpack the polling place first preference XML for each contest into tibbles
  results_fp_by_pp <-
    results_by_pp %>%
    unpack_xml(.data$pp_xml, fp_pp_formula) %>%
    unpack_xml(.data$fp_xml, fp_by_pp_formula)

  # Convert the data to appropriate types
  results_fp_by_pp <-
    results_fp_by_pp %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
        contest_id = readr::col_integer(),
        pollingplace_id = readr::col_integer(),
        results_updated = readr::col_datetime(),
        candidate_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        votes = readr::col_integer()
      )
    )

  # Convert zero votes for polling places that haven't returned results to NA
  results_fp_by_pp <-
    replace_zeroes_with_na(
      results_fp_by_pp,
      .data$contest_id,
      .data$pollingplace_id
    )

  # Create a tibble for vote by type by parsing the relevant XML nodes
  results_fp_by_type <- tibble::tibble(
    feed_id = feed_id,
    contest_id = contests %>%
      xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
      xml_attr("Id"),
    results_updated = contests %>%
      xml_find_first("./d1:FirstPreferences", ns = ns) %>%
      xml_attr("Updated"),
    candidates_xml = contests %>%
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
      votes = .x %>%
        xml_text()
    )

  # Unpack the candidate XML for each contest into tibbles
  results_fp_by_type <-
    results_fp_by_type %>%
    unpack_xml(.data$candidates_xml, fp_candidates_formula) %>%
    unpack_xml(.data$fp_xml, fp_by_type_formula)

  # Convert the data to appropriate types
  results_fp_by_type <-
    results_fp_by_type %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
        contest_id = readr::col_integer(),
        results_updated = readr::col_datetime(),
        candidate_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        vote_type = readr::col_character(),
        votes = readr::col_integer()
      )
    )

  # Convert zero votes for vote types that haven't returned any results to NA
  results_fp_by_type <-
    replace_zeroes_with_na(
      results_fp_by_type,
      .data$contest_id,
      .data$vote_type
    )

  tcp_pp_formula <-
    ~ tibble::tibble(
      pollingplace_id = .x %>%
        xml_find_first("./d1:PollingPlaceIdentifier", ns = ns) %>%
        xml_attr("Id"),
      results_updated = .x %>%
        xml_attr("Updated"),
      tcp_xml = .x %>%
        purrr::map(~ xml_find_all(
          .,
          "./d1:TwoCandidatePreferred/*",
          ns = ns
        ))
    )

    tcp_by_pp_formula <-
      ~ tibble::tibble(
        candidate_type = .x %>%
          xml_name(),
        candidate_id = .x %>%
          xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
          xml_attr("Id"),
        votes = .x %>%
          xml_find_first("./d1:Votes", ns = ns) %>%
          xml_text()
      )

  # Unpack the polling place two candidate preferred XML for each contest into
  # tibbles
  results_tcp_by_pp <-
    results_by_pp %>%
    unpack_xml(.data$pp_xml, tcp_pp_formula) %>%
    unpack_xml(.data$tcp_xml, tcp_by_pp_formula)

  # Convert the data to appropriate types
  results_tcp_by_pp <-
    results_tcp_by_pp %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
        contest_id = readr::col_integer(),
        pollingplace_id = readr::col_integer(),
        results_updated = readr::col_datetime(),
        candidate_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        votes = readr::col_integer()
      )
    )

  # Convert zero votes for polling places that haven't returned results to NA
  results_tcp_by_pp <-
    replace_zeroes_with_na(
      results_tcp_by_pp,
      .data$contest_id,
      .data$pollingplace_id
    )

  # Create a tibble for TCP vote by type by parsing the relevant XML nodes
  results_tcp_by_type <- tibble::tibble(
    feed_id = feed_id,
    contest_id = contests %>%
      xml_find_first("./eml:ContestIdentifier", ns = ns) %>%
      xml_attr("Id"),
    results_updated = contests %>%
      xml_find_first("./d1:FirstPreferences", ns = ns) %>%
      xml_attr("Updated"),
    candidates_xml = contests %>%
      purrr::map(~ xml_find_all(., "./d1:TwoCandidatePreferred/*", ns = ns))
  )

  tcp_candidates_formula <-
    ~ tibble::tibble(
      candidate_type = .x %>%
        xml_name(),
      candidate_id = .x %>%
        xml_find_first("./eml:CandidateIdentifier", ns = ns) %>%
        xml_attr("Id"),
      tcp_xml = .x %>%
        purrr::map(~ xml_find_all(
          .,
          "./d1:VotesByType/*",
          ns = ns
        ))
    )

  tcp_by_type_formula <-
    ~ tibble::tibble(
      vote_type = .x %>%
        xml_attr("Type"),
      votes = .x %>%
        xml_text()
    )

  # Unpack the candidate XML for each contest into tibbles
  results_tcp_by_type <-
    results_tcp_by_type %>%
    unpack_xml(.data$candidates_xml, tcp_candidates_formula) %>%
    unpack_xml(.data$tcp_xml, tcp_by_type_formula)

  # Convert the data to appropriate types
  results_tcp_by_type <-
    results_tcp_by_type %>%
    readr::type_convert(
      col_types = readr::cols(
        feed_id = readr::col_character(),
        contest_id = readr::col_integer(),
        results_updated = readr::col_datetime(),
        candidate_type = readr::col_character(),
        candidate_id = readr::col_integer(),
        vote_type = readr::col_character(),
        votes = readr::col_integer()
      )
    )

  # Convert zero votes for vote types that haven't returned any results to NA
  results_tcp_by_type <-
    replace_zeroes_with_na(
      results_tcp_by_type,
      .data$contest_id,
      .data$vote_type
    )

  # Create a list to store and return all tibbles created from the XML
  output <-
    list(
      feeds = feeds,
      results_fp_by_pp = results_fp_by_pp,
      results_fp_by_type = results_fp_by_type,
      results_tcp_by_pp = results_tcp_by_pp,
      results_tcp_by_type = results_tcp_by_type
    )

  output
}
