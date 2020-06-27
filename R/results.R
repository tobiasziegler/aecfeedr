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
  feed_id <- xml_attr(xml_find_first(xml, "/d1:MediaFeed", ns = ns), "Id")

  # Create a one-row tibble with feed details
  feeds <- tibble::tibble(
    feed_id = feed_id,
    feed_created =
      xml_attr(
        xml_find_first(xml, "/d1:MediaFeed", ns = ns),
        "Created"
      ),
    feed_granularity =
      xml_attr(
        xml_find_first(xml, "/d1:MediaFeed/d1:Results", ns = ns),
        "Granularity"
      ),
    feed_verbosity =
      xml_attr(
        xml_find_first(xml, "/d1:MediaFeed/d1:Results", ns = ns),
        "Verbosity"
      ),
    feed_phase =
      xml_attr(
        xml_find_first(xml, "/d1:MediaFeed/d1:Results", ns = ns),
        "Phase"
      ),
    feed_updated =
      xml_attr(
        xml_find_first(xml, "/d1:MediaFeed/d1:Results", ns = ns),
        "Updated"
      )
  )

  # Convert the data to appropriate types
  feeds <-
    readr::type_convert(
      feeds,
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
    contest_id =
      xml_attr(
        xml_find_first(contests, "./eml:ContestIdentifier", ns = ns),
        "Id"
      ),
    pp_xml =
      purrr::map(
        contests,
        ~ xml_find_all(
          .,
          "./d1:PollingPlaces/d1:PollingPlace",
          ns = ns
        )
      )
  )

  fp_pp_formula <-
    ~ tibble::tibble(
      pollingplace_id =
        xml_attr(
          xml_find_first(.x, "./d1:PollingPlaceIdentifier", ns = ns),
          "Id"
        ),
      results_updated =
        xml_attr(.x, "Updated"),
      fp_xml =
        purrr::map(
          .x,
          ~ xml_find_all(
            .,
            "./d1:FirstPreferences/*",
            ns = ns
          )
        )
    )

  fp_by_pp_formula <-
    ~ tibble::tibble(
      candidate_type =
        xml_name(.x),
      candidate_id =
        xml_attr(
          xml_find_first(.x, "./eml:CandidateIdentifier", ns = ns),
          "Id"
        ),
      votes =
        xml_text(
          xml_find_first(.x, "./d1:Votes", ns = ns)
        )
    )

  # Unpack the polling place first preference XML for each contest into tibbles
  results_fp_by_pp <-
    unpack_xml(results_by_pp, .data$pp_xml, fp_pp_formula)
  results_fp_by_pp <-
    unpack_xml(results_fp_by_pp, .data$fp_xml, fp_by_pp_formula)

  # Convert the data to appropriate types
  results_fp_by_pp <-
    readr::type_convert(
      results_fp_by_pp,
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
    contest_id =
      xml_attr(
        xml_find_first(contests, "./eml:ContestIdentifier", ns = ns),
        "Id"
      ),
    results_updated =
      xml_attr(
        xml_find_first(contests, "./d1:FirstPreferences", ns = ns),
        "Updated"
      ),
    candidates_xml =
      purrr::map(
        contests,
        ~ xml_find_all(., "./d1:FirstPreferences/*", ns = ns)
      )
  )

  fp_candidates_formula <-
    ~ tibble::tibble(
      candidate_type =
        xml_name(.x),
      candidate_id =
        xml_attr(
          xml_find_first(.x, "./eml:CandidateIdentifier", ns = ns),
          "Id"
        ),
      fp_xml =
        purrr::map(
          .x,
          ~ xml_find_all(
            .,
            "./d1:VotesByType/*",
            ns = ns
          )
        )
    )

  fp_by_type_formula <-
    ~ tibble::tibble(
      vote_type = xml_attr(.x, "Type"),
      votes = xml_text(.x)
    )

  # Unpack the candidate XML for each contest into tibbles
  results_fp_by_type <-
    unpack_xml(results_fp_by_type, .data$candidates_xml, fp_candidates_formula)
  results_fp_by_type <-
    unpack_xml(results_fp_by_type, .data$fp_xml, fp_by_type_formula)

  # Convert the data to appropriate types
  results_fp_by_type <-
    readr::type_convert(
      results_fp_by_type,
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
      pollingplace_id =
        xml_attr(
          xml_find_first(.x, "./d1:PollingPlaceIdentifier", ns = ns),
          "Id"
        ),
      results_updated =
        xml_attr(.x, "Updated"),
      tcp_xml =
        purrr::map(
          .x,
          ~ xml_find_all(
            .,
            "./d1:TwoCandidatePreferred/*",
            ns = ns
          )
        )
    )

    tcp_by_pp_formula <-
      ~ tibble::tibble(
        candidate_type =
          xml_name(.x),
        candidate_id =
          xml_attr(
            xml_find_first(.x, "./eml:CandidateIdentifier", ns = ns),
            "Id"
          ),
        votes =
          xml_text(
            xml_find_first(.x, "./d1:Votes", ns = ns)
          )
      )

  # Unpack the polling place two candidate preferred XML for each contest into
  # tibbles
  results_tcp_by_pp <-
    unpack_xml(results_by_pp, .data$pp_xml, tcp_pp_formula)
  results_tcp_by_pp <-
    unpack_xml(results_tcp_by_pp, .data$tcp_xml, tcp_by_pp_formula)

  # Convert the data to appropriate types
  results_tcp_by_pp <-
    readr::type_convert(
      results_tcp_by_pp,
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
    contest_id =
      xml_attr(
        xml_find_first(contests, "./eml:ContestIdentifier", ns = ns),
        "Id"
      ),
    results_updated =
      xml_attr(
        xml_find_first(contests, "./d1:FirstPreferences", ns = ns),
        "Updated"
      ),
    candidates_xml =
      purrr::map(
        contests,
        ~ xml_find_all(., "./d1:TwoCandidatePreferred/*", ns = ns)
      )
  )

  tcp_candidates_formula <-
    ~ tibble::tibble(
      candidate_type =
        xml_name(.x),
      candidate_id =
        xml_attr(
          xml_find_first(.x, "./eml:CandidateIdentifier", ns = ns),
          "Id"
        ),
      tcp_xml =
        purrr::map(
          .x,
          ~ xml_find_all(
            .,
            "./d1:VotesByType/*",
            ns = ns
          )
        )
    )

  tcp_by_type_formula <-
    ~ tibble::tibble(
      vote_type = xml_attr(.x, "Type"),
      votes = xml_text(.x)
    )

  # Unpack the candidate XML for each contest into tibbles
  results_tcp_by_type <-
    unpack_xml(
      results_tcp_by_type,
      .data$candidates_xml,
      tcp_candidates_formula
    )
  results_tcp_by_type <-
    unpack_xml(results_tcp_by_type, .data$tcp_xml, tcp_by_type_formula)

  # Convert the data to appropriate types
  results_tcp_by_type <-
    readr::type_convert(
      results_tcp_by_type,
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
