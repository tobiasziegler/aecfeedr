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
  contests_xml <- xml_find_all(
    xml,
    "/d1:MediaFeed/d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest",
    ns = ns
  )

  # Create a tibble containing the contest (division) data
  contests <-
    tibble::tibble(
      feed_id = feed_id,
      contest_id =
        xml_attr(
          xml_find_first(
            contests_xml,
            "./eml:ContestIdentifier",
            ns = ns
          ),
          "Id"
        ),
      contest_name =
        xml_text(
          xml_find_first(
            contests_xml,
            "./eml:ContestIdentifier/eml:ContestName",
            ns = ns
          )
        ),
      district_code =
        xml_attr(
          xml_find_first(
            contests_xml,
            "./d1:PollingDistrictIdentifier",
            ns = ns
          ),
          "ShortCode"
        ),
      state =
        xml_attr(
          xml_find_first(
            contests_xml,
            "./d1:PollingDistrictIdentifier/d1:StateIdentifier",
            ns = ns
          ),
          "Id"
        ),
      enrolment_current =
        xml_attr(
          xml_find_first(
            contests_xml,
            "./d1:Enrolment",
            ns = ns
          ),
          "CloseOfRolls"
        ),
      enrolment_historic =
        xml_attr(
          xml_find_first(
            contests_xml,
            "./d1:Enrolment",
            ns = ns
          ),
          "Historic"
        )
    )

  # Convert the contest data to appropriate types
  contests <-
    readr::type_convert(
      contests,
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
      contest_id =
        xml_attr(
          xml_find_first(
            contests_xml,
            "./eml:ContestIdentifier",
            ns = ns
          ),
          "Id"
        ),
      pp_xml =
        purrr::map(
          contests_xml,
          ~ xml_find_all(., "./d1:PollingPlaces/d1:PollingPlace", ns = ns)
        )
    )

  pp_formula <-
    ~ tibble::tibble(
      pollingplace_id =
        xml_attr(
          xml_find_first(.x, "./d1:PollingPlaceIdentifier", ns = ns),
          "Id"
        ),
      pollingplace_name =
        xml_attr(
          xml_find_first(.x, "./d1:PollingPlaceIdentifier", ns = ns),
          "Name"
        )
    )

  # Unpack the polling place XML for each contest into tibbles
  pollingplaces <- unpack_xml(pollingplaces, .data$pp_xml, pp_formula)

  # Convert the polling place data to appropriate types
  pollingplaces <-
    readr::type_convert(
      pollingplaces,
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
      contest_id =
        xml_attr(
          xml_find_first(
            contests_xml,
            "./eml:ContestIdentifier",
            ns = ns
          ),
          "Id"
        ),
      candidates_xml =
        purrr::map(
          contests_xml,
          ~ xml_find_all(., "./d1:FirstPreferences/*", ns = ns)
        )
    )

  candidates_formula <-
    ~ tibble::tibble(
      candidate_type =
        xml_name(.x),
      candidate_id =
        xml_attr(
          xml_find_first(
            .x,
            "./eml:CandidateIdentifier",
            ns = ns
          ),
          "Id"
        ),
      candidate_name =
        xml_text(
          xml_find_first(
            .x,
            "./eml:CandidateIdentifier/eml:CandidateName",
            ns = ns
          )
        ),
      affiliation_id =
        xml_attr(
          xml_find_first(
            .x,
            "./eml:AffiliationIdentifier",
            ns = ns
          ),
          "Id"
        ),
      affiliation_code =
        xml_attr(
          xml_find_first(
            .x,
            "./eml:AffiliationIdentifier",
            ns = ns
          ),
          "ShortCode"
        ),
      affiliation_name =
        xml_text(
          xml_find_first(
            .x,
            "./eml:AffiliationIdentifier/eml:RegisteredName",
            ns = ns
          )
        ),
      ballot_position =
        xml_text(
          xml_find_first(
            .x,
            "./d1:BallotPosition",
            ns = ns
          )
        ),
      elected_current =
        xml_text(
          xml_find_first(
            .x,
            "./d1:Elected",
            ns = ns
          )
        ),
      elected_historic =
        xml_attr(
          xml_find_first(
            .x,
            "./d1:Elected",
            ns = ns
          ),
          "Historic"
        ),
      incumbent =
        xml_text(
          xml_find_first(
            .x,
            "./d1:Incumbent",
            ns = ns
          )
        ),
      incumbent_notional =
        xml_attr(
          xml_find_first(
            .x,
            "./d1:Incumbent",
            ns = ns
          ),
          "Notional"
        )
    )

  # Unpack the candidate XML for each contest into tibbles
  candidates <-
    unpack_xml(candidates, .data$candidates_xml, candidates_formula)

  # Convert the candidate data to appropriate types
  candidates <-
    readr::type_convert(
      candidates,
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
      contest_id =
        xml_attr(
          xml_find_first(
            contests_xml,
            "./eml:ContestIdentifier",
            ns = ns
          ),
          "Id"
        ),
      pp_xml =
        purrr::map(
          contests_xml,
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
          xml_find_first(
            .x,
            "./d1:PollingPlaceIdentifier",
            ns = ns
          ),
          "Id"
        ),
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
      vote_type =
        xml_name(.x),
      candidate_id =
        xml_attr(
          xml_find_first(.x, "./eml:CandidateIdentifier", ns = ns),
          "Id"
        ),
      votes_historic =
        xml_attr(
          xml_find_first(.x, "./d1:Votes", ns = ns),
          "Historic"
        )
    )

  # Unpack the polling place XML for each contest into tibbles
  results_fp_by_pp_historic <-
    unpack_xml(results_fp_by_pp_historic, .data$pp_xml, fp_pp_formula)

  results_fp_by_pp_historic <-
    unpack_xml(results_fp_by_pp_historic, .data$fp_xml, fp_by_pp_formula)

  # Convert the data to appropriate types
  results_fp_by_pp_historic <-
    readr::type_convert(
      results_fp_by_pp_historic,
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
      contest_id =
        xml_attr(
          xml_find_first(contests_xml, "./eml:ContestIdentifier", ns = ns),
          "Id"
        ),
      candidates_xml =
        purrr::map(
          contests_xml,
          ~ xml_find_all(., "./d1:FirstPreferences/*", ns = ns)
        )
    )

  fp_candidates_formula <-
    ~ tibble::tibble(
      candidate_type = xml_name(.x),
      candidate_id =
        xml_attr(
          xml_find_first(
            .x,
            "./eml:CandidateIdentifier",
            ns = ns
          ),
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
      votes_historic = xml_attr(.x, "Historic")
    )

  # Unpack the candidate XML for each contest into tibbles
  results_fp_by_type_historic <-
    unpack_xml(
      results_fp_by_type_historic,
      .data$candidates_xml,
      fp_candidates_formula
    )

  results_fp_by_type_historic <-
    unpack_xml(
      results_fp_by_type_historic,
      .data$fp_xml,
      fp_by_type_formula
    )

  # Convert the data to appropriate types
  results_fp_by_type_historic <-
    readr::type_convert(
      results_fp_by_type_historic,
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
