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

  # Start by finding the nodeset containing each House contest in the document
  contests <- xml2::xml_find_all(xml, "//d1:House/d1:Contests/d1:Contest")

  # Create a tibble by parsing the relevant XML nodes
  results <- tibble::tibble(
    contest_id = contests %>%
      xml2::xml_find_first(".//eml:ContestIdentifier") %>%
      xml2::xml_attr("Id"),
    pp_xml = contests %>%
      purrr::map(~ xml2::xml_find_all(
        .,
        ".//d1:PollingPlaces/d1:PollingPlace"
      ))
  )

  # Return the tibble containing the results
  results
}
