# aecfeedr (development version)

- Added `xml_find_attr` and `xml_find_text` functions that extract attributes or text from the first node matching a specified xpath.

- `feed_get_url` uses default values and argument matching to identify invalid granularity and verbosity values.

- Refactored XML processing functions for (somewhat) reduced repetition and improved performance.

- Updated the minimum required R version to 3.6.

- Switched from Travis CI to GitHub Actions for package checking.

- `read_results_house` replaces `read_results_house_fp` and now processes two candidate preferred vote counts as well as first preferences.

- When no results have been returned for a given polling place or vote type, the zero figures from the AEC feed will be replaced with NAs.

# aecfeedr 0.1.0

- Initial version used for live testing during the Wentworth by-election on 20 October 2018.

- Message processing functions only handle the Detailed Preload and Detailed Light feeds, and only extract first preference data.
