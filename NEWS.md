# aecfeedr *development version*

- `read_results_house` replaces `read_results_house_fp` and now processes two candidate preferred vote counts as well as first preferences.

- When no results have been returned for a given polling place or vote type, the zero figures from the AEC feed will be replaced with NAs.

# aecfeedr 0.1.0

- Initial version used for live testing during the Wentworth by-election on 20 October 2018.

- Message processing functions only handle the Detailed Preload and Detailed Light feeds, and only extract first preference data.
