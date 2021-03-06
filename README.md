# aecfeedr

<!-- badges: start -->
[![R build status](https://github.com/tobiasziegler/aecfeedr/workflows/R-CMD-check/badge.svg)](https://github.com/tobiasziegler/aecfeedr/actions)
[![Codecov test coverage](https://codecov.io/gh/tobiasziegler/aecfeedr/branch/develop/graph/badge.svg)](https://codecov.io/gh/tobiasziegler/aecfeedr?branch=develop)
<!-- badges: end -->

## Overview

`aecfeedr` is an R package for accessing and reading information from the Australian Electoral Commission's (AEC) FTP media feeds.

This package is under active early development and testing.

## Installation

At this stage the package isn't available on CRAN. You can get the latest release version of the package from GitHub:

``` r
devtools::install_github("tobiasziegler/aecfeedr")
```

Or you can get the development version:

``` r
devtools::install_github("tobiasziegler/aecfeedr", ref = "develop")
```

## Usage

At the moment the package only has functions to read and process the following feed files:

- **Preload data (Detailed granularity):** `read_preload_house_fp` takes the preload results message and returns a list containing tibbles (data frames) with details of contests (divisions), polling places, candidates, and historic (previous election) votes by polling place and vote type.

- **Results count (Light verbosity, Detailed granularity):** `read_results_house` takes the results messages from election night or subsequent counting and returns a list containing tibbles (data frames) with vote counts (first preferences and two candidate preferred) by polling place and vote type.

These two feeds allow you to produce detailed reporting, analysis and projection of the results based on vote counts at the polling place level.

There are also some helper functions available to construct the URLs for retrieving the messages from the AEC's FTP sites, to get a listing of files in a given directory, to download and unzip a feed file, and to write processed data to a database.

## Examples

The AEC website provides [an archive of feeds](https://results.aec.gov.au/) from past federal elections and by-elections back to 2007, which you can access for testing before using the package during a live election, or for historical analysis.
