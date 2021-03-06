% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results.R
\name{read_results_house}
\alias{read_results_house}
\title{Read the House votes from a Detailed Light results message}
\usage{
read_results_house(x)
}
\arguments{
\item{x}{A string, connection or raw vector to be processed by \code{read_xml}}
}
\value{
A list of tibbles containing results by polling place and vote type
}
\description{
The AEC's Detailed Light results feed provides updates on election results
with counts at the polling place level. This function processes a message
from this results feed and produces tibbles that contain the vote counts by
polling place and vote type. Contest, candidates and polling places in the
data from this function are only identified by their ID numbers, with full
details available by combining the data with information from the preload
feed.
}
