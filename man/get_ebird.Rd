% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ebird.R
\name{get_ebird}
\alias{get_ebird}
\title{Retrieve eBird Data}
\usage{
get_ebird(path = NULL, species = NULL, year = NULL, month = NULL, ...)
}
\arguments{
\item{path}{Character string specifying the file path to the eBird GeoDatabase file (.gdb).}

\item{species}{Character vector of species names to filter (e.g., "Snow Goose").
If \code{NULL}, the query will not filter by species.}

\item{year}{Integer vector of years to filter (e.g., \code{2012:2014}).}

\item{month}{Integer vector of months to filter (e.g., \code{4:5}). If \code{NULL}, the query
will filter by year only.}

\item{...}{Additional arguments passed to \code{terra::query()}, such as \code{select} for selecting specific columns
or \code{limit} to limit the number of rows returned.}
}
\value{
A \code{SpatVector} object containing the filtered eBird data.
}
\description{
Filters and retrieves data from an eBird GeoDatabase file (.gdb) using a SQL query.
}
\examples{
\dontrun{
# Example: Retrieve data for Snow Goose in April and May of 2012-2014
get_ebird(
  path = "path/to/ebird.gdb",
  species = "Snow Goose",
  year = 2012:2014,
  month = 4:5
)

# Example: Retrieve only specific columns
get_ebird(
  path = "path/to/ebird.gdb",
  species = "Snow Goose",
  year = 2012,
  select = c("COMMON_NAME", "OBSERVATION_DATE")
)}
}
