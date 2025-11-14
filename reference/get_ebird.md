# Retrieve eBird Data

Filters and retrieves data from an eBird GeoDatabase file (.gdb) using a
SQL query.

## Usage

``` r
get_ebird(
  path = NULL,
  species = NULL,
  year = NULL,
  month = NULL,
  extent = NULL,
  ...
)
```

## Arguments

- path:

  Character string specifying the file path to the eBird GeoDatabase
  file (.gdb). If `NULL`, uses the default path from
  `external_files()$ebird_data$path`.

- species:

  Character vector of species names to filter (e.g., "Snow Goose"). If
  `NULL`, the query will not filter by species.

- year:

  Integer vector of years to filter (e.g., `2012:2014`). If `NULL`, the
  query will not filter by year.

- month:

  Integer vector of months to filter (e.g., `4:5`). If `NULL`, the query
  will filter by year only.

- extent:

  An `sf` or `terra` spatial object defining the spatial extent to
  filter data. If `NULL`, the query will not apply spatial filtering.

- ...:

  Additional arguments passed to the SQL query construction function
  (`compose_query`). This allows for further customization of query
  conditions.

## Value

A `SpatVector` object containing the filtered eBird data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Retrieve data for Snow Goose in April and May of 2012-2014
get_ebird(
  path = "path/to/ebird.gdb",
  species = "Snow Goose",
  year = 2012:2014,
  month = 4:5
)

# Using default path from external_files
get_ebird(species = "Snow Goose", year = 2020)
} # }
```
