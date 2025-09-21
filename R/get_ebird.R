#' Retrieve eBird Data
#'
#' Filters and retrieves data from an eBird GeoDatabase file (.gdb) using a SQL query.
#'
#' @param path Character string specifying the file path to the eBird GeoDatabase file (.gdb).
#'   If `NULL`, uses the default path from `external_files$ebird_data$path`.
#' @param species Character vector of species names to filter (e.g., "Snow Goose").
#'   If `NULL`, the query will not filter by species.
#' @param year Integer vector of years to filter (e.g., `2012:2014`).
#'   If `NULL`, the query will not filter by year.
#' @param month Integer vector of months to filter (e.g., `4:5`). If `NULL`, the query
#'   will filter by year only.
#' @param extent An `sf` or `terra` spatial object defining the spatial extent to filter data.
#'   If `NULL`, the query will not apply spatial filtering.
#' @param ... Additional arguments passed to the SQL query construction function
#'   (`compose_query`). This allows for further customization of query conditions.
#'
#' @return A `SpatVector` object containing the filtered eBird data.
#'
#' @examples
#'\dontrun{
#' # Example: Retrieve data for Snow Goose in April and May of 2012-2014
#' get_ebird(
#'   path = "path/to/ebird.gdb",
#'   species = "Snow Goose",
#'   year = 2012:2014,
#'   month = 4:5
#' )
#' 
#' # Using default path from external_files
#' get_ebird(species = "Snow Goose", year = 2020)
#' }
#'
#' @export
get_ebird <- function(path = NULL, species = NULL, year = NULL, month = NULL, extent = NULL, ...) {
  
  if (is.null(path)) {
    path <- external_files$ebird_data$path
  }
  
  if (!file.exists(path)) {
    stop("eBird file not found at: ", path)
  }
  
  gdb_prox <- terra::vect(path, proxy = TRUE)
  q <- compose_query(species, year, month, ...)
  terra::query(gdb_prox, where = q, extent = extent)
}

# Internal function: Compose SQL Query  
compose_query <- function(species = NULL, year = NULL, month = NULL, var_time = "OBSERVATION_DATE", var_species = "COMMON_NAME", extent = NULL) {
  time_q <- NULL

  # Generate time query if year or month is provided
  if (!is.null(year)) {
    if (!is.null(month)) {
      time_grd <- expand.grid(year, sprintf("%02d", month)) |> setNames(c("year", "month"))
      time_q <- paste(time_grd$year, time_grd$month, "%", sep = "-")
    } else {
      time_q <- paste(year, "%", sep = "-")
    }
  }
  
  # Build time condition
  if (!is.null(time_q)) {
    time_condition <- glue::glue_collapse(
      glue::glue("{var_time} LIKE {paste0(\"'\", time_q, \"'\")}"),
      sep = " OR "
    )
  } else {
    time_condition <- NULL
  }

  # Build species condition
  if (!is.null(species)) {
    species <- paste0("'", species, "'", collapse = ", ")
    species_condition <- glue::glue("{var_species} IN ({species})")
  } else {
    species_condition <- NULL
  }

  # Combine conditions
  if (!is.null(time_condition) && !is.null(species_condition)) {
    query <- glue::glue("({time_condition}) AND {species_condition}")
  } else if (!is.null(time_condition)) {
    query <- time_condition
  } else if (!is.null(species_condition)) {
    query <- species_condition
  } else {
    query <- ""
  }

  return(query)
}
