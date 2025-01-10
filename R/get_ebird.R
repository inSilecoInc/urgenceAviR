#' Retrieve eBird Data
#'
#' Filters and retrieves data from an eBird GeoDatabase file (.gdb) using a SQL query.
#'
#' @param path Character string specifying the file path to the eBird GeoDatabase file (.gdb).
#' @param species Character vector of species names to filter (e.g., "Snow Goose").
#'   If `NULL`, the query will not filter by species.
#' @param year Integer vector of years to filter (e.g., `2012:2014`).
#' @param month Integer vector of months to filter (e.g., `4:5`). If `NULL`, the query
#'   will filter by year only.
#'
#' @return A `SpatVector` object containing the filtered eBird data.
#'
#' @examples
#' # Example: Retrieve data for Snow Goose in April and May of 2012-2014
#' get_ebird(
#'   path = "path/to/ebird.gdb",
#'   species = "Snow Goose",
#'   year = 2012:2014,
#'   month = 4:5
#' )
#' @export
get_ebird <- function(path = NULL, species = NULL, year = NULL, month = NULL) {
  gdb_prox <- terra::vect(path, proxy = TRUE)
  q <- compose_query(species, year, month)
  terra::query(gdb_prox, where = q)
}

# Internal function: Compose SQL Query
compose_query <- function(species = NULL, year = NULL, month = NULL, var_time = "OBSERVATION_DATE", var_species = "COMMON_NAME") {
  if (!is.null(month)) {
    time_grd <- expand.grid(year, sprintf("%02d", month)) |> setNames(c("year", "month"))
    time_q <- paste(time_grd$year, time_grd$month, "%", sep = "-")
  } else {
    time_q <- paste(year, "%", sep = "-")
  }

  query <- glue::glue_collapse(
    glue::glue("{var_time} LIKE {paste0(\"'\", time_q, \"'\")}"),
    sep = " OR "
  )

  if (!is.null(species)) {
		species <- paste0("'", species, "'", collapse = ", ")
    query <- glue::glue("({query}) AND {var_species} IN ({ species })")
  }

  return(query)
}
