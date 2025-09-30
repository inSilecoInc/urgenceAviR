#' Load and process the Atlantic Colonies dataset
#'
#' This function loads and processes the "Atlantic Colonies" dataset from a predefined spatial data source.
#' It validates the file, applies transformations, and integrates coordinates with census data.
#'
#' @return A processed `data.frame` with standardized columns and integrated spatial data.
#' @examples
#' \dontrun{
#' processed_colonies <- load_atlantic_colonies()
#' }
#' @export
load_atlantic_colonies <- function() {

    cli::cli_h2("Atlantic Colonies")
    cli::cli_alert_info("Starting integration procedure on {external_files$atlantic_colonies$path}")

    # Assert file exists
    if (!file.exists(external_files$atlantic_colonies$path)) {
        cli::cli_abort("Could not find file: {external_files$atlantic_colonies$path}")
    }

    # Read layers
    atlantic_colonies_coords <- sf::st_read(dsn = external_files$atlantic_colonies$path, layer = "Colonies", quiet = TRUE)
    atlantic_colonies_census <- sf::st_read(dsn = external_files$atlantic_colonies$path, layer = "Censuses", quiet = TRUE)

    # Process coordinates
    atlantic_colonies_coords <- atlantic_colonies_coords |>
        dplyr::select(latitude = Latdec, longitude = Londec, ColonyId = ColonyID)

    # Merge coordinates with census data
    atlantic_colonies_census <- atlantic_colonies_census |>
        dplyr::left_join(atlantic_colonies_coords, by = "ColonyId") |>
        dplyr::select(
            Date, latitude, longitude, Platform, Colony_size, Data_location, Species_code,
            Observer1, Observer2, source = Source, ColonyId
        )

    # Final transformations and renaming
    atlantic_colonies <- atlantic_colonies_census |>
        dplyr::rename(
            locality = ColonyId,
            code_id = Species_code,
            abondance = Colony_size,
            inv_type = Platform,
            date = Date
        ) |>
        dplyr::mutate(
            locality = as.character(locality),
            link = external_files$atlantic_colonies$path,
            colony = TRUE
        ) |>
        tidyr::unite("obs", Observer1, Observer2, sep = ", ", na.rm=TRUE)
    
    # Re-order cols
    atlantic_colonies <- dplyr::select(atlantic_colonies, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(atlantic_colonies)} rows")

    return(atlantic_colonies)
}
