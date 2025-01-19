#' Load and process the SOMEC dataset
#'
#' This function loads and processes the "SOMEC" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_somec <- load_somec()
#' }
#' @export
load_somec <- function() {

    cli::cli_h1("SOMEC")
    cli::cli_alert_info("Starting integration procedure on {external_files$somec$path}")

    # Assert file exists
    if (!file.exists(external_files$somec$path)) {
        cli::cli_abort("Could not find file: {external_files$somec$path}")
    }

    somec <- read.csv(external_files$somec$path) |> tibble::as_tibble()

    # Assert columns exist
    missing_cols <- setdiff(external_files$somec$check_columns, names(somec))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    cli::cli_alert_info("Applying transformation on {nrow(somec)} rows")

    somec <- somec |>
        dplyr::rename(
            latitude = LatStart,
            longitude = LongStart,
            species_id = Latin,
            date = Date,
            abondance = Count,
            obs = ObserverName
        ) |>
        dplyr::select(latitude, longitude, date, abondance, species_id, obs, Alpha) |>
        dplyr::mutate(
            latitude = as.numeric(latitude),
            longitude = as.numeric(longitude),
            source = "somec",
            link = external_files$somec$path,
            inv_type = NA,
            locality = NA,
            colony = FALSE
        )

    # Join TAXO - Match CODE_ID using Alpha_Code
    somec <- somec |>
        dplyr::left_join(
            dplyr::select(
                get_species_codes(),
                code_id,
                alpha_code
            ) |> dplyr::distinct(),
            by = c("Alpha" = "alpha_code")
        ) |>
        dplyr::mutate(
            code_id = ifelse(
                Alpha %in% names(alpha_to_species_id),
                alpha_to_species_id[Alpha],
                code_id
            )
        ) |>
        dplyr::select(-Alpha)


    cli::cli_alert_success("Returning {nrow(somec)} rows")

    return(somec)
}
