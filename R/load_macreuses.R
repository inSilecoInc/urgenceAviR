#' Load and process the Macreuse dataset
#'
#' This function loads and processes the "Macreuse" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_macreuse <- load_macreuse()
#' }
#' @export
load_macreuse <- function() {

    cli::cli_h2("Macreuse")
    cli::cli_alert_info("Starting integration procedure on {external_files()$macreuse$path}")

    # Assert file exists
    if (!file.exists(external_files()$macreuse$path)) {
        cli::cli_abort("Could not find file: {external_files()$macreuse$path}")
    }

    macreuse <- read.csv2(external_files()$macreuse$path) |> tibble::as_tibble()

    # Assert columns exist
    missing_cols <- setdiff(external_files()$macreuse$check_columns, names(macreuse))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    cli::cli_alert_info("Applying transformation on {nrow(macreuse)} rows")

    macreuse <- macreuse |>
        dplyr::select(Date, Observateur, Espece, Nombre, Longitude, Latitude) |>
        dplyr::rename(
            longitude = Longitude,
            latitude = Latitude,
            abondance = Nombre,
            date = Date,
            obs = Observateur,
            code_sp = Espece
        ) |>
        dplyr::mutate(
            date = lubridate::ymd(date),
            latitude = as.numeric(latitude),
            longitude = as.numeric(longitude),
            link = external_files()$macreuse$path,
            source = "Macreuse",
            inv_type = NA,  # Quel type d'inventaire
            locality = NA,
            colony = FALSE
        ) 

    # Join TAXO - Match CODE_ID using Code4_FR
    macreuse <- macreuse |>
        dplyr::left_join(
            dplyr::select(
                    get_species_codes(), 
                    code_id, 
                    code4_fr
                ) |>
                dplyr::distinct(),
            by = c("code_sp" = "code4_fr"),
            na_matches = "never"
        ) |>
        dplyr::mutate(
            code_id = ifelse(
                code_sp %in% names(equivalences_garrots),
                equivalences_garrots[code_sp],
                code_id
            )
        )
    
    # Re-order cols
    macreuse <- dplyr::select(macreuse, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(macreuse)} rows")

    return(macreuse)
}
