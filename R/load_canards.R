
#' Load and process the Canards de Mer canards
#'
#' This function loads and processes the "Canards de Mer" canards from a predefined external source.
#' It standardizes column names, performs transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_canards <- load_canards()
#' }
#' @export
load_canards <- function() {

    cli::cli_h2("Canards de mer")
    cli::cli_alert_info("Starting integration procedure on { external_files()$canards_de_mer$path }")

    # assert file exists
    if(!file.exists(external_files()$canards_de_mer$path)) {
        cli::cli_abort("Could not find file { external_files()$canards_de_mer$path }")
    }

    # Read file
    canards <- read.csv2(external_files()$canards_de_mer$path) |> tibble::as_tibble()

    # assert columns exist
    missing_cols <- setdiff(external_files()$canards_de_mer$check_columns, names(canards))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }
    
    cli::cli_alert_info("Applying transformation on { nrow(canards) } rows")

    canards <- canards |>
        dplyr::mutate(date_obs = lubridate::make_date(Annee, Mois, Jour)) |>
        dplyr::select(
            latitude = "LATITUDE",
            longitude = "LONGITUDE",
            locality = "NomLieu",
            date = "date_obs",
            abondance = "NombreTotal",
            obs = "NomObservateur",
            inv_type = "Signification.1",
            nom_fr = "Nom_FR"
        ) |>
        dplyr::mutate(
            latitude = as.numeric(latitude),
            longitude = as.numeric(longitude),
            nom_fr = stringi::stri_trans_general(gsub("sp", "sp\\.", nom_fr, ignore.case = TRUE), "latin-ascii") |> 
                tolower(),
            source = "Canards de mer",
            colony = FALSE,
            link = external_files()$canards_de_mer$path
        )

    # Join TAXO - Match code_id using nom_fr
    canards <- canards |>
        dplyr::left_join(
            dplyr::select(get_species_codes(), code_id, nom_fr) |> dplyr::distinct(),
            by = "nom_fr",
            na_matches = "never"
        ) |>
        dplyr::mutate(
            code_id = ifelse(
                nom_fr %in% names(equivalences),
                equivalences[nom_fr],
                code_id
            )
        )

    # Re-order cols
    canards <- dplyr::select(canards, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning { nrow(canards) } rows")

    return(canards)
    
}
