#' Load and process the Garrot dataset
#'
#' This function loads and processes the "Garrot" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and standardizes the data structure.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_garrot <- load_garrot()
#' }
#' @export
load_garrot <- function() {

    cli::cli_h2("Garrot")
    cli::cli_alert_info("Starting integration procedure on {external_files()$garrot$path}")

    # Assert file exists
    if (!file.exists(external_files()$garrot$path)) {
        cli::cli_abort("Could not find file: {external_files()$garrot$path}")
    }

    garrot <- data.table::fread(external_files()$garrot$path, dec = ",", sep = ";") |> tibble::as_tibble()

    # Assert columns exist
    missing_cols <- setdiff(external_files()$garrot$check_columns, names(garrot))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    cli::cli_alert_info("Applying transformation on {nrow(garrot)} rows")

    garrot <- garrot |>
        dplyr::select(annee, no_seance, mois, jour, CodeSp, N, Observateurs, Lat, Long, loc_ID, CodeSp) |>
        dplyr::mutate(
            latitude = as.numeric(Lat),
            longitude = as.numeric(Long),
            date = lubridate::make_date(annee, mois, jour),
            locality = NA,
            source = "Garrot",
            link = external_files()$garrot$path
        ) |>
        dplyr::select(!c(annee, mois, jour)) |>
        dplyr::rename(
            abondance = N,
            obs = Observateurs,
            inv_type = no_seance,
            code_sp = CodeSp,
        )

    # Join TAXO - Match code_id using nom_fr
    garrot <- garrot |>
        dplyr::mutate(
            code_id = ifelse(
                code_sp %in% names(equivalences),
                equivalences[code_sp],
                code_sp
            )
        ) |> dplyr::left_join(taxonomy, by = "code_id")
    
    # Re-order cols
    garrot <- dplyr::select(garrot, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(garrot)} rows")

    return(garrot)
}
