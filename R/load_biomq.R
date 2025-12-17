#' Load and process the BIOMQ dataset
#'
#' This function loads and processes the "BIOMQ" dataset from a predefined external Excel file.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_biomq <- load_biomq()
#' }
#' @export
load_biomq <- function() {

    cli::cli_h2("BIOMQ")
    cli::cli_alert_info("Starting integration procedure on {external_files()$biomq$path}")

    # Assert file exists
    if (!file.exists(external_files()$biomq$path)) {
        cli::cli_abort("Could not find file: {external_files()$biomq$path}")
    }

    # Load data from the second sheet
    biomq <- readxl::read_excel(path = external_files()$biomq$path, sheet = 1) |> tibble::as_tibble()

    # Assert columns exist
    missing_cols <- setdiff(external_files()$biomq$check_columns, names(biomq))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    cli::cli_alert_info("Applying transformation on {nrow(biomq)} rows")

    # Select and rename columns
    biomq <- biomq |>
        dplyr::select(
            NomCol, CentroideX, CentroideY, NomFR,
            nb_nicheur, methode, nomRef, "Ann\u00e9e",
            MoisDebut, JourDebut
        ) |>
        dplyr::rename(
            locality = NomCol,
            longitude = CentroideX,
            latitude = CentroideY,
            abondance = nb_nicheur,
            inv_type = methode,
            obs = nomRef,
            year = "Ann\u00e9e",
            month = MoisDebut,
            day = JourDebut
        ) |>
        dplyr::mutate(
            date = lubridate::make_date(year, month, day),
            source = "BIOMQ",
            code_fr = stringi::stri_trans_general(tolower(NomFR), "latin-ascii")
        )

    biomq <- biomq |>
        dplyr::mutate(
            code_fr = stringr::str_replace_all(
              code_fr, 
                c(
                    "goelands" = "goeland sp.",
                    "sternes" = "sterne sp.",
                    "cormorans" = "cormoran sp."
                ))
        )

    # Join TAXO - Match CODE_ID using code_fr
    biomq <- biomq |>
        dplyr::left_join(
            .pkg_env$taxonomy,
            by = "code_fr",
            na_matches = "never"
        ) |>
        dplyr::mutate(
            code_id = ifelse(
                code_fr %in% names(.pkg_env$equivalences),
                .pkg_env$equivalences[code_fr],
                code_id
            )
        )
    
    # Drop non-relevant coordinates
    biomq <- biomq |>
        dplyr::filter(
            longitude >= -100 & longitude <= -30,
            latitude >= 30 & latitude <= 70
        )

    
    biomq <- dplyr::select(biomq, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(biomq)} rows")
    return(biomq)
}
