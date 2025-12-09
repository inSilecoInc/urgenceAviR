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
    biomq <- readxl::read_excel(path = external_files()$biomq$path, sheet = 2) |> tibble::as_tibble()

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
        dplyr::rename(
            locality = NomCol,
            longitude = CentroideX,
            latitude = CentroideY,
            abondance = nb_nicheur,
            inv_type = methode,
            obs = nomRef,
            year = AnneeDebut,
            month = MoisDebut,
            day = JourDebut
        ) |>
        dplyr::mutate(
            date = lubridate::make_date(year, month, day),
            source = "BIOMQ",
            nom_fr = tolower(NomFR),
            link = external_files()$biomq$path,
            colony = TRUE
        )

    # Enforce sp. 
    biomq$code_id<-taxo$Species_ID[match(biomq$NomFR,taxo$French_Name)]
    
    biomq <- biomq |>
      dplyr::mutate(
        code_id = ifelse(
          NomFR %in% names(equivalences),
          equivalences[NomFR],
          code_id
        )
      )
    
    # Drop non-relevant coordinates
    biomq <- biomq |>
        dplyr::filter(
            longitude >= -100 & longitude <= -30,
            latitude >= 30 & latitude <= 70
        )

    # Re-order cols
    biomq <- dplyr::select(biomq, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(biomq)} rows")
    return(biomq)
}
