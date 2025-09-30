#' Load and process the Sauvagine Fleuve dataset
#'
#' This function loads and processes the "Sauvagine Fleuve" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_sauvagine_fleuve <- load_sauvagine_fleuve()
#' }
#' @export
load_sauvagine_fleuve <- function() {

    cli::cli_h2("Sauvagine Fleuve")
    cli::cli_alert_info("Starting integration procedure on {external_files()$sauvagine_fleuve$path}")

    # Assert file exists
    if (!file.exists(external_files()$sauvagine_fleuve$path)) {
        cli::cli_abort("Could not find file: {external_files()$sauvagine_fleuve$path}")
    }

    sauvagine_fleuve <- read.csv2(external_files()$sauvagine_fleuve$path) |> tibble::as_tibble()

    # Assert columns exist
    missing_cols <- setdiff(external_files()$sauvagine_fleuve$check_columns, names(sauvagine_fleuve))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    cli::cli_alert_info("Applying transformation on {nrow(sauvagine_fleuve)} rows")

    sauvagine_fleuve <- sauvagine_fleuve |>
        dplyr::rename(
            date = Date,
            latitude = Latitude,
            longitude = Longitude,
            abondance = Nombre,
            obs = Observateur
        ) |>
        dplyr::mutate(
            date = lubridate::ymd(date),
            latitude = as.numeric(latitude),
            longitude = as.numeric(longitude),
            inv_type = NA,  # Est-ce qu'il y a un type d'inventaire ?
            link = external_files()$sauvagine_fleuve$path,
            source = "Sauvagine Fleuve",
            locality = NA,
            colony = FALSE
        ) 

    # Join TAXO 
    sauvagine_fleuve$code_id<-NA
    sauvagine_fleuve <- sauvagine_fleuve |>
      dplyr::mutate(
        code_id = ifelse(
          Espece %in% names(equivalences),
          equivalences[Espece],
          code_id
        )
      )
    
    # Re-order cols
    sauvagine_fleuve <- dplyr::select(sauvagine_fleuve, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(sauvagine_fleuve)} rows")

    return(sauvagine_fleuve)
}
